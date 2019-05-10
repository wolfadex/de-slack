port module Server exposing (main)

import Browser exposing (Document)
import Chat exposing (Address, Channel, ChannelName, Channels, Message, User)
import Crypto.Hash exposing (sha512)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed as HtmlKeyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Set exposing (Set)
import Task
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type alias Model =
    { address : Maybe String
    , connections : Int
    , users : Dict Email User
    , channels : Channels
    , logs : List String
    , clients : Dict Address Client
    }


type alias Email =
    String


type Client
    = Unauthenticated
    | SignedUp User
    | Authenticated User


type Msg
    = SetAddress String
    | SetConnectionCount Int
    | ClientSeen Address
    | AddMessage { address : Address, channelName : ChannelName, content : String }
    | CommitMessage { address : Address, user : User, channelName : ChannelName, content : String, time : Posix }
    | ClientMessageError String
    | AuthenticationError
    | AuthenticateLogin { address : Address, email : String, password : String }
    | AuthenticateSignUp { address : Address, email : String, password : String }
    | ApproveUserSignUp Address Email
    | DelayedUserJoinChannel Address User



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( { address = Nothing
      , connections = 0
      , users = Dict.empty
      , channels =
            Dict.singleton
                "general"
                { name = "general"
                , messages = []
                , activeUsers = Set.empty
                }
      , logs = []
      , clients = Dict.empty
      }
    , Cmd.none
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setAddress SetAddress
        , connectionCountChange SetConnectionCount
        , clientSeen ClientSeen
        , clientMessage handleClientMessage
        , clientAuthenticate handleClientAuth
        ]



---- PORTS ----
--INCOMING


port setAddress : (String -> msg) -> Sub msg


port connectionCountChange : (Int -> msg) -> Sub msg


port clientSeen : (String -> msg) -> Sub msg


port clientMessage : (Value -> msg) -> Sub msg


handleClientMessage : Value -> Msg
handleClientMessage value =
    case Decode.decodeValue decodeClientMessage value of
        Ok msg ->
            msg

        Err err ->
            ClientMessageError <| Decode.errorToString err


decodeClientMessage : Decoder Msg
decodeClientMessage =
    Decode.map2
        (\address ( channelName, content ) ->
            AddMessage { address = address, channelName = channelName, content = content }
        )
        (Decode.field "address" decodeAddress)
        (Decode.field "message" Chat.decodeUserMessage)


decodeAddress : Decoder Address
decodeAddress =
    Decode.string


port clientAuthenticate : (Value -> msg) -> Sub msg


handleClientAuth : Value -> Msg
handleClientAuth value =
    case Decode.decodeValue decodeClientAuth value of
        Ok msg ->
            msg

        Err err ->
            AuthenticationError


decodeClientAuth : Decoder Msg
decodeClientAuth =
    Decode.field "address" Decode.string
        |> Decode.andThen decodeClientAuthBody


decodeClientAuthBody : Address -> Decoder Msg
decodeClientAuthBody address =
    Decode.field "body" (decodeClientAuthBodyHelper address)


decodeClientAuthBodyHelper : Address -> Decoder Msg
decodeClientAuthBodyHelper address =
    Decode.field "event" Decode.string
        |> Decode.andThen (decodeClientAuthHelper address)


decodeClientAuthHelper : Address -> String -> Decoder Msg
decodeClientAuthHelper address event =
    case event of
        "authLogin" ->
            decodeLogin address

        "authSignUp" ->
            decodeSignUp address

        _ ->
            Decode.fail <| "Unknown authentication event: " ++ event


decodeLogin : Address -> Decoder Msg
decodeLogin address =
    Decode.map2
        (\email password ->
            AuthenticateLogin { address = address, email = email, password = password }
        )
        (Decode.field "email" Decode.string)
        (Decode.field "password" Decode.string)


decodeSignUp : Address -> Decoder Msg
decodeSignUp address =
    Decode.map3
        (\email password passwordVerify ->
            if password == passwordVerify then
                AuthenticateSignUp { address = address, email = email, password = password }

            else
                AuthenticationError
        )
        (Decode.field "email" Decode.string)
        (Decode.field "password" Decode.string)
        (Decode.field "passwordVerify" Decode.string)



--OUTGOING


port sendMessageToAddress : Value -> Cmd msg


updateUsersAboutChannelStatus : Channel -> Cmd Msg
updateUsersAboutChannelStatus ({ activeUsers } as channel) =
    let
        message =
            Encode.object
                [ ( "event", Encode.string "updateChannelStatus" )
                , ( "data", Chat.encodeChannelStatus channel )
                ]
    in
    activeUsers
        |> Set.toList
        |> List.map
            (\address ->
                sendMessageToAddress <|
                    Encode.object
                        [ ( "address", Encode.string address )
                        , ( "message", message )
                        ]
            )
        |> Cmd.batch


broadcastMessage : Channel -> Message -> Cmd Msg
broadcastMessage { name, activeUsers } message =
    let
        encodedMessage =
            Encode.object
                [ ( "event", Encode.string "message" )
                , ( "data", Chat.encodeMessage name message )
                ]
    in
    activeUsers
        |> Set.toList
        |> List.map
            (\address ->
                sendMessageToAddress <|
                    Encode.object
                        [ ( "address", Encode.string address )
                        , ( "message", encodedMessage )
                        ]
            )
        |> Cmd.batch


notifyUserAuthenticated : Address -> Cmd msg
notifyUserAuthenticated address =
    Encode.object
        [ ( "address", Encode.string address )
        , ( "message"
          , Encode.object
                [ ( "event", Encode.string "authenticated" ) ]
          )
        ]
        |> sendMessageToAddress



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAddress address ->
            ( { model | address = Just address }, Cmd.none )

        SetConnectionCount count ->
            ( { model | connections = count }, Cmd.none )

        ClientSeen address ->
            ( { model | clients = Dict.insert address Unauthenticated model.clients }
            , Cmd.none
            )

        AuthenticationError ->
            ( model, Cmd.none )

        AuthenticateLogin { address, email, password } ->
            ( model, Cmd.none )

        AuthenticateSignUp { address, email, password } ->
            case Dict.get email model.users of
                Nothing ->
                    case Dict.get address model.clients of
                        Nothing ->
                            ( model, Cmd.none )

                        Just clientState ->
                            case clientState of
                                Unauthenticated ->
                                    ( { model
                                        | clients =
                                            Dict.insert
                                                address
                                                (SignedUp { name = address, email = email, password = sha512 password })
                                                model.clients
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                Just _ ->
                    ( model, Cmd.none )

        ApproveUserSignUp address email ->
            case Dict.get address model.clients of
                Nothing ->
                    ( model, Cmd.none )

                Just client ->
                    case client of
                        SignedUp user ->
                            ( { model
                                | clients = Dict.insert address (Authenticated user) model.clients
                              }
                            , Cmd.batch
                                [ notifyUserAuthenticated address
                                , Task.perform (\_ -> DelayedUserJoinChannel address user) (Process.sleep 0)
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

        DelayedUserJoinChannel address user ->
            let
                ( updatedChannels, updateUsersCmd ) =
                    userJoinChannel model.channels address "general"
            in
            ( { model
                | users = createUser model.users user
                , channels = updatedChannels
              }
            , updateUsersCmd
            )

        AddMessage { address, channelName, content } ->
            case Dict.get address model.clients of
                Just (Authenticated user) ->
                    ( model
                    , Task.perform
                        (\time ->
                            CommitMessage { address = address, user = user, channelName = channelName, content = content, time = time }
                        )
                        Time.now
                    )

                Nothing ->
                    if address == "automated" then
                        ( model
                        , Task.perform
                            (\time ->
                                CommitMessage { address = address, user = { name = "", email = "automated", password = "" }, channelName = channelName, content = content, time = time }
                            )
                            Time.now
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CommitMessage { address, user, channelName, content, time } ->
            let
                message =
                    { user = user, content = content, time = time }
            in
            ( { model
                | channels =
                    model.channels
                        |> Dict.update
                            channelName
                            (\maybeChannel ->
                                case maybeChannel of
                                    Nothing ->
                                        Nothing

                                    Just channel ->
                                        Just { channel | messages = message :: channel.messages }
                            )
              }
            , case Dict.get channelName model.channels of
                Nothing ->
                    Cmd.none

                Just channel ->
                    broadcastMessage channel message
            )

        ClientMessageError error ->
            ( { model | logs = error :: model.logs }, Cmd.none )


createUser : Dict Email User -> User -> Dict Email User
createUser users user =
    Dict.update
        user.email
        (\maybeUser ->
            case maybeUser of
                Just u ->
                    Just u

                Nothing ->
                    Just user
        )
        users


userJoinChannel : Dict ChannelName Channel -> Address -> ChannelName -> ( Dict ChannelName Channel, Cmd Msg )
userJoinChannel channels user channelToJoin =
    let
        updatedChannels =
            Dict.update
                channelToJoin
                (\maybeChannel ->
                    case maybeChannel of
                        Nothing ->
                            Nothing

                        Just channel ->
                            Just { channel | activeUsers = Set.insert user channel.activeUsers }
                )
                channels
    in
    ( updatedChannels
    , case Dict.get channelToJoin updatedChannels of
        Nothing ->
            Cmd.none

        Just channel ->
            Cmd.batch
                [ updateUsersAboutChannelStatus channel
                , adminMessage ("User " ++ user ++ " joined the channel") "general"
                ]
    )


adminMessage : String -> ChannelName -> Cmd Msg
adminMessage content channelName =
    Task.perform
        (\_ ->
            AddMessage
                { address = "automated"
                , channelName = channelName
                , content = content
                }
        )
        (Task.succeed ())



---- VIEW ----


view : Model -> Document Msg
view { address, connections, users, channels, logs, clients } =
    { title = "De-Slack - Server"
    , body =
        [ Html.toUnstyled <|
            Html.div
                []
                [ Html.text <|
                    "Address: "
                        ++ Maybe.withDefault "loading..." address
                , Html.br [] []
                , Html.text <|
                    "Conenctions: "
                        ++ String.fromInt connections
                , Html.br [] []
                , Html.text "Users:"
                , HtmlKeyed.ul
                    []
                    (users
                        |> Dict.toList
                        |> List.map
                            (\( userAddress, { name } ) ->
                                ( userAddress
                                , Html.li
                                    []
                                    [ Html.text <| "Address: " ++ userAddress ++ ", Name: " ++ name ]
                                )
                            )
                    )
                , Html.text "Channels:"
                , HtmlKeyed.ul
                    []
                    (channels
                        |> Dict.toList
                        |> List.map
                            (\( channelName, { messages, activeUsers } ) ->
                                ( channelName
                                , Html.li
                                    []
                                    [ Html.text <| "Name: " ++ channelName
                                    , Html.br [] []
                                    , Html.text "Messages:"
                                    , HtmlKeyed.ul
                                        []
                                        (List.map
                                            (\{ user, content, time } ->
                                                ( time |> Time.posixToMillis |> String.fromInt
                                                , Html.li [] [ Html.text <| user.email ++ ": " ++ content ]
                                                )
                                            )
                                            messages
                                        )
                                    , Html.text "Active Users:"
                                    , Html.ul
                                        []
                                        (activeUsers
                                            |> Set.toList
                                            |> List.map
                                                (\clientAddress ->
                                                    Html.li
                                                        []
                                                        [ Html.text clientAddress ]
                                                )
                                        )
                                    ]
                                )
                            )
                    )
                , Html.text "Clients:"
                , HtmlKeyed.ul
                    []
                    (clients
                        |> Dict.toList
                        |> List.map
                            (\( clientAddress, clientState ) ->
                                ( clientAddress
                                , Html.li
                                    []
                                    [ case clientState of
                                        Unauthenticated ->
                                            Html.text <| "Unauthenticated: " ++ clientAddress

                                        SignedUp { email } ->
                                            Html.button
                                                [ Attrs.type_ "button"
                                                , Events.onClick <| ApproveUserSignUp clientAddress email
                                                ]
                                                [ Html.text <| "Approve user at email: " ++ email ]

                                        Authenticated { email } ->
                                            Html.text <| "Authenticated user email: " ++ email
                                    ]
                                )
                            )
                    )
                , Html.text "Logs:"
                , Html.ul
                    []
                  <|
                    List.map (\log -> Html.li [] [ Html.text log ]) logs
                ]
        ]
    }
