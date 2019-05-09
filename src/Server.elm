port module Server exposing (main)

import Browser exposing (Document)
import Chat exposing (Address, Channel, ChannelName, Channels, Message, User)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Keyed as HtmlKeyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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
    , users : Dict Address User
    , channels : Channels
    , logs : List String
    }


type Msg
    = SetAddress String
    | SetConnectionCount Int
    | ClientSeen Address
    | AddMessage { user : Address, channelName : ChannelName, content : String }
    | CommitMessage { user : Address, channelName : ChannelName, content : String, time : Posix }
    | ClientMessageError String



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
            AddMessage { user = address, channelName = channelName, content = content }
        )
        (Decode.field "address" Decode.string)
        (Decode.field "message" Chat.decodeUserMessage)



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



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAddress address ->
            ( { model | address = Just address }, Cmd.none )

        SetConnectionCount count ->
            ( { model | connections = count }, Cmd.none )

        ClientSeen address ->
            let
                ( updatedChannels, updateUsersCmd ) =
                    userJoinChannel model.channels address "general"
            in
            ( { model
                | users = createUser model.users address
                , channels = updatedChannels
              }
            , updateUsersCmd
            )

        AddMessage { user, channelName, content } ->
            ( model
            , Task.perform
                (\time ->
                    CommitMessage { user = user, channelName = channelName, content = content, time = time }
                )
                Time.now
            )

        CommitMessage { user, channelName, content, time } ->
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


createUser : Dict Address User -> Address -> Dict Address User
createUser users address =
    Dict.update
        address
        (\maybeUser ->
            case maybeUser of
                Just user ->
                    Just user

                Nothing ->
                    Just { name = address }
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
        (\_ -> AddMessage { user = "admin", channelName = channelName, content = content })
        (Task.succeed ())



---- VIEW ----


view : Model -> Document Msg
view { address, connections, users, channels, logs } =
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
                                                , Html.li [] [ Html.text <| user ++ ": " ++ content ]
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
                , Html.text "Logs:"
                , Html.ul
                    []
                  <|
                    List.map (\log -> Html.li [] [ Html.text log ]) logs
                ]
        ]
    }
