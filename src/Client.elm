port module Client exposing (main)

import Browser exposing (Document)
import Chat exposing (Address, Channel, ChannelName, User)
import Debug
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed as HtmlKeyed
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Set exposing (Set)
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type Model
    = Disconnected Address
    | Connected AuthState
    | Authenticated AuthenticatedData


type AuthState
    = Login { email : String, password : String }
    | SignUp { email : String, password : String, passwordVerify : String }


type alias AuthenticatedData =
    { channels : Dict ChannelName Channel
    , activeChannel : String
    , currentMessage : String
    }


type Msg
    = SetServerAddress String
    | ConnectToServer
    | ConnectedToServer
    | UpdateChannelStatus Value
    | ServerMessageError String
    | AddMessageFromServer Value
    | SendMessage
    | SetCurrentMessage String
    | SwitchToSignUp
    | SwitchToLogin
    | SetAuthEmail String
    | SetAuthPassword String
    | SetAuthPasswordVerify String
    | DoAuthenticate
    | UserAuthenticated



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( Disconnected ""
    , Cmd.none
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ serverMessage handleServerMessage
        , connectedToServer (\_ -> ConnectedToServer)
        ]



---- PORTS ----
--INCOMING


port serverMessage : (Value -> msg) -> Sub msg


handleServerMessage : Value -> Msg
handleServerMessage message =
    case Decode.decodeValue decodeServerMessage message of
        Ok msg ->
            msg

        Err err ->
            ServerMessageError <| Decode.errorToString err


decodeServerMessage : Decoder Msg
decodeServerMessage =
    Decode.field "event" Decode.string
        |> Decode.andThen decodeServerMessageHelper


decodeServerMessageHelper : String -> Decoder Msg
decodeServerMessageHelper event =
    case event of
        "updateChannelStatus" ->
            Decode.map UpdateChannelStatus
                (Decode.field "data" Decode.value)

        "message" ->
            Decode.map AddMessageFromServer
                (Decode.field "data" Decode.value)

        "authenticated" ->
            Decode.succeed UserAuthenticated

        _ ->
            Decode.fail <| "Unknown server event: " ++ event


port connectedToServer : (() -> msg) -> Sub msg



--OUTGOING


port connectToServer : Address -> Cmd msg


port sendMessage : Value -> Cmd msg


port sendAuth : Value -> Cmd msg


sendLogin : { email : String, password : String } -> Cmd msg
sendLogin { email, password } =
    Encode.object
        [ ( "event", Encode.string "authLogin" )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]
        |> sendAuth


sendSignUp : { email : String, password : String, passwordVerify : String } -> Cmd msg
sendSignUp { email, password, passwordVerify } =
    Encode.object
        [ ( "event", Encode.string "authSignUp" )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        , ( "passwordVerify", Encode.string passwordVerify )
        ]
        |> sendAuth



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SetServerAddress address, Disconnected _ ) ->
            ( Disconnected address, Cmd.none )

        ( ConnectToServer, Disconnected serverAddress ) ->
            ( model, connectToServer serverAddress )

        ( ConnectedToServer, Disconnected serverAddress ) ->
            ( Connected <| Login { email = "", password = "" }
            , Cmd.none
            )

        ( SwitchToSignUp, Connected (Login { email }) ) ->
            ( Connected <| SignUp { email = email, password = "", passwordVerify = "" }, Cmd.none )

        ( SwitchToLogin, Connected (SignUp { email }) ) ->
            ( Connected <| Login { email = email, password = "" }, Cmd.none )

        ( UpdateChannelStatus data, Authenticated authData ) ->
            ( Authenticated { authData | channels = Chat.updateChannelStatus authData.channels data }
            , Cmd.none
            )

        ( AddMessageFromServer data, Authenticated authData ) ->
            ( Authenticated { authData | channels = Chat.addMessageToChannel authData.channels data }
            , Cmd.none
            )

        ( SetCurrentMessage message, Authenticated authData ) ->
            ( Authenticated { authData | currentMessage = message }
            , Cmd.none
            )

        ( SendMessage, Authenticated ({ currentMessage, activeChannel } as authData) ) ->
            ( Authenticated { authData | currentMessage = "" }
            , sendMessage <| Chat.encodeUserMessage activeChannel currentMessage
            )

        ( SetAuthEmail email, Connected (Login { password }) ) ->
            ( Connected <| Login { email = email, password = password }, Cmd.none )

        ( SetAuthPassword password, Connected (Login { email }) ) ->
            ( Connected <| Login { email = email, password = password }, Cmd.none )

        ( SetAuthEmail email, Connected (SignUp { password, passwordVerify }) ) ->
            ( Connected <| SignUp { email = email, password = password, passwordVerify = passwordVerify }, Cmd.none )

        ( SetAuthPassword password, Connected (SignUp { email, passwordVerify }) ) ->
            ( Connected <| SignUp { email = email, password = password, passwordVerify = passwordVerify }, Cmd.none )

        ( SetAuthPasswordVerify passwordVerify, Connected (SignUp { email, password }) ) ->
            ( Connected <| SignUp { email = email, password = password, passwordVerify = passwordVerify }, Cmd.none )

        ( DoAuthenticate, Connected authState ) ->
            case authState of
                Login data ->
                    ( model, sendLogin data )

                SignUp data ->
                    ( model, sendSignUp data )

        ( UserAuthenticated, Connected authState ) ->
            ( Authenticated
                { channels = Dict.empty
                , activeChannel = "general"
                , currentMessage = ""
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "De-Slack - Client"
    , body =
        [ Html.toUnstyled <|
            case model of
                Disconnected serverAddress ->
                    Html.form
                        [ Events.onSubmit ConnectToServer ]
                        [ Html.input
                            [ Attrs.value serverAddress
                            , Attrs.placeholder "Server Address"
                            , Attrs.autofocus True
                            , Events.onInput SetServerAddress
                            ]
                            []
                        ]

                Connected authState ->
                    case authState of
                        Login { email, password } ->
                            Html.form
                                [ Events.onSubmit DoAuthenticate ]
                                [ Html.text "login"
                                , Html.input
                                    [ Attrs.value email
                                    , Attrs.placeholder "email"
                                    , Attrs.autofocus True
                                    , Events.onInput SetAuthEmail
                                    ]
                                    []
                                , Html.input
                                    [ Attrs.value password
                                    , Attrs.placeholder "password"
                                    , Attrs.type_ "password"
                                    , Events.onInput SetAuthPassword
                                    ]
                                    []
                                , Html.button
                                    []
                                    [ Html.text "login" ]
                                , Html.button
                                    [ Attrs.type_ "button"
                                    , Events.onClick SwitchToSignUp
                                    ]
                                    [ Html.text "signup" ]
                                ]

                        SignUp { email, password, passwordVerify } ->
                            Html.form
                                [ Events.onSubmit DoAuthenticate ]
                                [ Html.text "signup"
                                , Html.input
                                    [ Attrs.value email
                                    , Attrs.placeholder "email"
                                    , Attrs.autofocus True
                                    , Events.onInput SetAuthEmail
                                    ]
                                    []
                                , Html.input
                                    [ Attrs.value password
                                    , Attrs.placeholder "password"
                                    , Attrs.type_ "password"
                                    , Events.onInput SetAuthPassword
                                    ]
                                    []
                                , Html.input
                                    [ Attrs.value passwordVerify
                                    , Attrs.placeholder "verify password"
                                    , Attrs.type_ "password"
                                    , Events.onInput SetAuthPasswordVerify
                                    ]
                                    []
                                , Html.button
                                    []
                                    [ Html.text "signup" ]
                                , Html.button
                                    [ Attrs.type_ "button"
                                    , Events.onClick SwitchToLogin
                                    ]
                                    [ Html.text "login" ]
                                ]

                Authenticated { channels, currentMessage } ->
                    Html.div
                        []
                        [ Html.text "Channels:"
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
                                                (messages
                                                    |> List.reverse
                                                    |> List.map
                                                        (\{ user, content, time } ->
                                                            ( time |> Time.posixToMillis |> String.fromInt
                                                            , Html.li
                                                                []
                                                                [ Html.text <|
                                                                    user.email
                                                                        ++ ": "
                                                                        ++ content
                                                                ]
                                                            )
                                                        )
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
                        , Html.form
                            [ Events.onSubmit SendMessage ]
                            [ Html.input
                                [ Attrs.value currentMessage
                                , Attrs.placeholder "Message for Channel"
                                , Attrs.autofocus True
                                , Events.onInput SetCurrentMessage
                                ]
                                []
                            ]
                        ]
        ]
    }
