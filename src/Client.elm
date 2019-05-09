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
    | Connected ConnectedData


type alias ConnectedData =
    { serverAddress : Address
    , channels : Dict ChannelName Channel
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

        _ ->
            Decode.fail <| "Unknown server event: " ++ event


port connectedToServer : (() -> msg) -> Sub msg



--OUTGOING


port connectToServer : Address -> Cmd msg


port sendMessage : Value -> Cmd msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SetServerAddress address, Disconnected _ ) ->
            ( Disconnected address, Cmd.none )

        ( ConnectToServer, Disconnected serverAddress ) ->
            ( model, connectToServer serverAddress )

        ( ConnectedToServer, Disconnected serverAddress ) ->
            ( Connected
                { serverAddress = serverAddress
                , channels = Dict.empty
                , activeChannel = "general"
                , currentMessage = ""
                }
            , Cmd.none
            )

        ( UpdateChannelStatus data, Connected connectedData ) ->
            ( Connected { connectedData | channels = Chat.updateChannelStatus connectedData.channels data }
            , Cmd.none
            )

        ( AddMessageFromServer data, Connected connectedData ) ->
            ( Connected { connectedData | channels = Chat.addMessageToChannel connectedData.channels data }
            , Cmd.none
            )

        ( SetCurrentMessage message, Connected connectedData ) ->
            ( Connected { connectedData | currentMessage = message }
            , Cmd.none
            )

        ( SendMessage, Connected ({ currentMessage, activeChannel } as connectedData) ) ->
            ( Connected { connectedData | currentMessage = "" }
            , sendMessage <| Chat.encodeUserMessage activeChannel currentMessage
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

                Connected { channels, currentMessage } ->
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
                                                                    user
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
