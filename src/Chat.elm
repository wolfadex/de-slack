module Chat exposing
    ( Address
    , Channel
    , ChannelName
    , Channels
    , Message
    , User
    , addMessageToChannel
    , decodeMessage
    , decodeUserMessage
    , encodeChannelStatus
    , encodeMessage
    , encodeUserMessage
    , updateChannelStatus
    )

import Debug
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Set exposing (Set)
import Time exposing (Posix)


type alias Address =
    String


type alias ChannelName =
    String


type alias Message =
    { user : Address
    , content : String
    , time : Posix
    }


type alias Channel =
    { name : ChannelName
    , messages : List Message
    , activeUsers : Set Address
    }


type alias Channels =
    Dict ChannelName Channel


type alias User =
    { name : String }


encodeChannelStatus : Channel -> Value
encodeChannelStatus { name, activeUsers } =
    Encode.object
        [ ( "channel", Encode.string name )
        , ( "status", Encode.list Encode.string <| Set.toList activeUsers )
        ]


updateChannelStatus : Channels -> Value -> Channels
updateChannelStatus channels data =
    case Decode.decodeValue decodeChannelStatus data of
        Ok ( name, activeUsers ) ->
            let
                users =
                    Set.fromList activeUsers
            in
            Dict.update
                name
                (\maybeChannel ->
                    case maybeChannel of
                        Nothing ->
                            Just { name = name, activeUsers = users, messages = [] }

                        Just channel ->
                            Just { channel | activeUsers = users }
                )
                channels

        Err _ ->
            channels


decodeChannelStatus : Decoder ( ChannelName, List Address )
decodeChannelStatus =
    Decode.map2
        Tuple.pair
        (Decode.field "channel" Decode.string)
        (Decode.field "status" <| Decode.list Decode.string)


encodeMessage : ChannelName -> Message -> Value
encodeMessage channel { user, content, time } =
    Encode.object
        [ ( "channel", Encode.string channel )
        , ( "message"
          , Encode.object
                [ ( "user", Encode.string user )
                , ( "content", Encode.string content )
                , ( "time", Encode.int <| Time.posixToMillis time )
                ]
          )
        ]


decodeMessage : Decoder ( ChannelName, Message )
decodeMessage =
    Decode.map2
        Tuple.pair
        (Decode.field "channel" Decode.string)
        (Decode.field "message" <|
            Decode.map3 Message
                (Decode.field "user" Decode.string)
                (Decode.field "content" Decode.string)
                (Decode.field "time" decodeTime)
        )


decodeTime : Decoder Posix
decodeTime =
    Decode.int
        |> Decode.andThen (\time -> Decode.succeed <| Time.millisToPosix time)


addMessageToChannel : Channels -> Value -> Channels
addMessageToChannel channels data =
    case Decode.decodeValue decodeMessage data of
        Err err ->
            Debug.log (Decode.errorToString err) channels

        Ok ( channelName, message ) ->
            Dict.update
                channelName
                (\maybeChannel ->
                    case maybeChannel of
                        Nothing ->
                            Nothing

                        Just channel ->
                            Just { channel | messages = message :: channel.messages }
                )
                channels


encodeUserMessage : ChannelName -> String -> Value
encodeUserMessage channelName content =
    Encode.object
        [ ( "channelName", Encode.string channelName )
        , ( "content", Encode.string content )
        ]


decodeUserMessage : Decoder ( ChannelName, String )
decodeUserMessage =
    Decode.map2
        Tuple.pair
        (Decode.field "channelName" Decode.string)
        (Decode.field "content" Decode.string)
