module Message exposing (Event, GetMessagesResponse, Message(..), RoomEvent(..), getMessages, onlyMessageEvents)

import ApiUtils exposing (apiRequest, clientEndpoint)
import Http
import Json.Decode as JD
import Task exposing (Task)
import Url.Builder


type alias Event a =
    { eventType : String
    , content : a
    , sender : String
    , originServerTs : Int
    }


type RoomEvent
    = MessageEvent (Event Message)
    | UnsupportedEvent (Event ())


type Message
    = Text { body : String, format : Maybe String, formatted_body : Maybe String }
    | Emote
    | Notice
    | Image
    | File
    | Audio
    | Location
    | Video
    | UnsupportedMessageType


onlyMessageEvents : List RoomEvent -> List (Event Message)
onlyMessageEvents roomEvents =
    -- filter room events for message events
    List.foldl
        (\roomEvent messageEvents ->
            case roomEvent of
                MessageEvent msg ->
                    msg :: messageEvents

                _ ->
                    messageEvents
        )
        []
        roomEvents


type alias GetMessagesResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
    }


getMessages :
    { homeserverUrl : String, accessToken : String, roomId : String, from : String }
    -> Task Http.Error GetMessagesResponse
getMessages { homeserverUrl, accessToken, roomId, from } =
    apiRequest
        { method = "GET"
        , url =
            clientEndpoint homeserverUrl
                [ "rooms", roomId, "messages" ]
                [ Url.Builder.string "dir" "b"
                , Url.Builder.string "from" <| from
                ]
        , accessToken = Just accessToken
        , responseDecoder = decodeMessages
        , body = Http.emptyBody
        }


decodeMessages : JD.Decoder { start : String, end : String, chunk : List RoomEvent }
decodeMessages =
    JD.map3
        (\start end chunk -> { start = start, end = end, chunk = chunk })
        (JD.field "start" JD.string)
        (JD.field "end" JD.string)
        (JD.field "chunk" <| JD.list decodeRoomEvent)


decodeRoomEvent : JD.Decoder RoomEvent
decodeRoomEvent =
    let
        makeRoomEvent : (String -> a -> String -> Int -> RoomEvent) -> JD.Decoder a -> JD.Decoder RoomEvent
        makeRoomEvent constructor contentDecoder =
            JD.map4
                constructor
                (JD.field "type" JD.string)
                (JD.field "content" contentDecoder)
                (JD.field "sender" JD.string)
                (JD.field "origin_server_ts" JD.int)
    in
    JD.field "type" JD.string
        |> JD.andThen
            (\eventType ->
                case eventType of
                    "m.room.message" ->
                        makeRoomEvent
                            (\t msg s ots ->
                                MessageEvent
                                    { eventType = t
                                    , content = msg
                                    , sender = s
                                    , originServerTs = ots
                                    }
                            )
                            decodeMessage

                    _ ->
                        makeRoomEvent
                            (\t msg s ots ->
                                UnsupportedEvent
                                    { eventType = t
                                    , content = msg
                                    , sender = s
                                    , originServerTs = ots
                                    }
                            )
                            (JD.succeed ())
            )


decodeMessage : JD.Decoder Message
decodeMessage =
    JD.field "msgtype" JD.string
        |> JD.andThen
            (\mt ->
                case mt of
                    "m.text" ->
                        JD.map3
                            (\body format formatted_body ->
                                Text
                                    { body = body
                                    , format = format
                                    , formatted_body = formatted_body
                                    }
                            )
                            (JD.field "body" JD.string)
                            (JD.maybe <| JD.field "format" JD.string)
                            (JD.maybe <| JD.field "formatted_body" JD.string)

                    "m.emote" ->
                        JD.succeed Emote

                    "m.notice" ->
                        JD.succeed Notice

                    "m.image" ->
                        JD.succeed Image

                    "m.file" ->
                        JD.succeed File

                    "m.audio" ->
                        JD.succeed Audio

                    "m.location" ->
                        JD.succeed Location

                    "m.video" ->
                        JD.succeed Video

                    _ ->
                        JD.succeed UnsupportedMessageType
            )
