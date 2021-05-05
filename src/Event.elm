module Event exposing (Event, GetMessagesResponse, RoomEvent(..), decodePaginatedEvents, messageEvents)

import Json.Decode as JD
import Member exposing (MemberData, decodeMember)
import Message exposing (Message, decodeMessage)
import Time


type RoomEvent
    = MessageEvent (Event Message)
    | StateEvent (Event State)
    | UnsupportedEvent (Event ())


type alias Event a =
    { eventType : String
    , content : a
    , sender : String
    , originServerTs : Time.Posix
    }


type State
    = Member String MemberData


type alias GetMessagesResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
    }


messageEvents : List RoomEvent -> List (Event Message)
messageEvents roomEvents =
    -- filter room events for message events
    List.foldl
        (\roomEvent msgs ->
            case roomEvent of
                MessageEvent msg ->
                    msg :: msgs

                _ ->
                    msgs
        )
        []
        roomEvents


stateEvents : List RoomEvent -> List (Event State)
stateEvents roomEvents =
    -- filter room events for state events
    List.foldl
        (\roomEvent xs ->
            case roomEvent of
                StateEvent x ->
                    x :: xs

                _ ->
                    xs
        )
        []
        roomEvents


decodePaginatedEvents : JD.Decoder { start : String, end : String, chunk : List RoomEvent }
decodePaginatedEvents =
    JD.map3
        (\start end chunk -> { start = start, end = end, chunk = chunk })
        (JD.field "start" JD.string)
        (JD.field "end" JD.string)
        (JD.field "chunk" <| JD.list decodeRoomEvent)


makeRoomEvent : (String -> a -> String -> Time.Posix -> RoomEvent) -> JD.Decoder a -> JD.Decoder RoomEvent
makeRoomEvent constructor contentDecoder =
    JD.map4
        constructor
        (JD.field "type" JD.string)
        (JD.field "content" contentDecoder)
        (JD.field "sender" JD.string)
        (JD.field "origin_server_ts" JD.int |> JD.map Time.millisToPosix)


decodeRoomEvent : JD.Decoder RoomEvent
decodeRoomEvent =
    JD.oneOf
        [ -- switch on room event type,
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

                        "m.room.member" ->
                            JD.field "state_key" JD.string
                                |> JD.andThen
                                    (\uid ->
                                        makeRoomEvent
                                            (\t mdata s ots ->
                                                StateEvent
                                                    { eventType = t
                                                    , content = Member uid mdata
                                                    , sender = s
                                                    , originServerTs = ots
                                                    }
                                            )
                                            decodeMember
                                    )

                        _ ->
                            JD.fail ("Unsupported event type: " ++ eventType)
                )

        -- on failure: return unsupported event
        , makeRoomEvent
            (\t msg s ots ->
                UnsupportedEvent
                    { eventType = t
                    , content = msg
                    , sender = s
                    , originServerTs = ots
                    }
            )
            (JD.succeed ())
        ]
