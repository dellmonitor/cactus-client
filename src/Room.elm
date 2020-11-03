module Room exposing (Room, getInitialRoom, getRoomAsGuest, mergeNewMessages)

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Member exposing (Member, getJoinedMembers)
import Message exposing (RoomEvent(..), getMessages)
import Session exposing (Session, authenticatedRequest, registerGuest)
import Task exposing (Task)
import Time
import Url.Builder


type alias Room =
    { roomAlias : String
    , roomId : String
    , events : List RoomEvent
    , start : String
    , end : String
    , members : Dict String Member
    , time : Time.Posix
    }


mergeNewMessages : Room -> { a | start : String, end : String, chunk : List RoomEvent } -> Room
mergeNewMessages room newMessages =
    { room
        | events = sortByTime (room.events ++ newMessages.chunk)
        , end = newMessages.end
    }


sortByTime : List RoomEvent -> List RoomEvent
sortByTime events =
    events
        |> List.sortBy
            (\e ->
                case e of
                    MessageEvent msgEvent ->
                        .originServerTs msgEvent |> Time.posixToMillis

                    UnsupportedEvent uEvt ->
                        .originServerTs uEvt |> Time.posixToMillis
            )


{-| Register a guest account on the default homeserver, then get a room using
the guest access token.
-}
getRoomAsGuest : { homeserverUrl : String, roomAlias : String } -> Task Http.Error ( Session, Room )
getRoomAsGuest { homeserverUrl, roomAlias } =
    registerGuest homeserverUrl
        |> Task.andThen
            (\session ->
                getInitialRoom session roomAlias
                    |> Task.map (\room -> ( session, room ))
            )


{-| This (admittedly huge) call chains 4 API requests to set up the room:

1.  Look up roomId using roomAlias
2.  Get a token to sync events from
3.  Get message events from sync token
4.  Get current room members
5.  (Get current time)

The Task eventually completes a Room record

-}
getInitialRoom : Session -> String -> Task Http.Error Room
getInitialRoom session roomAlias =
    let
        -- find roomId
        addRoomId =
            getRoomId session roomAlias
                |> Task.map
                    (\roomId ->
                        { roomAlias = roomAlias
                        , roomId = roomId
                        }
                    )

        -- get since token from /events
        addSinceToken data =
            getSinceToken session data.roomId
                |> Task.map
                    (\sinceToken ->
                        { roomId = data.roomId
                        , roomAlias = data.roomAlias
                        , --
                          sinceToken = sinceToken
                        }
                    )

        -- get messages from /room/{roomId}/messages
        addEvents data =
            getMessages session data.roomId data.sinceToken
                |> Task.map
                    (\events ->
                        { roomAlias = data.roomAlias
                        , roomId = data.roomId
                        , --
                          events = sortByTime events.chunk
                        , start = events.start
                        , end = events.end
                        }
                    )

        -- get joined members
        addMembers data =
            getJoinedMembers session data.roomId
                |> Task.map
                    (\members ->
                        { roomAlias = data.roomAlias
                        , roomId = data.roomId
                        , events = data.events
                        , start = data.start
                        , end = data.end
                        , --
                          members = members
                        }
                    )

        -- note current time
        addTime data =
            Time.now
                |> Task.map
                    (\time ->
                        { roomAlias = data.roomAlias
                        , roomId = data.roomId
                        , events = data.events
                        , start = data.start
                        , end = data.end
                        , members = data.members
                        , --
                          time = time
                        }
                    )
    in
    addRoomId
        |> Task.andThen addSinceToken
        |> Task.andThen addEvents
        |> Task.andThen addMembers
        |> Task.andThen addTime


{-| Make a GET request to resolve a roomId from a given roomAlias.
-}
getRoomId : Session -> String -> Task Http.Error String
getRoomId session roomAlias =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "directory", "room", roomAlias ]
        , params = []
        , responseDecoder = JD.field "room_id" JD.string
        , body = Http.emptyBody
        }


{-| Make a GET to events endpoint - only to extract a "since-token", which can
be used to fetch events from another endpoint.
-}
getSinceToken : Session -> String -> Task Http.Error String
getSinceToken session roomId =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "events" ]
        , params =
            [ Url.Builder.string "room_id" roomId
            , Url.Builder.int "timeout" 0
            ]
        , responseDecoder = JD.field "end" JD.string
        , body = Http.emptyBody
        }
