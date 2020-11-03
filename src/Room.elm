module Room exposing (Room, getInitialRoom, getRoomAsGuest, getRoomAsUser, mergeNewMessages)

import ApiUtils exposing (apiRequest, clientEndpoint)
import Authentication exposing (Authentication, login, registerGuest)
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Member exposing (Member, getJoinedMembers)
import Message exposing (RoomEvent(..), getMessages)
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
getRoomAsGuest : { homeserverUrl : String, roomAlias : String } -> Task Http.Error ( Authentication, Room )
getRoomAsGuest { homeserverUrl, roomAlias } =
    registerGuest homeserverUrl
        |> Task.andThen
            (\auth ->
                getInitialRoom { auth = auth, roomAlias = roomAlias }
                    |> Task.map (\room -> ( auth, room ))
            )


{-| Login using password credentials on a chosen homeserver, then get a room
using the user access token.
-}
getRoomAsUser : { homeserverUrl : String, roomAlias : String, user : String, password : String } -> Task Http.Error ( Authentication, Room )
getRoomAsUser { homeserverUrl, roomAlias, user, password } =
    login { homeserverUrl = homeserverUrl, user = user, password = password }
        |> Task.andThen
            (\auth ->
                getInitialRoom { auth = auth, roomAlias = roomAlias }
                    |> Task.map (\room -> ( auth, room ))
            )


{-| This (admittedly huge) call chains 4 API requests to set up the room:

1.  Look up roomId using roomAlias
2.  Get a token to sync events from
3.  Get message events from sync token
4.  Get current room members
5.  (Get current time)

The Task eventually completes a Room record

-}
getInitialRoom : { auth : Authentication, roomAlias : String } -> Task Http.Error Room
getInitialRoom { auth, roomAlias } =
    let
        -- find roomId
        addRoomId =
            getRoomId auth.homeserverUrl roomAlias
                |> Task.map
                    (\roomId ->
                        { roomAlias = roomAlias
                        , roomId = roomId
                        }
                    )

        -- get since token from /events
        addSinceToken data =
            getSinceToken
                { homeserverUrl = auth.homeserverUrl
                , accessToken = auth.accessToken
                , roomId = data.roomId
                }
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
            getMessages
                { homeserverUrl = auth.homeserverUrl
                , accessToken = auth.accessToken
                , roomId = data.roomId
                , from = data.sinceToken
                }
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
            getJoinedMembers
                { homeserverUrl = auth.homeserverUrl
                , accessToken = auth.accessToken
                , roomId = data.roomId
                }
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
getRoomId : String -> String -> Task Http.Error String
getRoomId homeserverUrl roomAlias =
    apiRequest
        { method = "GET"
        , url = clientEndpoint homeserverUrl [ "directory", "room", roomAlias ] []
        , responseDecoder = JD.field "room_id" JD.string
        , accessToken = Nothing
        , body = Http.emptyBody
        }


{-| Make a GET to events endpoint - only to extract a "since-token", which can
be used to fetch events from another endpoint.
-}
getSinceToken : { homeserverUrl : String, accessToken : String, roomId : String } -> Task Http.Error String
getSinceToken { homeserverUrl, accessToken, roomId } =
    apiRequest
        { method = "GET"
        , url =
            clientEndpoint homeserverUrl
                [ "events" ]
                [ Url.Builder.string "room_id" roomId
                , Url.Builder.int "timeout" 0
                ]
        , accessToken = Just accessToken
        , responseDecoder = JD.field "end" JD.string
        , body = Http.emptyBody
        }
