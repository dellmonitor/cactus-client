module Room exposing (Room, commentCount, getInitialRoom, getMoreMessages, getRoomAsGuest, mergeNewMessages, viewRoom)

import Accessibility exposing (Html, button, div, text)
import Dict exposing (Dict)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Member exposing (Member, getJoinedMembers)
import Message exposing (GetMessagesResponse, RoomEvent(..), decodeMessages, getMessages, messageEvents, viewMessageEvent)
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



-- merge new messages
-- if new messages isn't empty and the current message count is less than
-- showComments:
--   get more messages
--


{-| Get more messages messages from current end token of the room.
Get strictly less than

To be merged using `mergeNewMessages`

-}
getMoreMessages : Session -> Room -> Task Session.Error GetMessagesResponse
getMoreMessages session room =
    getMessages session room.roomId room.start


{-| Count the number of renderable messages in the room
-}
commentCount : Room -> Int
commentCount room =
    List.length <| messageEvents room.events


mergeNewMessages : Room -> { a | start : String, end : String, chunk : List RoomEvent } -> Room
mergeNewMessages room newMessages =
    { room
        | events = sortByTime (room.events ++ newMessages.chunk)
        , start = newMessages.end
    }


sortByTime : List RoomEvent -> List RoomEvent
sortByTime events =
    events
        |> List.sortBy
            (\e ->
                case e of
                    MessageEvent msgEvent ->
                        .originServerTs msgEvent |> Time.posixToMillis

                    StateEvent stEvent ->
                        .originServerTs stEvent |> Time.posixToMillis

                    UnsupportedEvent uEvt ->
                        .originServerTs uEvt |> Time.posixToMillis
            )


{-| Register a guest account on the default homeserver, then get a room using
the guest access token.
-}
getRoomAsGuest : { homeserverUrl : String, roomAlias : String } -> Task Session.Error ( Session, Room )
getRoomAsGuest { homeserverUrl, roomAlias } =
    registerGuest homeserverUrl
        |> Task.andThen
            (\session ->
                getInitialRoom session roomAlias
                    |> Task.map (\room -> ( session, room ))
            )


{-| This call chains 3 API requests to set up the room:

1.  Look up roomId using roomAlias
2.  Get message events and pagination tokens
3.  Get current room members
4.  (Get current time)

The Task eventually completes a Room record

-}
getInitialRoom : Session -> String -> Task Session.Error Room
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

        -- get messages from /room/{roomId}/messages
        addEvents data =
            getInitialSync session data.roomId
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
        |> Task.andThen addEvents
        |> Task.andThen addMembers
        |> Task.andThen addTime


{-| Make a GET request to resolve a roomId from a given roomAlias.
-}
getRoomId : Session -> String -> Task Session.Error String
getRoomId session roomAlias =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "directory", "room", roomAlias ]
        , params = []
        , responseDecoder = JD.field "room_id" JD.string
        , body = Http.emptyBody
        }


{-| Get initial room events and sync tokens to get further messages
-}
getInitialSync : Session -> String -> Task Session.Error { chunk : List RoomEvent, start : String, end : String }
getInitialSync session roomId =
    authenticatedRequest
        session
        { method = "GET"
        , path = [ "rooms", roomId, "initialSync" ]
        , params = []
        , responseDecoder = JD.field "messages" decodeMessages
        , body = Http.emptyBody
        }


{-| View all of the comments in a Room
The `homeserverUrl` is used translate mxc:// to media API endpoints
-}
viewRoom : Room -> String -> Int -> Html msg
viewRoom room homeserverUrl count =
    viewRoomEvents
        homeserverUrl
        room.time
        room.members
        room.events
        count


viewRoomEvents : String -> Time.Posix -> Dict String Member -> List RoomEvent -> Int -> Html msg
viewRoomEvents defaultHomeserverUrl time members roomEvents count =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl time members)
            (messageEvents roomEvents |> List.take count)
