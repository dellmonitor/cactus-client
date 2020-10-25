module Room exposing (Room, getInitialRoom, mergeNewMessages)

import ApiUtils exposing (apiRequest, clientEndpoint)
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Member exposing (Member, getJoinedMembers)
import Message exposing (RoomEvent(..), getMessages)
import Register exposing (registerGuest)
import Task exposing (Task)
import Time
import Url.Builder


type alias Room =
    { accessToken : String
    , roomAlias : String
    , roomId : String
    , events : List RoomEvent
    , start : String
    , end : String
    , members : Dict String Member
    , time : Time.Posix
    }


{-| Make a matrix room alias given a sitename, a unique id for the comments
section, and a matrix homeserver servername.

    makeRoomAlias "myblog.com" "october-blogpost" "matrix.example.com" == "#comments_myblog.com_october-blogpost:matrix.example.com"

-}
makeRoomAlias : String -> String -> String -> String
makeRoomAlias siteName uniqueId serverName =
    "#comments_" ++ siteName ++ "_" ++ uniqueId ++ ":" ++ serverName


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


{-| This (admittedly huge) call chains 4 API requests to set up the room:

1.  Register a guest account, get an access token
2.  Get a token to sync events from
3.  Get message events
4.  Get current room members
5.  (Get current time)

The Task eventually completes a Room record.

-}
getInitialRoom : { defaultHomeserverUrl : String, siteName : String, uniqueId : String } -> Task Http.Error Room
getInitialRoom config =
    -- Register a guest user and and get serverName
    registerGuest config.defaultHomeserverUrl
        -- find roomId from roomAlias
        |> Task.andThen
            (\data ->
                let
                    roomAlias =
                        makeRoomAlias
                            config.siteName
                            config.uniqueId
                            data.serverName
                in
                getRoomId config.defaultHomeserverUrl roomAlias
                    |> Task.map
                        (\roomId ->
                            { accessToken = data.accessToken
                            , --
                              roomId = roomId
                            , roomAlias = roomAlias
                            }
                        )
            )
        -- get since token from /events
        |> Task.andThen
            (\data ->
                getSinceToken
                    { homeserverUrl = config.defaultHomeserverUrl
                    , accessToken = data.accessToken
                    , roomId = data.roomId
                    }
                    |> Task.map
                        (\sinceToken ->
                            { accessToken = data.accessToken
                            , roomId = data.roomId
                            , roomAlias = data.roomAlias
                            , --
                              sinceToken = sinceToken
                            }
                        )
            )
        -- get messages from /room/{roomId}/messages
        |> Task.andThen
            (\data ->
                getMessages
                    { homeserverUrl = config.defaultHomeserverUrl
                    , accessToken = data.accessToken
                    , roomId = data.roomId
                    , from = data.sinceToken
                    }
                    |> Task.map
                        (\events ->
                            { accessToken = data.accessToken
                            , roomAlias = data.roomAlias
                            , roomId = data.roomId
                            , --
                              events = sortByTime events.chunk
                            , start = events.start
                            , end = events.end
                            }
                        )
            )
        -- get joined members
        |> Task.andThen
            (\data ->
                getJoinedMembers
                    { homeserverUrl = config.defaultHomeserverUrl
                    , accessToken = data.accessToken
                    , roomId = data.roomId
                    }
                    |> Task.map
                        (\members ->
                            { accessToken = data.accessToken
                            , roomAlias = data.roomAlias
                            , roomId = data.roomId
                            , events = data.events
                            , start = data.start
                            , end = data.end
                            , --
                              members = members
                            }
                        )
            )
        |> Task.andThen
            (\data ->
                Time.now
                    |> Task.map
                        (\time ->
                            { accessToken = data.accessToken
                            , roomAlias = data.roomAlias
                            , roomId = data.roomId
                            , events = data.events
                            , start = data.start
                            , end = data.end
                            , members = data.members
                            , --
                              time = time
                            }
                        )
            )


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
