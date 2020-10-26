module Room exposing (Room, getInitialRoom, makeRoomAlias, mergeNewMessages)

import ApiUtils exposing (apiRequest, clientEndpoint, serverNameFromId)
import Authentication exposing (registerGuest)
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Member exposing (Member, getJoinedMembers)
import Message exposing (RoomEvent(..), getMessages)
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
3.  Get message events from sync token
4.  Get current room members
5.  (Get current time)

The Task eventually completes a Room record.

-}
getInitialRoom : { defaultHomeserverUrl : String, siteName : String, uniqueId : String } -> Task Http.Error Room
getInitialRoom config =
    let
        addRoomAlias authdata =
            let
                serverName =
                    -- TODO: this doesn't work for external server
                    serverNameFromId authdata.userId

                roomAlias =
                    Maybe.map
                        (makeRoomAlias config.siteName config.uniqueId)
                        serverName
            in
            case roomAlias of
                Nothing ->
                    Task.fail <| Http.BadBody "Could not determine server name from user id"

                Just ra ->
                    Task.succeed
                        { accessToken = authdata.accessToken
                        , roomAlias = ra
                        }

        addRoomId data =
            getRoomId config.defaultHomeserverUrl data.roomAlias
                |> Task.map
                    (\roomId ->
                        { accessToken = data.accessToken
                        , roomAlias = data.roomAlias
                        , --
                          roomId = roomId
                        }
                    )

        addSinceToken data =
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

        addEvents data =
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

        addMembers data =
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

        addTime data =
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
    in
    -- Register a guest user and and get serverName
    registerGuest config.defaultHomeserverUrl
        -- figure out which roomAlias to hit
        |> Task.andThen addRoomAlias
        -- then find roomId from roomAlias
        |> Task.andThen addRoomId
        -- get since token from /events
        |> Task.andThen addSinceToken
        -- get messages from /room/{roomId}/messages
        |> Task.andThen addEvents
        -- get joined members
        |> Task.andThen addMembers
        -- note current time
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
