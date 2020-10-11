module Main exposing (main)

import Browser
import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as Pipeline exposing (required)
import Task exposing (Task)
import Time
import Url exposing (percentEncode)
import Url.Builder exposing (QueryParameter, crossOrigin)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }



-- MODEL


type alias Model =
    { config : StaticConfig
    , room : Maybe Room
    , error : Maybe String
    }


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , siteName : String
    , uniqueId : String
    }


init : StaticConfig -> ( Model, Cmd Msg )
init config =
    ( { config = config
      , room = Nothing
      , error = Nothing
      }
    , Task.attempt GotRoom <| getInitialRoom config
    )



-- UPDATE


type Msg
    = GotRoom (Result Http.Error Room)
    | ViewMoreClicked
    | GotMessages (Result Http.Error GetMessagesResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.room ) of
        ( GotRoom (Ok room), _ ) ->
            ( { model | room = Just room }
            , Cmd.none
            )

        ( GotRoom (Err err), _ ) ->
            ( { model | error = Just <| Debug.toString err }
            , Cmd.none
            )

        ( ViewMoreClicked, Just room ) ->
            ( model
            , Task.attempt GotMessages <|
                getMessages
                    { homeserverUrl = model.config.defaultHomeserverUrl
                    , accessToken = room.accessToken
                    , roomId = room.roomId
                    , from = room.end
                    }
            )

        ( ViewMoreClicked, _ ) ->
            ( { model | error = Just "Can't fetch messages: no connection to homeserver" }
            , Cmd.none
            )

        ( GotMessages (Ok newMsgs), Just room ) ->
            ( { model | room = Just <| mergeNewMessages room newMsgs }
            , Cmd.none
            )

        ( GotMessages (Err httpErr), Just room ) ->
            ( { model | error = Just <| Debug.toString httpErr }
            , Cmd.none
            )

        ( GotMessages newMsgs, _ ) ->
            ( { model | error = Just "Unexpected state: got message response without a room" }
            , Cmd.none
            )



-- MATRIX TYPES


type alias Room =
    { accessToken : String
    , roomAlias : String
    , roomId : String
    , events : List RoomEvent
    , start : String
    , end : String
    , members : Dict String RoomMember
    }


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


mergeNewMessages : Room -> GetMessagesResponse -> Room
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
                        .originServerTs msgEvent

                    UnsupportedEvent uEvt ->
                        .originServerTs uEvt
            )



-- MATRIX API HELPERS


matrixEndpoint : List String -> String -> List String -> List QueryParameter -> String
matrixEndpoint pathPrefix homeserverUrl path params =
    crossOrigin
        homeserverUrl
        (pathPrefix ++ List.map percentEncode path)
        params


clientServerEndpoint : String -> List String -> List QueryParameter -> String
clientServerEndpoint =
    matrixEndpoint [ "_matrix", "client", "r0" ]


mediaEndpoint : String -> List String -> List QueryParameter -> String
mediaEndpoint =
    matrixEndpoint [ "_matrix", "media", "r0" ]


makeRoomAlias : String -> String -> String -> String
makeRoomAlias siteName uniqueId serverName =
    "#comments_" ++ siteName ++ "_" ++ uniqueId ++ ":" ++ serverName


handleJsonResponse : JD.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result



{- INITIAL ROOM GET
   1. registerGuest
   2. getRoomId
   3. getSinceToken
   4. getMessages
   TODO: 5. ...getUsers
-}


getInitialRoom : StaticConfig -> Task Http.Error Room
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
                            , roomId = roomId
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
                            , sinceToken = sinceToken
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
                            , events = sortByTime events.chunk
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
                            , events = sortByTime data.events
                            , start = data.start
                            , end = data.end
                            , members = members
                            }
                        )
            )



-- REGISTER


type alias RegisterResponse =
    { userId : String
    , serverName : String
    , accessToken : String
    }


registerGuest : String -> Task Http.Error RegisterResponse
registerGuest homeserverUrl =
    Http.task
        { method = "POST"
        , headers = []
        , url =
            clientServerEndpoint homeserverUrl [ "register" ] [ Url.Builder.string "kind" "guest" ]
        , body = Http.stringBody "application/json" "{}"
        , resolver = Http.stringResolver <| handleJsonResponse decodeRegisterResponse
        , timeout = Nothing
        }


decodeRegisterResponse : JD.Decoder RegisterResponse
decodeRegisterResponse =
    let
        serverNameFromUserId : String -> Maybe String
        serverNameFromUserId userId =
            userId
                |> String.split ":"
                |> List.drop 1
                |> List.head
    in
    JD.map3 RegisterResponse
        -- userId
        (JD.field "user_id" JD.string)
        -- serverName
        (JD.field "user_id" JD.string
            |> JD.andThen
                (\userId ->
                    case serverNameFromUserId userId of
                        Nothing ->
                            JD.fail <| "Could not parse serverName from userId: " ++ userId

                        Just serverName ->
                            JD.succeed serverName
                )
        )
        -- accessToken
        (JD.field "access_token" JD.string)



-- ROOM ID LOOKUP


getRoomId : String -> String -> Task Http.Error String
getRoomId homeserverUrl roomAlias =
    Http.task
        { method = "GET"
        , url = clientServerEndpoint homeserverUrl [ "directory", "room", roomAlias ] []
        , headers = []
        , body = Http.stringBody "application/json" "{}"
        , resolver = Http.stringResolver <| handleJsonResponse <| JD.field "room_id" JD.string
        , timeout = Nothing
        }



-- SINCE TOKEN


getSinceToken : { homeserverUrl : String, accessToken : String, roomId : String } -> Task Http.Error String
getSinceToken { homeserverUrl, accessToken, roomId } =
    Http.task
        { method = "GET"
        , url =
            clientServerEndpoint homeserverUrl
                [ "events" ]
                [ Url.Builder.string "room_id" roomId
                , Url.Builder.int "timeout" 0
                ]
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.stringBody "application/json" "{}"

        -- TODO - better message
        , resolver = Http.stringResolver <| handleJsonResponse <| JD.field "end" JD.string
        , timeout = Nothing
        }



-- MESSAGES


type alias GetMessagesResponse =
    { start : String
    , end : String
    , chunk : List RoomEvent
    }


getMessages :
    { homeserverUrl : String, accessToken : String, roomId : String, from : String }
    -> Task Http.Error GetMessagesResponse
getMessages { homeserverUrl, accessToken, roomId, from } =
    Http.task
        { method = "GET"
        , url =
            clientServerEndpoint homeserverUrl
                [ "rooms", roomId, "messages" ]
                [ Url.Builder.string "dir" "b"
                , Url.Builder.string "from" <| from
                ]
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.stringBody "application/json" "{}"
        , resolver = Http.stringResolver <| handleJsonResponse decodeMessages
        , timeout = Nothing
        }


decodeMessages : JD.Decoder { start : String, end : String, chunk : List RoomEvent }
decodeMessages =
    JD.map3
        (\start end chunk -> { start = start, end = end, chunk = chunk })
        (JD.field "start" JD.string)
        (JD.field "end" JD.string)
        (JD.field "chunk" <| JD.list decodeRoomEvent)



-- JOINED MEMBERS


type alias RoomMember =
    { displayname : Maybe String
    , avatarUrl : Maybe String
    , userId : String
    }


getJoinedMembers : { homeserverUrl : String, accessToken : String, roomId : String } -> Task Http.Error (Dict String RoomMember)
getJoinedMembers { homeserverUrl, accessToken, roomId } =
    Http.task
        { method = "GET"
        , url = clientServerEndpoint homeserverUrl [ "rooms", roomId, "members" ] []
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse decodeRoomMemberResponse
        , timeout = Nothing
        }


decodeRoomMemberResponse : JD.Decoder (Dict String RoomMember)
decodeRoomMemberResponse =
    (JD.field "chunk" <| JD.list decodeRoomMemberEvent)
        |> JD.andThen
            (\roomMembers ->
                roomMembers
                    |> List.map (\rm -> ( rm.userId, rm ))
                    |> Dict.fromList
                    |> JD.succeed
            )


decodeRoomMemberEvent : JD.Decoder RoomMember
decodeRoomMemberEvent =
    (JD.field "content" <| decodeRoomMemberContent)
        |> JD.andThen
            (\content ->
                JD.field "state_key" JD.string
                    |> JD.andThen (\uid -> JD.succeed <| RoomMember content.displayname content.avatarUrl uid)
            )


decodeRoomMemberContent : JD.Decoder { displayname : Maybe String, avatarUrl : Maybe String }
decodeRoomMemberContent =
    JD.map2 (\dn au -> { displayname = dn, avatarUrl = au })
        (JD.maybe <| JD.field "displayname" JD.string)
        (JD.maybe <| JD.field "avatar_url" JD.string)


mxcToHttp : String -> String -> Maybe String
mxcToHttp homeserverUrl mxcUrl =
    let
        serverName =
            mxcUrl
                |> String.dropLeft 6
                |> String.split "/"
                |> List.head

        mediaId =
            mxcUrl
                |> String.split "/"
                |> (List.reverse >> List.head)
    in
    Maybe.map2
        (\sn mid ->
            mediaEndpoint homeserverUrl
                [ "thumbnail", sn, mid ]
                [ Url.Builder.int "width" 32
                , Url.Builder.int "height" 32
                , Url.Builder.string "method" "crop"
                ]
        )
        serverName
        mediaId



-- DECODERS


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



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        [ -- view errors
          h5 [] <|
            case model.error of
                Nothing ->
                    []

                Just errmsg ->
                    [ text <| "ERROR: " ++ errmsg ]
        , -- show MessageEvents
          case model.room of
            Nothing ->
                p [] [ text "Getting comments..." ]

            Just room ->
                div []
                    [ viewRoomEvents model.config.defaultHomeserverUrl room.members room.events
                    , viewMoreButton
                    ]
        ]


viewRoomEvents : String -> Dict String RoomMember -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl members)
            (onlyMessageEvents roomEvents)


toUtcString : Int -> String
toUtcString timestamp =
    let
        time =
            Time.millisToPosix timestamp

        dateStr =
            String.fromInt (Time.toYear Time.utc time)
                ++ "-"
                ++ String.fromInt (Date.monthToNumber <| Time.toMonth Time.utc time)
                ++ "-"
                ++ String.fromInt (Time.toDay Time.utc time)

        timeStr =
            String.fromInt (Time.toHour Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toMinute Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toSecond Time.utc time)
                ++ " (UTC)"
    in
    dateStr ++ " " ++ timeStr


viewMessageEvent : String -> Dict String RoomMember -> Event Message -> Html Msg
viewMessageEvent defaultHomeserverUrl members messageEvent =
    let
        member : Maybe RoomMember
        member =
            Dict.get messageEvent.sender members

        name : String
        name =
            member
                |> Maybe.map (\m -> Maybe.withDefault "" m.displayname)
                |> Maybe.withDefault messageEvent.sender

        avatarUrl : Maybe String
        avatarUrl =
            member
                |> Maybe.map
                    (\m ->
                        case m.avatarUrl of
                            Just mxcUrl ->
                                mxcToHttp defaultHomeserverUrl mxcUrl

                            Nothing ->
                                Nothing
                    )
                |> Maybe.withDefault Nothing

        timeStr : String
        timeStr =
            toUtcString messageEvent.originServerTs

        textBody =
            case messageEvent.content of
                Text textMessage ->
                    textMessage.body

                _ ->
                    "unsupported message event"
    in
    div []
        [ img [ src <| Maybe.withDefault "" avatarUrl ] []
        , p [] [ text <| name ++ " " ++ timeStr ]
        , p [] [ text textBody ]
        ]


viewMoreButton : Html Msg
viewMoreButton =
    button
        [ onClick ViewMoreClicked ]
        [ text "View More!" ]
