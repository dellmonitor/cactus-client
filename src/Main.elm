module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as Pipeline exposing (required)
import Task exposing (Task)
import Url exposing (percentEncode)
import Url.Builder exposing (QueryParameter, crossOrigin)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { config : StaticConfig

    -- room state
    , room : Maybe Room

    -- used for fatal errors
    , error : Maybe String
    }


type alias StaticConfig =
    -- TODO: allow config to use different homeservers for different tasks
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoom (Ok room) ->
            ( { model | room = Just room }
            , Cmd.none
            )

        GotRoom (Err err) ->
            -- HTTP/Json error
            ( { model | error = Just <| Debug.toString err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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


getMessageEvents : List RoomEvent -> List (Event Message)
getMessageEvents roomEvents =
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


mergeRoomEvents : List RoomEvent -> List RoomEvent -> List RoomEvent
mergeRoomEvents events newEvents =
    (events ++ newEvents)
        |> List.sortBy
            (\e ->
                case e of
                    MessageEvent msgEvent ->
                        .originServerTs msgEvent

                    UnsupportedEvent uEvt ->
                        .originServerTs uEvt
            )



-- MATRIX API HELPERS


clientServerEndpoint : String -> List String -> List QueryParameter -> String
clientServerEndpoint homeserverUrl endpoint params =
    crossOrigin
        homeserverUrl
        ([ "_matrix", "client", "r0" ]
            ++ List.map percentEncode endpoint
        )
        params


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



-- ROOM ID LOOKUP


getRoomId : String -> String -> Task Http.Error String
getRoomId homeserverUrl roomAlias =
    Http.task
        { method = "GET"
        , url = clientServerEndpoint homeserverUrl [ "directory", "room", roomAlias ] []
        , headers = []
        , body = Http.stringBody "application/json" "{}"
        , resolver = Http.stringResolver <| handleJsonResponse decodeRoomId
        , timeout = Nothing
        }



-- INITIAL SINCE TOKEN


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


getMessages :
    { homeserverUrl : String
    , accessToken : String
    , roomId : String
    , since : String
    }
    ->
        Task Http.Error
            { start : String
            , end : String
            , chunk : List RoomEvent
            }
getMessages { homeserverUrl, accessToken, roomId, since } =
    Http.task
        { method = "GET"
        , url =
            clientServerEndpoint homeserverUrl
                [ "rooms", roomId, "messages" ]
                [ Url.Builder.string "dir" "b" ]
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


type alias Room =
    { accessToken : String
    , roomAlias : String
    , roomId : String
    , events : List RoomEvent
    }



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
                    , since = data.sinceToken
                    }
                    |> Task.map
                        (\events ->
                            { accessToken = data.accessToken
                            , roomAlias = data.roomAlias
                            , roomId = data.roomId
                            , events = events.chunk
                            }
                        )
            )



-- DECODERS


decodeRegisterResponse : JD.Decoder RegisterResponse
decodeRegisterResponse =
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


serverNameFromUserId : String -> Maybe String
serverNameFromUserId userId =
    userId
        |> String.split ":"
        |> List.drop 1
        |> List.head


decodeRoomId : JD.Decoder String
decodeRoomId =
    JD.field "room_id" JD.string


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
                viewRoomEvents room.events
        ]


viewRoomEvents : List RoomEvent -> Html Msg
viewRoomEvents roomEvents =
    div [] <|
        List.map
            viewMessageEvent
            (getMessageEvents roomEvents)


viewMessageEvent : Event Message -> Html Msg
viewMessageEvent messageEvent =
    let
        textBody =
            case messageEvent.content of
                Text textMessage ->
                    textMessage.body

                _ ->
                    "unsupported message event"
    in
    div []
        [ p [] [ text messageEvent.sender ]
        , p [] [ text textBody ]
        ]
