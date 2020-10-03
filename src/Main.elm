module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as Pipeline exposing (required)
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
    , roomState : RoomState

    -- used for fatal errors
    , error : Maybe String
    }


type alias StaticConfig =
    -- TODO: allow config to use different homeservers for different tasks
    { defaultHomeserverUrl : String
    , siteName : String
    , uniqueId : String
    }


type RoomState
    = NoAccess
    | Registered
        { accessToken : String
        , roomAlias : String
        }
    | Joined
        { accessToken : String
        , roomAlias : String
        , roomId : String
        , roomEvents : List RoomEvent
        }


init : StaticConfig -> ( Model, Cmd Msg )
init config =
    ( { config = config
      , roomState = NoAccess

      -- display error
      , error = Nothing
      }
    , Cmd.batch [ registerGuest config.defaultHomeserverUrl ]
    )



-- UPDATE


type Msg
    = RegisteredGuest (Result Http.Error RegisterResponse)
    | JoinedRoom (Result Http.Error String)
    | GotSync (Result Http.Error SyncResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        unexpectedStateError =
            Just <| "Unexpected RoomState:" ++ Debug.toString model.roomState ++ "\nMsg:" ++ Debug.toString msg
    in
    case ( msg, model.roomState ) of
        ( RegisteredGuest (Ok response), NoAccess ) ->
            let
                roomAlias =
                    makeRoomAlias
                        model.config.siteName
                        model.config.uniqueId
                        response.serverName
            in
            ( { model
                | roomState =
                    Registered
                        { accessToken = response.accessToken
                        , roomAlias = roomAlias
                        }
              }
            , joinRoom
                model.config.defaultHomeserverUrl
                response.accessToken
                roomAlias
            )

        ( RegisteredGuest (Err err), _ ) ->
            -- HTTP/Json error
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        ( RegisteredGuest _, _ ) ->
            -- unexpected http response - impossible state?
            ( { model | error = unexpectedStateError }, Cmd.none )

        ( JoinedRoom (Ok roomId), Registered { accessToken, roomAlias } ) ->
            ( { model
                | roomState =
                    Joined
                        { accessToken = accessToken
                        , roomAlias = roomAlias
                        , roomId = roomId
                        , roomEvents = []
                        }
              }
            , -- initial sync
              syncClient
                model.config.defaultHomeserverUrl
                accessToken
                roomId
                Nothing
            )

        ( JoinedRoom (Err err), Registered _ ) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        ( JoinedRoom _, _ ) ->
            ( { model | error = unexpectedStateError }
            , Cmd.none
            )

        ( GotSync (Ok syncResponse), Joined roomState ) ->
            let
                newRoomEvents =
                    mergeRoomEvents
                        roomState.roomEvents
                        syncResponse.room.timeline.events

                newRoomState =
                    { roomState | roomEvents = newRoomEvents }
            in
            ( { model | roomState = Joined newRoomState }
              -- TODO: another sync
            , Cmd.none
            )

        ( GotSync (Err err), Joined _ ) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        ( GotSync _, _ ) ->
            ( { model | error = unexpectedStateError }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MATRIX TYPES


type alias RegisterResponse =
    { userId : String
    , serverName : String
    , accessToken : String
    }


type alias SyncResponse =
    { room : { timeline : SyncTimeline }
    , nextBatch : String
    }


type alias SyncTimeline =
    { prevBatch : String
    , events : List RoomEvent
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



-- MATRIX API


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


registerGuest : String -> Cmd Msg
registerGuest homeserverUrl =
    Http.post
        { url =
            clientServerEndpoint
                homeserverUrl
                [ "register" ]
                [ Url.Builder.string "kind" "guest" ]
        , body = Http.stringBody "application/json" "{}"
        , expect = Http.expectJson RegisteredGuest decodeRegistration
        }


joinRoom : String -> String -> String -> Cmd Msg
joinRoom homeserverUrl accessToken roomAlias =
    Http.request
        { method = "POST"
        , url = clientServerEndpoint homeserverUrl [ "join", roomAlias ] []
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.stringBody "application/json" "{}"
        , expect = Http.expectJson JoinedRoom decodeRoomId

        -- TODO: set these?
        , timeout = Nothing
        , tracker = Nothing
        }


syncClient : String -> String -> String -> Maybe String -> Cmd Msg
syncClient homeserverUrl accessToken roomId since =
    let
        timeout =
            5000

        sinceParams : List QueryParameter
        sinceParams =
            case since of
                Nothing ->
                    []

                Just sinceStr ->
                    [ Url.Builder.string "since" sinceStr ]
    in
    Http.request
        { method = "GET"
        , url =
            clientServerEndpoint
                homeserverUrl
                [ "sync" ]
                (Url.Builder.int "timeout" 3000 :: sinceParams)
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.emptyBody
        , expect = Http.expectJson GotSync <| decodeSyncResponse roomId

        -- TODO: set these?
        , timeout = Nothing
        , tracker = Nothing
        }


serverNameFromUserId : String -> Maybe String
serverNameFromUserId userId =
    userId
        |> String.split ":"
        |> List.drop 1
        |> List.head



-- DECODERS


decodeRegistration : JD.Decoder RegisterResponse
decodeRegistration =
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


decodeRoomId : JD.Decoder String
decodeRoomId =
    JD.field "room_id" JD.string


decodeSyncResponse : String -> JD.Decoder SyncResponse
decodeSyncResponse roomId =
    JD.map2
        (\room nextBatch -> { room = room, nextBatch = nextBatch })
        (JD.field "rooms" <| JD.field "join" <| JD.field roomId <| decodeRoom)
        (JD.field "next_batch" JD.string)


decodeRoom : JD.Decoder { timeline : SyncTimeline }
decodeRoom =
    JD.map
        (\t -> { timeline = t })
        (JD.field "timeline" decodeTimeline)


decodeTimeline : JD.Decoder SyncTimeline
decodeTimeline =
    JD.map2
        (\prevBatch events -> { prevBatch = prevBatch, events = events })
        (JD.field "prev_batch" JD.string)
        (JD.field "events" <| JD.list decodeRoomEvent)


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
          h1 [] <|
            case model.error of
                Nothing ->
                    []

                Just errmsg ->
                    [ text <| "ERROR: " ++ errmsg ]
        , -- show MessageEvents
          case model.roomState of
            NoAccess ->
                p [] [ text "Creating guest user..." ]

            Registered _ ->
                p [] [ text "Getting comments..." ]

            Joined joinedRoom ->
                viewRoomEvents joinedRoom.roomEvents
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
                    "unsupported event"
    in
    div []
        [ p [] [ text messageEvent.sender ]
        , p [] [ text textBody ]
        ]
