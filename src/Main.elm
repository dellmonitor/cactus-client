module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

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
    { -- provided config
      config : StaticConfig
    , roomAlias : Maybe String

    -- internal state
    , accessToken : Maybe String
    , roomId : Maybe String

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
      , roomAlias = Nothing
      , accessToken = Nothing
      , roomId = Nothing
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
    case msg of
        RegisteredGuest (Ok response) ->
            let
                roomAlias =
                    makeRoomAlias
                        model.config.siteName
                        model.config.uniqueId
                        response.serverName
            in
            ( { model
                | accessToken = Just response.accessToken
                , roomAlias = Just roomAlias
              }
            , joinRoom
                model.config.defaultHomeserverUrl
                response.accessToken
                roomAlias
            )

        JoinedRoom (Ok roomId) ->
            ( { model | roomId = Just roomId }
            , case model.accessToken of
                Just accessToken ->
                    syncClient
                        model.config.defaultHomeserverUrl
                        accessToken
                        roomId

                _ ->
                    -- TODO: error better
                    Debug.log "Couldn't get access token" Cmd.none
            )

        GotSync (Ok syncResponse) ->
            -- XXX DEBUG
            ( { model | error = Just <| Debug.toString syncResponse }, Cmd.none )

        GotSync (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        RegisteredGuest (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        JoinedRoom (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Matrix Client-Server API


type alias RegisterResponse =
    { userId : String
    , serverName : String
    , accessToken : String
    }


type alias SyncResponse =
    { rooms : { join : { timeline : SyncTimeline } }
    , nextBatch : String
    }


type alias SyncTimeline =
    { prevBatch : String
    , events : List RoomEvent
    }


type RoomEvent
    = MessageEvent { content : Message, sender : String, origin_server_ts : Int }
    | UnsupportedRoomEvent String


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


syncClient : String -> String -> String -> Cmd Msg
syncClient homeserverUrl accessToken roomId =
    Http.request
        { method = "GET"
        , url = clientServerEndpoint homeserverUrl [ "sync" ] []
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
        (\rooms nextBatch -> { rooms = rooms, nextBatch = nextBatch })
        (JD.field "rooms" <| decodeRooms roomId)
        (JD.field "next_batch" JD.string)


decodeRooms : String -> JD.Decoder { join : { timeline : SyncTimeline } }
decodeRooms roomId =
    JD.map
        (\join -> { join = join })
        (JD.field "join" <| decodeJoinedRoom roomId)


decodeJoinedRoom : String -> JD.Decoder { timeline : SyncTimeline }
decodeJoinedRoom roomId =
    JD.field roomId <|
        JD.map
            (\timeline -> { timeline = timeline })
            (JD.field "timeline" decodeTimeline)


decodeTimeline : JD.Decoder SyncTimeline
decodeTimeline =
    JD.map2
        (\prevBatch events -> { prevBatch = prevBatch, events = events })
        (JD.field "prev_batch" JD.string)
        (JD.field "events" <| JD.list decodeRoomEvent)


decodeRoomEvent : JD.Decoder RoomEvent
decodeRoomEvent =
    JD.field "type" JD.string
        |> JD.andThen
            (\eventType ->
                case eventType of
                    "m.room.message" ->
                        JD.map3
                            (\msg s ots -> MessageEvent { content = msg, sender = s, origin_server_ts = ots })
                            (JD.field "content" decodeMessage)
                            (JD.field "sender" JD.string)
                            (JD.field "origin_server_ts" JD.int)

                    _ ->
                        JD.succeed (UnsupportedRoomEvent eventType)
            )


decodeMessage : JD.Decoder Message
decodeMessage =
    JD.field "msgtype" JD.string
        |> JD.andThen
            (\mt ->
                case mt of
                    "m.text" ->
                        JD.map
                            (\body -> Text { body = body, format = Nothing, formatted_body = Nothing })
                            (JD.field "body" JD.string)

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
    div []
        [ -- view errors
          case model.error of
            Nothing ->
                text "no error"

            Just errmsg ->
                h1 [] [ text <| "ERROR: " ++ errmsg ]
        ]
