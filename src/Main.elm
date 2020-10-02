module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as Pipeline


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
              -- TODO /sync
            , Cmd.none
            )

        RegisteredGuest (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )

        JoinedRoom (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MATRIX


type alias RegisterResponse =
    { userId : String
    , serverName : String
    , accessToken : String
    }


makeRoomAlias : String -> String -> String -> String
makeRoomAlias siteName uniqueId serverName =
    "#comments_" ++ siteName ++ "_" ++ uniqueId ++ ":" ++ serverName


registerGuest : String -> Cmd Msg
registerGuest homeserverUrl =
    Http.post
        { url = homeserverUrl ++ "/_matrix/client/r0/register?kind=guest"
        , body = Http.stringBody "application/json" "{}"
        , expect = Http.expectJson RegisteredGuest decodeRegistration
        }


joinRoom : String -> String -> String -> Cmd Msg
joinRoom homeserverUrl accessToken roomAlias =
    Http.request
        { method = "POST"
        , url = homeserverUrl ++ "/_matrix/client/r0/join/" ++ roomAlias
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.stringBody "application/json" "{}"
        , expect = Http.expectJson JoinedRoom decodeRoomId

        -- TODO: set these?
        , timeout = Nothing
        , tracker = Nothing
        }



{--
syncClient : String -> String -> String -> Cmd Msg
syncClient homeserverUrl accessToken roomAlias =
    Http.request
        { method = "GET"
        , url = homeserverUrl ++ "/_matrix/client/r0/sync"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , body = Http.emptyBody
        , expect = Http.expectJson GotSync

        -- TODO: set these?
        , timeout = Nothing
        , tracker = Nothing
        }
--}


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



{-
   decodeSync : JD.Decoder String
   decodeSync =
       JD.succeed SyncResponse
-}
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ -- view errors
          case model.error of
            Nothing ->
                text ""

            Just errmsg ->
                h1 [] [ text <| "ERROR: " ++ errmsg ]

        -- debug: accessToken
        , h1 []
            [ case model.accessToken of
                Nothing ->
                    text "no auth yet"

                Just token ->
                    text <| "token! " ++ token
            ]

        -- debug: roomAlias
        , h1 []
            [ case model.roomAlias of
                Nothing ->
                    text "no alias yet"

                Just roomAlias ->
                    text <| "alias! " ++ roomAlias
            ]
        ]
