module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { defaultHomeserverUrl : String
    , accessToken : Maybe String
    , roomAlias : String
    , roomId : Maybe String
    , error : Maybe String
    }


init : { defaultHomeserverUrl : String } -> ( Model, Cmd Msg )
init { defaultHomeserverUrl } =
    ( { accessToken = Nothing
      , defaultHomeserverUrl = defaultHomeserverUrl
      , roomAlias = "#public_test_room:olli.ng"
      , roomId = Nothing
      , error = Nothing
      }
    , Cmd.batch
        [ guestRegister defaultHomeserverUrl
        , getRoomId defaultHomeserverUrl "%23public_test_room%3Aolli.ng"
        ]
    )



-- UPDATE


type Msg
    = RegisteredGuest (Result Http.Error String)
    | GotRoomId (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisteredGuest (Ok accessToken) ->
            ( { model | accessToken = Just accessToken }, Cmd.none )

        RegisteredGuest (Err _) ->
            -- TODO: display error!
            ( model, Cmd.none )

        GotRoomId (Ok roomId) ->
            ( { model | roomId = Just roomId }, Cmd.none )

        GotRoomId (Err err) ->
            ( { model | error = Just <| Debug.toString err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MATRIX


guestRegister : String -> Cmd Msg
guestRegister homeserverUrl =
    Http.post
        { url = homeserverUrl ++ "/_matrix/client/r0/register?kind=guest"
        , body = Http.stringBody "application/json" "{}"
        , expect = Http.expectJson RegisteredGuest decodeRegistration
        }


getRoomId : String -> String -> Cmd Msg
getRoomId homeserverUrl roomAlias =
    Http.get
        { url = homeserverUrl ++ "/_matrix/client/r0/directory/room/" ++ roomAlias
        , expect = Http.expectJson GotRoomId decodeRoomId
        }


decodeRegistration : JD.Decoder String
decodeRegistration =
    JD.field "access_token" JD.string


decodeRoomId : JD.Decoder String
decodeRoomId =
    JD.field "room_id" JD.string



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ case model.error of
            Nothing ->
                text ""

            Just errmsg ->
                h1 [] [ text <| "ERROR: " ++ errmsg ]
        , h1 [] [ text "Hello, world" ]
        , h1 []
            [ case model.accessToken of
                Just token ->
                    text <| "token! " ++ token

                Nothing ->
                    text "no auth yet"
            ]
        , h1 []
            [ text <| "looking for " ++ model.roomAlias ++ "...\n"
            , case model.roomId of
                Nothing ->
                    text ""

                Just roomId ->
                    text <| "found id! " ++ roomId
            ]
        ]
