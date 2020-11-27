module Main exposing (main)

import Accessibility exposing (Html, button, div, h5, p, text)
import ApiUtils exposing (makeRoomAlias)
import Browser
import Dict exposing (Dict)
import Editor exposing (Editor, joinPut, joinPutLeave, viewEditor)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)
import Member exposing (Member)
import Message exposing (GetMessagesResponse, Message(..), RoomEvent, messageEvents, viewMessageEvent)
import Room exposing (Room, commentCount, getInitialRoom, getMoreMessages, mergeNewMessages)
import Session exposing (Kind(..), Session, decodeStoredSession, incrementTransactionId, registerGuest, sessionKind, storeSessionCmd)
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { config : StaticConfig
    , editor : Editor
    , session : Maybe Session
    , room : Maybe Room
    , loginForm : Maybe LoginForm
    , showComments : Int
    , error : Maybe String
    }


type alias Flags =
    { defaultHomeserverUrl : String
    , serverName : String
    , siteName : String
    , commentSectionId : String
    , storedSession : JD.Value
    }


parseFlags : Flags -> ( StaticConfig, Maybe Session )
parseFlags flags =
    -- TODO: remove magic 5
    ( StaticConfig flags.defaultHomeserverUrl (makeRoomAlias flags) 5
    , JD.decodeValue decodeStoredSession flags.storedSession
        |> Result.toMaybe
    )


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , roomAlias : String
    , pageSize : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( config, session ) =
            parseFlags flags
    in
    ( { config = config
      , editor = { content = "" }
      , session = session
      , room = Nothing
      , loginForm = Nothing
      , showComments = config.pageSize
      , error = Nothing
      }
    , -- first GET to the matrix room
      Task.attempt GotRoom <|
        case session of
            -- register guest if no localstorage session was found
            Nothing ->
                registerGuest config.defaultHomeserverUrl
                    |> Task.andThen
                        (\sess -> getInitialRoom sess config.roomAlias |> Task.map (Tuple.pair sess))

            -- use stored session
            Just sess ->
                getInitialRoom sess config.roomAlias
                    |> Task.map (Tuple.pair sess)
    )


type Msg
    = GotRoom (Result Session.Error ( Session, Room ))
    | ViewMore Session Room
    | GotMessages Session Room (Result Session.Error GetMessagesResponse)
      -- EDITOR
    | EditComment String
    | SendComment Session Room
    | SentComment (Result Session.Error ())
      -- LOGIN
    | ShowLogin
    | HideLogin
    | EditLogin LoginForm
    | Login LoginForm
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        {- Get more comments if needed -}
        fillComments session room =
            if commentCount room < model.showComments then
                getMoreMessages (GotMessages session room) session room

            else
                Cmd.none
    in
    case msg of
        GotRoom (Ok ( session, room )) ->
            -- got initial room, when loading page first ime
            ( { model
                | session = Just session
                , room = Just room
                , loginForm = Nothing
              }
            , Cmd.batch <|
                [ storeSessionCmd session
                , fillComments session room
                ]
            )

        GotRoom (Err (Session.Error code error)) ->
            -- error while setting up initial room
            ( { model | error = Just <| code ++ error }
            , Cmd.none
            )

        ViewMore session room ->
            -- "view more" button hit, increase count of comments to show,
            -- and issue request to fetch more messages
            ( { model | showComments = model.showComments + model.config.pageSize }
            , getMoreMessages (GotMessages session room) session room
            )

        GotMessages session room (Ok newMsgs) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    mergeNewMessages room newMsgs
            in
            ( { model | room = Just <| newRoom }
            , fillComments session newRoom
            )

        GotMessages _ _ (Err (Session.Error code error)) ->
            -- http error while getting more comments
            ( { model | error = Just <| code ++ error }
            , Cmd.none
            )

        EditComment str ->
            -- user changes text in comment box
            ( { model | editor = { content = str } }
            , Cmd.none
            )

        SendComment session room ->
            -- user hit send button
            let
                -- increment transaction Id (idempotency measure)
                newSession =
                    incrementTransactionId session

                putTask =
                    case sessionKind session of
                        Guest ->
                            joinPutLeave

                        User ->
                            joinPut
            in
            ( { model
                | editor = { content = "" }
                , session = Just newSession
                , room = Just room
              }
            , Cmd.batch
                [ Task.attempt SentComment <|
                    putTask
                        session
                        room.roomId
                        model.editor.content

                -- store session with updated txnId
                , storeSessionCmd newSession
                ]
            )

        SentComment (Ok ()) ->
            ( model
              -- TODO: fetch messages again
            , Cmd.none
            )

        SentComment (Err (Session.Error code error)) ->
            ( { model | error = Just <| code ++ error }
            , Cmd.none
            )

        ShowLogin ->
            ( { model | loginForm = Just initLoginForm }
            , Cmd.none
            )

        HideLogin ->
            ( { model | loginForm = Nothing }
            , Cmd.none
            )

        EditLogin form ->
            ( { model | loginForm = Just form }
            , Cmd.none
            )

        LogOut ->
            ( model
            , Task.attempt GotRoom <|
                (registerGuest model.config.defaultHomeserverUrl
                    |> Task.andThen
                        (\sess ->
                            getInitialRoom sess model.config.roomAlias
                                |> Task.map (Tuple.pair sess)
                        )
                )
            )

        Login form ->
            let
                ( newForm, loginTask ) =
                    loginWithForm form
            in
            ( { model | loginForm = Just newForm }
            , Task.attempt GotRoom <|
                (loginTask
                    |> Task.andThen
                        (\session ->
                            getInitialRoom session model.config.roomAlias
                                |> Task.map (\room -> ( session, room ))
                        )
                )
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        errors =
            h5 [] <|
                case model.error of
                    Nothing ->
                        []

                    Just errmsg ->
                        [ text <| "ERROR: " ++ errmsg ]

        loginPopup =
            case model.loginForm of
                Just loginForm ->
                    viewLoginForm loginForm { submitMsg = Login, hideMsg = HideLogin, editMsg = EditLogin }

                Nothing ->
                    text ""

        editor =
            viewEditor
                { showLoginMsg = ShowLogin
                , logoutMsg = LogOut
                , editMsg = EditComment
                , sendMsg = Maybe.map2 SendComment model.session model.room
                , session = model.session
                , roomAlias = model.config.roomAlias
                , editor = model.editor
                }
    in
    div [ class "cactus-container" ] <|
        [ errors
        , loginPopup
        , editor
        , case ( model.room, model.session ) of
            ( Just room, Just session ) ->
                div []
                    [ viewRoomEvents
                        model.config.defaultHomeserverUrl
                        room.time
                        room.members
                        room.events
                    , viewMoreButton session room
                    ]

            _ ->
                p [] [ text "Getting comments..." ]
        ]


viewRoomEvents : String -> Time.Posix -> Dict String Member -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl time members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl time members)
            (messageEvents roomEvents)


viewMoreButton : Session -> Room -> Html Msg
viewMoreButton auth room =
    div [ class "cactus-view-more" ]
        [ button
            [ class "cactus-button"
            , onClick <| ViewMore auth room
            ]
            [ text "View more" ]
        ]
