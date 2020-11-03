module Main exposing (main)

import Accessibility exposing (Html, button, div, h5, p, text)
import ApiUtils exposing (makeRoomAlias)
import Browser
import Dict exposing (Dict)
import Editor exposing (Editor, joinPut, joinPutLeave, viewEditor)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)
import Member exposing (Member)
import Message exposing (GetMessagesResponse, Message(..), RoomEvent, getMessages, onlyMessageEvents, viewMessageEvent)
import Room exposing (Room, getInitialRoom, getRoomAsGuest, mergeNewMessages)
import Session exposing (Kind(..), Session, incrementTransactionId, sessionKind)
import Task
import Time


main : Program StaticConfig Model Msg
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
    , roomState :
        Maybe
            { room : Room
            , session : Session
            }
    , loginForm : Maybe LoginForm
    , error : Maybe String
    }


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , serverName : String
    , siteName : String
    , uniqueId : String
    }


init : StaticConfig -> ( Model, Cmd Msg )
init config =
    ( { config = config
      , editor = { content = "" }
      , roomState = Nothing
      , loginForm = Nothing
      , error = Nothing
      }
    , Task.attempt GotRoom <|
        getRoomAsGuest
            { homeserverUrl = config.defaultHomeserverUrl
            , roomAlias = makeRoomAlias config
            }
    )


type Msg
    = GotRoom (Result Http.Error ( Session, Room ))
    | ViewMore Session Room
    | GotMessages Room (Result Http.Error GetMessagesResponse)
      -- EDITOR
    | EditComment String
    | SendComment Session Room
    | SentComment (Result Http.Error ())
      -- LOGIN
    | ShowLogin
    | HideLogin
    | EditLogin LoginForm
    | Login LoginForm
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRoom (Ok ( auth, room )) ->
            -- got initial room, when loading page first ime
            ( { model
                | roomState =
                    Just
                        -- init both room and editor. this enables most of the ui
                        { room = room
                        , session = auth
                        }
                , loginForm = Nothing
              }
            , Cmd.none
            )

        GotRoom (Err err) ->
            -- error while setting up initial room
            ( { model | error = Just <| Debug.toString err }
            , Cmd.none
            )

        ViewMore session room ->
            -- "view more" button hit - issue request to fetch more messages
            ( model
            , Task.attempt (GotMessages room) <|
                getMessages session room.roomId room.end
            )

        GotMessages room (Ok newMsgs) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    mergeNewMessages room newMsgs

                newRoomState =
                    Maybe.map
                        (\rs -> { rs | room = newRoom })
                        model.roomState
            in
            ( { model | roomState = newRoomState }
            , Cmd.none
            )

        GotMessages _ (Err httpErr) ->
            -- http error while getting more comments
            ( { model | error = Just <| Debug.toString httpErr }
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

                {- WIP:
                   if guest:
                     joinPutLeave |> andthen getMessages

                   if user:
                     joinPut |> andThen getMessages

                -}
                putTask =
                    case sessionKind session of
                        Guest ->
                            joinPutLeave

                        User ->
                            joinPut
            in
            ( { model
                | editor = { content = "" }
                , roomState = Just { session = newSession, room = room }
              }
            , Task.attempt SentComment <|
                putTask
                    session
                    room.roomId
                    model.editor.content
            )

        SentComment (Ok ()) ->
            ( model
              -- TODO: fetch messages again
            , Cmd.none
            )

        SentComment (Err httpErr) ->
            ( { model | error = Just <| Debug.toString httpErr }
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
                getRoomAsGuest
                    { homeserverUrl = model.config.defaultHomeserverUrl
                    , roomAlias = makeRoomAlias model.config
                    }
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
                            getInitialRoom session (makeRoomAlias model.config)
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
                , sendMsg = Maybe.map (\rs -> SendComment rs.session rs.room) model.roomState
                , session = Maybe.map .session model.roomState
                , roomAlias = makeRoomAlias model.config
                , editor = model.editor
                }
    in
    div [ class "cactus-container" ] <|
        [ errors
        , loginPopup
        , editor
        , case model.roomState of
            Nothing ->
                p [] [ text "Getting comments..." ]

            Just roomState ->
                div []
                    [ viewRoomEvents
                        model.config.defaultHomeserverUrl
                        roomState.room.time
                        roomState.room.members
                        roomState.room.events
                    , viewMoreButton roomState.session roomState.room
                    ]
        ]


viewRoomEvents : String -> Time.Posix -> Dict String Member -> List RoomEvent -> Html Msg
viewRoomEvents defaultHomeserverUrl time members roomEvents =
    div [] <|
        List.map
            (viewMessageEvent defaultHomeserverUrl time members)
            (onlyMessageEvents roomEvents)


viewMoreButton : Session -> Room -> Html Msg
viewMoreButton auth room =
    div [ class "cactus-view-more" ]
        [ button
            [ class "cactus-button"
            , onClick <| ViewMore auth room
            ]
            [ text "View more" ]
        ]
