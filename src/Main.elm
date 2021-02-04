module Main exposing (main)

import Accessibility exposing (Html, button, div, h5, p, text)
import ApiUtils exposing (makeRoomAlias)
import Browser
import Dict exposing (Dict)
import Editor exposing (joinPutLeave, joinRoom, putMessage, viewEditor)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)
import Member exposing (Member)
import Message exposing (GetMessagesResponse, Message(..), RoomEvent, messageEvents)
import Room exposing (Room, commentCount, getInitialRoom, getNewerMessages, getOlderMessages, mergeNewMessages, viewRoom)
import Session exposing (Kind(..), Session, decodeStoredSession, getHomeserverUrl, incrementTransactionId, registerGuest, sessionKind, storeSessionCmd)
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
    , editorContent : String
    , session : Maybe Session
    , room : Maybe Room
    , loginForm : Maybe LoginForm
    , showComments : Int
    , gotAllComments : Bool
    , error : Maybe String
    }


type alias Flags =
    { defaultHomeserverUrl : String
    , serverName : String
    , siteName : String
    , commentSectionId : String
    , storedSession : JD.Value
    , pageSize : Int
    }


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , roomAlias : String
    , pageSize : Int
    }


parseFlags : Flags -> ( StaticConfig, Maybe Session )
parseFlags flags =
    ( StaticConfig flags.defaultHomeserverUrl (makeRoomAlias flags) flags.pageSize
    , JD.decodeValue decodeStoredSession flags.storedSession
        |> Result.toMaybe
    )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( config, session ) =
            parseFlags flags
    in
    ( { config = config
      , editorContent = ""
      , session = session
      , room = Nothing
      , loginForm = Nothing
      , showComments = config.pageSize
      , gotAllComments = False
      , error = Nothing
      }
    , -- first GET to the matrix room, as Guest or User
      Task.attempt GotRoom <|
        case session of
            -- if no localstorage session was found
            -- then register a guest user w/ default homeserver
            Nothing ->
                registerGuest config.defaultHomeserverUrl
                    |> Task.andThen
                        (\sess -> getInitialRoom sess config.roomAlias |> Task.map (Tuple.pair sess))

            -- otherwise, use stored session
            Just sess ->
                joinIfUser sess config.roomAlias
                    |> Task.andThen
                        (\_ -> getInitialRoom sess config.roomAlias |> Task.map (Tuple.pair sess))
    )


joinIfUser : Session -> String -> Task.Task Session.Error ()
joinIfUser session roomAlias =
    -- make sure Users are always joined
    case sessionKind session of
        User ->
            joinRoom session roomAlias

        Guest ->
            -- Guests don't need to be joined
            Task.succeed ()


type Msg
    = GotRoom (Result Session.Error ( Session, Room ))
    | ViewMore Session Room
    | GotMessages Session Room (Result Session.Error GetMessagesResponse)
      -- EDITOR
    | EditComment String
    | SendComment Session Room
    | SentComment Session Room (Result Session.Error ())
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
                Task.attempt (GotMessages session room) <|
                    getOlderMessages session room

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

        GotMessages session room (Ok newMsgs) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    mergeNewMessages room newMsgs

                newModel =
                    { model
                        | room = Just <| newRoom
                        , gotAllComments = newMsgs.chunk == []
                    }
            in
            ( newModel
            , if not newModel.gotAllComments then
                fillComments session newRoom

              else
                Cmd.none
            )

        GotMessages _ _ (Err (Session.Error code error)) ->
            -- http error while getting more comments
            ( { model | error = Just <| code ++ error }
            , Cmd.none
            )

        ViewMore session room ->
            -- "view more" button hit, increase count of comments to show,
            -- and issue request to fetch more messages
            let
                newShowComments =
                    model.showComments + model.config.pageSize
            in
            ( { model | showComments = newShowComments }
            , Task.attempt (GotMessages session room) <|
                getOlderMessages session room
            )

        EditComment str ->
            -- user changes text in comment box
            ( { model | editorContent = str }
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
                            -- join room, HTTP PUT comment, leave room
                            joinPutLeave

                        User ->
                            -- user is already joined - leave room
                            putMessage
            in
            ( { model
                | editorContent = ""
                , session = Just newSession
                , room = Just room
              }
            , Cmd.batch
                [ -- send message
                  Task.attempt (SentComment session room) <|
                    putTask session room.roomId model.editorContent

                -- store session with updated txnId
                , storeSessionCmd newSession
                ]
            )

        SentComment session room (Ok ()) ->
            ( model
            , Task.attempt (GotMessages session room) <|
                getNewerMessages session room
            )

        SentComment session room (Err (Session.Error code error)) ->
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
                            joinRoom session model.config.roomAlias
                                |> Task.andThen
                                    (\_ ->
                                        getInitialRoom session model.config.roomAlias
                                            |> Task.map (\room -> ( session, room ))
                                    )
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
                , editorContent = model.editorContent
                }
    in
    div [ class "cactus-container" ] <|
        [ errors
        , loginPopup
        , editor
        , case ( model.room, model.session ) of
            ( Just room, Just session ) ->
                div []
                    [ viewRoom room (getHomeserverUrl session) model.showComments
                    , if not model.gotAllComments then
                        viewMoreButton <| ViewMore session room

                      else
                        text ""
                    ]

            _ ->
                p [] [ text "Getting comments..." ]
        ]


viewMoreButton : msg -> Html msg
viewMoreButton msg =
    div [ class "cactus-view-more" ]
        [ button
            [ class "cactus-button"
            , onClick msg
            ]
            [ text "View more" ]
        ]
