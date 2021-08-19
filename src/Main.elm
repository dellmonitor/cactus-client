module Main exposing (main)

import Accessibility exposing (Html, button, div, p, text)
import Accessibility.Aria exposing (errorMessage)
import Browser
import Config exposing (StaticConfig, parseConfig)
import Duration
import Editor exposing (Editor)
import Event exposing (GetMessagesResponse)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Json.Decode as JD
import LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)
import Member exposing (setDisplayname)
import Message exposing (Message(..))
import Room
    exposing
        ( Direction(..)
        , Room
        , RoomId
        , commentCount
        , extractRoomId
        , getInitialRoom
        , getNewerMessages
        , getOlderMessages
        , joinRoom
        , mergeMessages
        , sendComment
        , viewRoomEvents
        )
import Session
    exposing
        ( Kind(..)
        , Session
        , getHomeserverUrl
        , registerGuest
        , sessionKind
        , storeSessionCmd
        )
import Task
import Time


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = BadConfig String
    | GoodConfig
        { config : StaticConfig
        , editor : Editor
        , session : Maybe Session
        , room : Maybe Room
        , loginForm : Maybe LoginForm
        , showComments : Int
        , gotAllComments : Bool
        , errors : List Error
        , now : Time.Posix
        }


subscriptions : Model -> Sub Msg
subscriptions m =
    case m of
        GoodConfig model ->
            let
                updatems =
                    Duration.inMilliseconds model.config.updateInterval
            in
            if updatems > 0 then
                Time.every updatems Tick

            else
                Sub.none

        _ ->
            Sub.none


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        parsedFlags : Result JD.Error ( StaticConfig, Maybe Session )
        parsedFlags =
            parseConfig flags
    in
    case parsedFlags of
        Ok ( config, session ) ->
            ( GoodConfig
                { config = config
                , editor = Editor.init
                , session = session
                , room = Nothing
                , loginForm = Nothing
                , showComments = config.pageSize
                , gotAllComments = False
                , errors = []
                , now = Time.millisToPosix 0 -- shitty default value
                }
            , Cmd.batch
                [ -- get current time
                  Task.perform Tick Time.now
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
                ]
            )

        Err err ->
            ( BadConfig <| JD.errorToString err
            , Cmd.none
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
    = Tick Time.Posix
    | CloseError Int
      -- LoginForm
    | EditLogin LoginForm
    | ShowLogin
    | HideLogin
      -- Editor
    | EditorMsg Editor.Msg
      -- Message Fetching
    | GotRoom (Result Session.Error ( Session, Room ))
    | GotMessages Session Direction (Result Session.Error GetMessagesResponse)
    | ViewMore Session Room
    | SendComment Session RoomId
    | SentComment Session (Result Session.Error ())
    | Login LoginForm
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case model_ of
        BadConfig _ ->
            ( model_, Cmd.none )

        GoodConfig model ->
            let
                {- Get more comments if needed -}
                fillComments session room =
                    if commentCount room < model.showComments then
                        Task.attempt (GotMessages session Older) <|
                            getOlderMessages session room

                    else
                        Cmd.none
            in
            Tuple.mapFirst GoodConfig <|
                case msg of
                    Tick now ->
                        ( -- update time
                          { model | now = now }
                          -- get more messages on each tick
                        , Maybe.map2
                            (\s r ->
                                getNewerMessages s r
                                    |> Task.attempt (GotMessages s Newer)
                            )
                            model.session
                            model.room
                            |> Maybe.withDefault Cmd.none
                        )

                    CloseError id ->
                        ( { model | errors = List.filter (\e -> e.id /= id) model.errors }
                        , Cmd.none
                        )

                    EditorMsg m ->
                        ( { model | editor = Editor.update m model.editor }, Cmd.none )

                    GotRoom (Ok ( session, room )) ->
                        -- got initial room, when loading page first time
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
                        ( { model | errors = addError model.errors <| code ++ " " ++ error }
                        , Cmd.none
                        )

                    GotMessages session dir (Ok newMsgs) ->
                        -- got more messages. result of the "ViewMore"-button request
                        let
                            newRoom : Maybe Room
                            newRoom =
                                Maybe.map
                                    (\r -> mergeMessages r dir newMsgs)
                                    model.room

                            newModel =
                                { model
                                    | room = newRoom
                                    , gotAllComments = List.isEmpty newMsgs.chunk && dir == Older
                                }
                        in
                        ( newModel
                        , case ( newRoom, newModel.gotAllComments ) of
                            ( Just r, False ) ->
                                -- we have room, and still want to backfill more comments
                                fillComments session r

                            _ ->
                                Cmd.none
                        )

                    GotMessages _ _ (Err (Session.Error code error)) ->
                        -- http error while getting more comments
                        ( { model | errors = addError model.errors <| code ++ " " ++ error }
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
                        , Task.attempt (GotMessages session Older) <|
                            getOlderMessages session room
                        )

                    SendComment session roomId ->
                        -- user hit send button
                        let
                            ( sendTask, newSession ) =
                                sendComment session roomId <| Editor.getComment model.editor
                        in
                        ( { model
                            | editor = Editor.clear model.editor
                            , session = Just newSession
                          }
                        , Cmd.batch
                            [ -- send message
                              Task.attempt (SentComment session) <|
                                case sessionKind session of
                                    User ->
                                        sendTask

                                    Guest ->
                                        setDisplayname session (Editor.getName model.editor)
                                            |> Task.andThen (\() -> sendTask)

                            -- store session with updated txnId
                            , storeSessionCmd newSession
                            ]
                        )

                    SentComment session (Ok ()) ->
                        ( model
                        , model.room
                            |> Maybe.map
                                (getNewerMessages session >> Task.attempt (GotMessages session Newer))
                            |> Maybe.withDefault Cmd.none
                        )

                    SentComment _ (Err (Session.Error code error)) ->
                        ( { model | errors = addError model.errors <| code ++ " " ++ error }
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



-- ERRORS


type alias Error =
    { id : Int
    , message : String
    }


addError : List Error -> String -> List Error
addError errors message =
    { id =
        List.map .id errors
            |> List.maximum
            |> Maybe.map ((+) 1)
            |> Maybe.withDefault 0
    , message = message
    }
        :: errors


viewError : Error -> Html Msg
viewError { id, message } =
    div [ class "cactus-error", errorMessage message ]
        [ button
            [ class "cactus-error-close"
            , attribute "aria-label" "close"
            , onClick <| CloseError id
            ]
            [ text "×" ]
        , p [ class "cactus-error-text"]
            [ text <| " Error: " ++ message ]
        ]



-- VIEW


view : Model -> Html Msg
view model_ =
    case model_ of
        BadConfig err ->
            viewError { id = 0, message = "Bad configuration: " ++ err }

        GoodConfig model ->
            let
                errors =
                    div [] <| List.map viewError model.errors

                loginPopup =
                    case model.loginForm of
                        Just loginForm ->
                            viewLoginForm loginForm
                                model.config.roomAlias
                                { submitMsg = Login
                                , hideMsg = HideLogin
                                , editMsg = EditLogin
                                }

                        Nothing ->
                            text ""

                editor =
                    Editor.view
                        model.editor
                        { session = model.session
                        , roomAlias = model.config.roomAlias
                        , loginEnabled = model.config.loginEnabled
                        , guestPostingEnabled = model.config.guestPostingEnabled
                        , msgmap = EditorMsg
                        , showLogin = ShowLogin
                        , logout = LogOut
                        , send = Maybe.map2 SendComment model.session <| Maybe.map extractRoomId model.room
                        }
            in
            div [ class "cactus-container" ] <|
                [ errors
                , loginPopup
                , editor
                , case ( model.room, model.session ) of
                    ( Just room, Just session ) ->
                        div [ class "cactus-comments-container" ]
                            [ viewRoomEvents
                                (getHomeserverUrl session)
                                room
                                model.showComments
                                model.now
                            , if model.gotAllComments then
                                text ""

                              else
                                -- "View More" button
                                div [ class "cactus-view-more" ]
                                    [ button
                                        [ class "cactus-button"
                                        , onClick <| ViewMore session room
                                        ]
                                        [ text "View more" ]
                                    ]
                            ]

                    _ ->
                        p [] [ text "Getting comments..." ]
                ]
