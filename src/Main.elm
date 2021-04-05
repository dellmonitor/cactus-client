module Main exposing (..)

import Accessibility exposing (Html, b, button, div, p, text)
import Accessibility.Aria exposing (errorMessage)
import ApiUtils exposing (makeRoomAlias)
import Browser
import Editor exposing (viewEditor)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as JD
import LoginForm exposing (FormState(..), LoginForm, initLoginForm, loginWithForm, viewLoginForm)
import Message exposing (GetMessagesResponse, Message(..))
import Room
    exposing
        ( Direction(..)
        , Room
        , commentCount
        , getInitialRoom
        , getNewerMessages
        , getOlderMessages
        , joinRoom
        , mergeNewerMessages
        , mergeOlderMessages
        , sendComment
        , viewRoomEvents
        )
import Session
    exposing
        ( Kind(..)
        , Session
        , decodeStoredSession
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
        , subscriptions = \_ -> Time.every 5000 Tick
        }


type alias Model =
    { config : StaticConfig
    , editorContent : String
    , session : Maybe Session
    , room : Maybe Room
    , loginForm : Maybe LoginForm
    , showComments : Int
    , gotAllComments : Bool
    , errors : List Error
    , now : Time.Posix
    }


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        parsedFlags : Result JD.Error ( StaticConfig, Maybe Session )
        parsedFlags =
            flags
                |> JD.decodeValue decodeFlags
                |> Result.map parseFlags

        config =
            parsedFlags
                |> Result.map Tuple.first
                -- TODO get rid of this dumb default
                |> Result.withDefault (StaticConfig "" "" 10 True)

        session =
            parsedFlags
                |> Result.map Tuple.second
                |> Result.withDefault Nothing
    in
    ( { config = config
      , editorContent = ""
      , session = session
      , room = Nothing
      , loginForm = Nothing
      , showComments = config.pageSize
      , gotAllComments = False
      , errors =
            case parsedFlags of
                Ok _ ->
                    []

                Err err ->
                    [ { id = 0, message = JD.errorToString err } ]
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
      -- Message Fetching
    | GotRoom (Result Session.Error ( Session, Room ))
    | ViewMore Session Room
    | GotMessages Session Room Direction (Result Session.Error GetMessagesResponse)
      -- Editor
    | EditComment String
    | SendComment Session Room
    | SentComment Session Room (Result Session.Error ())
      -- Login
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
                Task.attempt (GotMessages session room Older) <|
                    getOlderMessages session room

            else
                Cmd.none
    in
    case msg of
        Tick now ->
            ( { model | now = now }, Cmd.none )

        CloseError id ->
            ( { model | errors = List.filter (\e -> e.id /= id) model.errors }
            , Cmd.none
            )

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

        GotMessages session room dir (Ok newMsgs) ->
            -- got more messages. result of the "ViewMore"-button request
            let
                newRoom =
                    (case dir of
                        Newer ->
                            mergeNewerMessages

                        Older ->
                            mergeOlderMessages
                    )
                        room
                        newMsgs

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

        GotMessages _ _ _ (Err (Session.Error code error)) ->
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
            , Task.attempt (GotMessages session room Older) <|
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
                ( sendTask, newSession ) =
                    sendComment session room model.editorContent
            in
            ( { model
                | editorContent = ""
                , session = Just newSession
                , room = Just room
              }
            , Cmd.batch
                [ -- send message
                  Task.attempt (SentComment session room) sendTask

                -- store session with updated txnId
                , storeSessionCmd newSession
                ]
            )

        SentComment session room (Ok ()) ->
            ( model
            , Task.attempt (GotMessages session room Newer) <|
                getNewerMessages session room
            )

        SentComment _ _ (Err (Session.Error code error)) ->
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



-- CONFIG


type alias Flags =
    { defaultHomeserverUrl : String
    , serverName : String
    , siteName : String
    , commentSectionId : String
    , storedSession : Maybe Session
    , pageSize : Maybe Int
    , loginEnabled : Maybe Bool
    }


type alias StaticConfig =
    { defaultHomeserverUrl : String
    , roomAlias : String
    , pageSize : Int
    , loginEnabled : Bool
    }


parseFlags : Flags -> ( StaticConfig, Maybe Session )
parseFlags flags =
    ( StaticConfig
        flags.defaultHomeserverUrl
        (makeRoomAlias flags)
        (flags.pageSize |> Maybe.withDefault 10)
        (flags.loginEnabled |> Maybe.withDefault True)
    , flags.storedSession
    )


decodeFlags : JD.Decoder Flags
decodeFlags =
    JD.map7 Flags
        (JD.field "defaultHomeserverUrl" JD.string)
        (JD.field "serverName" JD.string)
        (JD.field "siteName" JD.string)
        (JD.field "commentSectionId" decodeCommentSectionId)
        (JD.field "storedSession" <| JD.nullable decodeStoredSession)
        (JD.maybe <| JD.field "pageSize" JD.int)
        (JD.maybe <| JD.field "loginEnabled" JD.bool)


decodeCommentSectionId : JD.Decoder String
decodeCommentSectionId =
    JD.string
        |> JD.andThen
            (\csid ->
                if String.contains "_" csid then
                    JD.fail "commentSectionId can't contain underscores"

                else
                    JD.succeed csid
            )
        |> JD.andThen
            (\csid ->
                if String.contains " " csid then
                    JD.fail "commentSectionId can't contain spaces"

                else
                    JD.succeed csid
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



-- VIEW


view : Model -> Html Msg
view model =
    let
        errors =
            div [] <|
                List.map
                    (\{ id, message } ->
                        div [ class "cactus-error", errorMessage message ]
                            [ button
                                [ class "cactus-button"
                                , onClick <| CloseError id
                                ]
                                [ text "X" ]
                            , b [] [ text <| " Error: " ++ message ]
                            ]
                    )
                    model.errors

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
            viewEditor
                { showLoginMsg = ShowLogin
                , logoutMsg = LogOut
                , editMsg = EditComment
                , sendMsg = Maybe.map2 SendComment model.session model.room
                , session = model.session
                , roomAlias = model.config.roomAlias
                , editorContent = model.editorContent
                , loginEnabled = model.config.loginEnabled
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
