module TestEditor exposing (..)

import Editor exposing (viewEditor)
import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, href)
import Json.Decode as JD
import Session
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type NoOp
    = NoOp
    | NoOpStr String


guestSession : Result JD.Error Session.Session
guestSession =
    """
        {
          "homeserverUrl": "https://example.com:8448",
          "kind": "guest",
          "txnId": 0,
          "userId": "@1234:example.com",
          "accessToken": "abcdVerySecret"
        }
    """
        |> JD.decodeString Session.decodeStoredSession


userSession : Result JD.Error Session.Session
userSession =
    """
        {
          "homeserverUrl": "https://example.com:8448",
          "kind": "user",
          "txnId": 0,
          "userId": "@1234:example.com",
          "accessToken": "abcdVerySecret"
        }
    """
        |> JD.decodeString Session.decodeStoredSession


viewEditorHelper : { loginEnabled : Bool, guestPostingEnabled : Bool, session : Maybe Session.Session } -> Html NoOp
viewEditorHelper { loginEnabled, guestPostingEnabled, session } =
    viewEditor
        { showLoginMsg = NoOp
        , logoutMsg = NoOp
        , editMsg = NoOpStr
        , sendMsg = Nothing
        , session = session
        , roomAlias = "#room:alias"
        , editorContent = "Hello, world!"
        , loginEnabled = loginEnabled
        , guestPostingEnabled = guestPostingEnabled
        }


testGuestPostingToggle : Test
testGuestPostingToggle =
    describe "Test guestPostingEnabled" <|
        [ test "Test guestPostingEnabled=True" <|
            \_ ->
                guestSession
                    |> Result.map
                        (\sess ->
                            viewEditorHelper { loginEnabled = True, guestPostingEnabled = True, session = Just sess }
                                |> Query.fromHtml
                                |> Query.find [ Selector.tag "textarea" ]
                                |> Query.has [ Selector.attribute (disabled False) ]
                        )
                    |> Result.withDefault (Expect.fail "Session did not decode")
        , test "Test guestPostingEnabled=False" <|
            Expect.all
                [ \_ ->
                    -- guests can't comment
                    guestSession
                        |> Result.map
                            (\sess ->
                                viewEditorHelper { loginEnabled = True, guestPostingEnabled = False, session = Just sess }
                                    |> Query.fromHtml
                                    |> Query.find [ Selector.tag "textarea" ]
                                    |> Query.has [ Selector.attribute (disabled True) ]
                            )
                        |> Result.withDefault (Expect.fail "Session did not decode")

                -- users can comment
                , \_ ->
                    userSession
                        |> Result.map
                            (\sess ->
                                viewEditorHelper { loginEnabled = True, guestPostingEnabled = False, session = Just sess }
                                    |> Query.fromHtml
                                    |> Query.find [ Selector.tag "textarea" ]
                                    |> Query.has [ Selector.attribute (disabled False) ]
                            )
                        |> Result.withDefault (Expect.fail "Session did not decode")

                -- unauthenticated users can do nothing
                , \_ ->
                    viewEditorHelper { loginEnabled = True, guestPostingEnabled = False, session = Nothing }
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "textarea" ]
                        |> Query.has [ Selector.attribute (disabled True) ]
                ]
        ]


testLoginDisabled : Test
testLoginDisabled =
    test "Test loginEnabled=False" <|
        \_ ->
            viewEditorHelper { loginEnabled = False, guestPostingEnabled = True, session = Nothing }
                |> Query.fromHtml
                |> Query.find [ Selector.tag "a" ]
                |> Query.has [ Selector.attribute (href "https://matrix.to/#/%23room%3Aalias") ]


testBothDisabled : Test
testBothDisabled =
    test "Test loginEnabled=False, guestPostingEnabled=False" <|
        \_ ->
            viewEditorHelper { loginEnabled = False, guestPostingEnabled = False, session = Nothing }
                |> Query.fromHtml
                |> Query.find [ Selector.tag "button", Selector.class "cactus-button" ]
                |> Query.has [ Selector.class "cactus-matrixdotto-only" ]
