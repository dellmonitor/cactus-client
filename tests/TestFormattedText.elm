module TestFormattedText exposing (..)

import Expect exposing (Expectation)
import FormattedText exposing (FormattedText(..), viewFormattedText)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (p)
import Html.Attributes exposing (height, src, width)
import Html.Parser
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Time


suite : Test
suite =
    describe "Formatted text"
        [ testViewFormattedText ]


testViewFormattedText : Test
testViewFormattedText =
    describe "viewFormattedText"
        [ test "Test Plain text" <|
            \_ ->
                (viewFormattedText "https://my.homeserver.tld" <| Plain "just some text")
                    |> Query.fromHtml
                    |> Query.has [ tag "p", text "just some text" ]
        , test "Plain text with script" <|
            \_ ->
                (viewFormattedText "https://my.homeserver.tld" <| Plain "<script>this will not run</script>")
                    |> Query.fromHtml
                    |> Query.has [ tag "p", text "<script>this will not run</script>" ]
        , test "Plain text with escaped script" <|
            \_ ->
                (viewFormattedText "https://my.homeserver.tld" <| Plain "<script>this will not run</script>")
                    |> Query.fromHtml
                    |> Query.hasNot [ tag "script" ]
        , test "Clean image" <|
            let
                cleanImg =
                    Html
                        [ Html.Parser.Element "img"
                            [ ( "width", "200" ), ( "height", "200" ), ( "src", "mxc://olli.ng/sWMkCgSyfhXzCoqWqzImfrFO" ) ]
                            []
                        ]
            in
            \_ ->
                viewFormattedText "https://my.homeserver.tld" cleanImg
                    |> Query.fromHtml
                    |> Query.has
                        [ tag "img"
                        , attribute <| width 200
                        , attribute <| height 200
                        , attribute <| src "https://my.homeserver.tld/_matrix/media/r0/download/olli.ng/sWMkCgSyfhXzCoqWqzImfrFO"
                        ]
        , test "Dirty image" <|
            let
                dirtyImg =
                    Html
                        [ Html.Parser.Element "img"
                            [ ( "onclick", "alert('haxed')" ), ( "width", "200" ), ( "height", "200" ), ( "src", "https://my.evil.site/exploit.png" ) ]
                            []
                        ]

                fmtHtml =
                    viewFormattedText "https://my.homeserver.tld" dirtyImg
                        |> Query.fromHtml
            in
            Expect.all
                [ \_ -> Query.hasNot [ attribute <| src "https://my.evil.site/exploit.png" ] fmtHtml
                , \_ -> Query.has [ tag "img", attribute <| width 200, attribute <| height 200 ] fmtHtml
                ]
        ]
