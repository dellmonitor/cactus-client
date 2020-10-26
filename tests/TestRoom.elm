module TestRoom exposing (..)

import Expect exposing (Expectation)
import Html
import Html.Attributes
import Html.Parser
import Room exposing (makeRoomAlias)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


suite : Test
suite =
    describe "Test Room"
        [ testMakeRoomAlias ]


testMakeRoomAlias : Test
testMakeRoomAlias =
    describe "Test makeRoomAlias"
        [ test "makeRoomAlias with realistic values" <|
            \_ ->
                makeRoomAlias "myblog" "october-blogpost" "matrix.example.com"
                    |> Expect.equal "#comments_myblog_october-blogpost:matrix.example.com"
        , test "makeRoomAlias with other values..." <|
            \_ ->
                makeRoomAlias "a" "b" "c"
                    |> Expect.equal "#comments_a_b:c"
        ]
