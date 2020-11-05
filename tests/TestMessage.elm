module TestMessage exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Message exposing (..)
import Test exposing (..)
import Time


testTimeSinceText : Test
testTimeSinceText =
    describe "Messages.timeSinceText"
        [ test "three seconds" <|
            \_ ->
                timeSinceText (Time.millisToPosix 4000) (Time.millisToPosix 1000)
                    |> Expect.equal "3 seconds ago"
        , test "five months" <|
            \_ ->
                timeSinceText (Time.millisToPosix 13392000000) (Time.millisToPosix 1000)
                    |> Expect.equal "5 months ago"
        , test "eight years" <|
            \_ ->
                timeSinceText (Time.millisToPosix 257126400000) (Time.millisToPosix 1000)
                    |> Expect.equal "8 years ago"
        ]
