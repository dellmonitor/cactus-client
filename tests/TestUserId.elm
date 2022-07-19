module TestUserId exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Url.Builder
import UserId exposing (parseUserId, toString)


testParseUserId : Test
testParseUserId =
    describe "Test parseUserId"
        [ test "Parse @asbjorn:olli.ng" <|
            \_ ->
                parseUserId "@asbjorn:olli.ng"
                    |> Result.map toString
                    |> Expect.equal (Ok "@asbjorn:olli.ng")
        , test "Parse @ASBJORN:OLLI.NG" <|
            \_ ->
                parseUserId "@ASBJORN:OLLI.NG"
                    |> Result.map toString
                    |> Expect.equal (Ok "@asbjorn:olli.ng")
        , test "Parse @dev1:localhost:8008" <|
            \_ ->
                parseUserId "@dev1:localhost:8008"
                    |> Result.map toString
                    |> Expect.equal (Ok "@dev1:localhost:8008")
        , test "Fail on invalid userid: @💀:🐻" <|
            \_ ->
                parseUserId "@💀:🐻"
                    |> Result.map toString
                    |> Expect.err
        , test "Fail on invalid userid: foobar" <|
            \_ ->
                parseUserId "foobar"
                    |> Result.map toString
                    |> Expect.err
        , test "Fail on invalid userid: @foobar:wow🐻" <|
            \_ ->
                parseUserId "@foobar:wow🐻"
                    |> Result.map toString
                    |> Expect.err
        , test "Fail on invalid userid: 🐻 @foobar:wow.com" <|
            \_ ->
                parseUserId "🐻 @foobar:wow.com"
                    |> Result.map toString
                    |> Expect.err
        ]
