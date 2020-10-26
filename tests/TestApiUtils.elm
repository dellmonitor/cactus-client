module TestApiUtils exposing (..)

import ApiUtils exposing (serverNameFromId)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Test ApiUtils module"
        [ testServerNameFromId ]


testServerNameFromId : Test
testServerNameFromId =
    describe "Test serverNameFromId"
        [ test "server name from room alias" <|
            \_ ->
                serverNameFromId "#room:server.com"
                    |> Expect.equal (Just "server.com")
        , test "server name from user id" <|
            \_ ->
                serverNameFromId "@user:my.home.server"
                    |> Expect.equal (Just "my.home.server")
        , test "garbage input" <|
            \_ ->
                serverNameFromId "foobar"
                    |> Expect.equal Nothing
        ]
