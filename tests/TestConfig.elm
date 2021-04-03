module TestConfig exposing (..)

import Expect exposing (Expectation)
import Json.Decode as JD
import Main exposing (..)
import Session
import Test exposing (..)


minimalValidJson : String
minimalValidJson =
    """
      {
        "defaultHomeserverUrl": "https://example.com",
        "serverName": "example.com",
        "siteName": "blog.example.com",
        "commentSectionId": "myCommentSection",
        "storedSession": null
      }
    """


testDecodeMinimalConfig : Test
testDecodeMinimalConfig =
    let
        result : Result JD.Error ( StaticConfig, Maybe Session.Session )
        result =
            minimalValidJson
                |> JD.decodeString decodeFlags
                |> Result.map parseFlags
    in
    test "Decode minimal configuration flags" <|
        \_ ->
            result
                |> Result.map
                    (\( conf, sess ) ->
                        Expect.all
                            [ \( _, s ) -> Expect.equal s Nothing
                            , \( c, _ ) ->
                                Expect.equal c
                                    { defaultHomeserverUrl = "https://example.com"
                                    , roomAlias = "#comments_blog.example.com_myCommentSection:example.com"
                                    , pageSize = 10
                                    , embeddedLogin = True
                                    }
                            ]
                            ( conf, sess )
                    )
                |> Result.withDefault (Expect.fail "Configuration did not decode")
