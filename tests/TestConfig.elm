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
                                    , loginEnabled = True
                                    }
                            ]
                            ( conf, sess )
                    )
                |> Result.withDefault (Expect.fail "Configuration did not decode")


completeValidJson : String
completeValidJson =
    """
      {
        "defaultHomeserverUrl": "https://example.com:8448",
        "serverName": "example.com",
        "siteName": "anotherblog.example.com",
        "commentSectionId": "anotherCommentSection",
        "pageSize": 2,
        "loginEnabled": false,
        "storedSession": {
          "homeserverUrl": "https://example.com:8448",
          "kind": "guest",
          "txnId": 0,
          "userId": "@1234:example.com",
          "accessToken": "abcdVerySecret"
        }
      }
    """


testDecodeCompleteConfig : Test
testDecodeCompleteConfig =
    let
        result : Result JD.Error ( StaticConfig, Maybe Session.Session )
        result =
            completeValidJson
                |> JD.decodeString decodeFlags
                |> Result.map parseFlags
    in
    test "Decode complete configuration JSON" <|
        \_ ->
            result
                |> Result.map
                    (\( conf, sess ) ->
                        Expect.all
                            [ \( _, s ) -> Expect.notEqual s Nothing
                            , \( c, _ ) ->
                                Expect.equal c
                                    { defaultHomeserverUrl = "https://example.com:8448"
                                    , roomAlias = "#comments_anotherblog.example.com_anotherCommentSection:example.com"
                                    , pageSize = 2
                                    , loginEnabled = False
                                    }
                            ]
                            ( conf, sess )
                    )
                |> Result.withDefault (Expect.fail "Configuration did not decode")
