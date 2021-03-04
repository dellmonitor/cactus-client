module TestImage exposing (..)

import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Message.Image exposing (ImageData, decodeImage, viewImage)
import Test exposing (..)
import Test.Html.Query
import Test.Html.Selector


event_content_image_a =
    """
    {
    "body": "flatmount.png",
    "info": {
      "h": 1800,
      "mimetype": "image/png",
      "size": 135786,
      "thumbnail_info": {
        "h": 500,
        "mimetype": "image/png",
        "size": 36525,
        "w": 800
      },
      "thumbnail_url": "mxc://matrix.org/WkIsslUoLuyGsSHsRRKWUWMe",
      "w": 2880
    },
    "msgtype": "m.image",
    "url": "mxc://matrix.org/PWXbVKjGzmLGgaQvVSAlCfBR"
    }
"""


testDecodeFullImage : Test
testDecodeFullImage =
    describe "Test decoding and rendering an m.image message event"
        [ test "decode and render a full image event" <|
            \_ ->
                let
                    imgdata : Result JD.Error ImageData
                    imgdata =
                        JD.decodeString decodeImage event_content_image_a

                    rendered : Result JD.Error (Html msg)
                    rendered =
                        Result.map (viewImage "https://example.com") imgdata
                in
                rendered
                    |> Result.map
                        (\r ->
                            r
                                |> Test.Html.Query.fromHtml
                                |> Expect.all
                                    -- check thumbnail image
                                    [ \h ->
                                        h
                                            |> Test.Html.Query.find [ Test.Html.Selector.tag "img" ]
                                            |> Test.Html.Query.has
                                                [ Test.Html.Selector.tag "img"
                                                , Test.Html.Selector.class "cactus-message-image"
                                                , Test.Html.Selector.attribute (Html.Attributes.height 500)
                                                , Test.Html.Selector.attribute (Html.Attributes.width 800)
                                                , Test.Html.Selector.attribute (Html.Attributes.src "https://example.com/_matrix/media/r0/download/matrix.org/WkIsslUoLuyGsSHsRRKWUWMe")
                                                ]

                                    -- check link to full image
                                    , \h ->
                                        h
                                            |> Test.Html.Query.has
                                                [ Test.Html.Selector.tag "a"
                                                , Test.Html.Selector.attribute (Html.Attributes.href "https://example.com/_matrix/media/r0/download/matrix.org/PWXbVKjGzmLGgaQvVSAlCfBR")
                                                ]
                                    ]
                        )
                    |> (\r ->
                            case r of
                                Ok exp ->
                                    exp

                                Err err ->
                                    Expect.fail <| JD.errorToString err
                       )
        ]
