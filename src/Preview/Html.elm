module Preview.Html
    exposing
        ( view
        )

{-| Preview HTML & SVG


## Functions

@docs view

-}

import Html exposing (..)
import Svg
import HtmlParser
import HtmlParser.Util


{-| HTML tags to remove from preview
-}
unsafeTagNames : List String
unsafeTagNames =
    [ "script", "link", "iframe", "object", "embed" ]


isSafeElement : String -> HtmlParser.Attributes -> List HtmlParser.Node -> Bool
isSafeElement tagName attributes children =
    not (List.member tagName unsafeTagNames)


{-| Previews a HTML string as santized HTML
-}
view : Bool -> String -> Html msg
view isSVG source =
    let
        elements =
            source
                |> HtmlParser.parse
                |> HtmlParser.Util.filterElements isSafeElement

        hasSurroundingSVGTag =
            case List.head elements of
                Just (HtmlParser.Element "svg" _ _) ->
                    True

                _ ->
                    False
    in
        if isSVG && not hasSurroundingSVGTag then
            elements
                |> HtmlParser.Util.toVirtualDomSvg
                |> Svg.svg []
        else
            elements
                |> HtmlParser.Util.toVirtualDom
                |> div []
