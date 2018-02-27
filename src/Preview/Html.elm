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


keepIfSafeNode : HtmlParser.Node -> Maybe HtmlParser.Node
keepIfSafeNode node =
    case node of
        HtmlParser.Element tagName attrs children ->
            if isSafeElement tagName attrs children then
                Just <| HtmlParser.Element tagName attrs (sanitizeNodes children)
            else
                Nothing

        _ ->
            Just node


sanitizeNodes : List HtmlParser.Node -> List HtmlParser.Node
sanitizeNodes =
    List.filterMap keepIfSafeNode


{-| Previews a HTML string as sanitized HTML
-}
view : Bool -> String -> Html msg
view isSVG source =
    let
        elements =
            source
                |> HtmlParser.parse
                |> sanitizeNodes

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
