module Preview.Html exposing (view)

{-| Preview HTML & SVG


## Functions

@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Parser
import Svg
import VirtualDom


{-| HTML tags to remove from preview
-}
unsafeTagNames : List String
unsafeTagNames =
    [ "script", "iframe", "object", "embed" ]


isSafeElement : String -> List ( String, String ) -> List Html.Parser.Node -> Bool
isSafeElement tagName attributes children =
    not (List.member tagName unsafeTagNames)


keepIfSafeNode : Html.Parser.Node -> Maybe Html.Parser.Node
keepIfSafeNode node =
    case node of
        Html.Parser.Element tagName attrs children ->
            if isSafeElement tagName attrs children then
                Just <| Html.Parser.Element tagName attrs (sanitizeNodes children)

            else
                Nothing

        _ ->
            Just node


sanitizeNodes : List Html.Parser.Node -> List Html.Parser.Node
sanitizeNodes =
    List.filterMap keepIfSafeNode


toDisplayAttribute : (String, String) -> Html.Attribute msg
toDisplayAttribute (key, value) =
    Html.Attributes.attribute key value


toDisplayHtml : Html.Parser.Node -> Html.Html msg
toDisplayHtml node =
    case node of
        Html.Parser.Element tagName attrs children ->
            VirtualDom.node tagName (List.map toDisplayAttribute attrs) (List.map toDisplayHtml children)
        
        Html.Parser.Text text ->
            Html.text text

        Html.Parser.Comment _ ->
            Html.text ""


{-| Previews a HTML string as sanitized HTML
-}
view : Bool -> String -> Html msg
view isSVG source =
    case Html.Parser.run source of
        Ok unsafeElements ->
            let
                elements =
                    sanitizeNodes unsafeElements

                hasSurroundingSVGTag =
                    case List.head elements of
                        Just (Html.Parser.Element "svg" _ _) ->
                            True

                        _ ->
                            False
            in
                if isSVG && not hasSurroundingSVGTag then
                    List.map toDisplayHtml elements
                        |> Svg.svg []

                else
                    List.map toDisplayHtml elements
                        |> Html.div []

        Err error ->
            Html.text "(Error)"