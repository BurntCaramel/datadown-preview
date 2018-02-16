module Preview
    exposing
        ( view
        )

{-| Preview code snippets


## Functions

@docs view

-}

import Dict exposing (Dict)
import Html exposing (Html, text)
import Preview.Html
import Preview.Json
import Preview.Markdown


languageToViewTable : Dict String (String -> Html msg)
languageToViewTable =
    Dict.fromList
        [ ( "html", Preview.Html.view False )
        , ( "svg", Preview.Html.view True )
        , ( "json", Preview.Json.view )
        , ( "markdown", Preview.Markdown.view )
        , ( "md", Preview.Markdown.view )
        ]


languageToView : String -> Maybe (String -> Html msg)
languageToView language =
    Dict.get language languageToViewTable


defaultView : Maybe String -> String -> Html msg
defaultView maybeLanguage source =
    text (maybeLanguage |> Maybe.withDefault "none")


{-| Previews a HTML string as santized HTML
-}
view : Maybe String -> String -> Html msg
view maybeLanguage source =
    let
        maybeView =
            maybeLanguage
                |> Maybe.andThen languageToView
    in
        case maybeView of
            Just view ->
                view source

            Nothing ->
                defaultView maybeLanguage source
