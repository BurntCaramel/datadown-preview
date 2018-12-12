module Preview.Markdown exposing (view)

{-| Preview Markdown


## Functions

@docs view

-}

import Html exposing (..)
import Markdown


{-| Previews a Markdown string as HTML
-}
view : String -> Html msg
view source =
    source
        |> Markdown.toHtml Nothing
        |> div []
