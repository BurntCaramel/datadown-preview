module Preview.Markdown
    exposing
        ( view
        )

{-| Preview Markdown


## Functions

@docs view

-}

import Markdown
import Html exposing (..)


{-| Previews a Markdown string as HTML
-}
view : String -> Html msg
view source =
    source
        |> Markdown.toHtml Nothing
        |> div []
