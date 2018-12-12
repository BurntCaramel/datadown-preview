module Preview.Decorate exposing (view)

{-| Add decorations for colors


## Functions

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Regex exposing (Regex)


hex24BitRegex : Regex
hex24BitRegex =
    Regex.fromString "(#[a-fA-F0-9]{6}|#[a-fA-F0-9]{3})\\b"
        |> Maybe.withDefault Regex.never


matchToColorSwatch : Regex.Match -> String
matchToColorSwatch match =
    let
        color =
            match.match
    in
    "__START__ color:" ++ color ++ "__END__"


process : String -> List (Html msg)
process input =
    case String.split "__END__" input of
        [] ->
            [ text "!!!" ]

        head :: tail ->
            let
                rest =
                    text (String.join "" tail)
            in
            if String.startsWith " color:" head then
                let
                    color =
                        String.dropLeft 7 head
                in
                [ span [ class "inline-block w-4 h-4 mr-1 border border-grey-dark", style "backgroundColor" color ] []
                , text color
                , rest
                ]

            else
                [ text head, rest ]


view : String -> List (Html msg)
view input =
    input
        |> Regex.replace hex24BitRegex matchToColorSwatch
        |> String.split "__START__"
        |> List.concatMap process
