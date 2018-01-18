module Preview.Json
    exposing
        ( view
        )

{-| Preview JSON

## Functions

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode
import JsonValue exposing (JsonValue(..))


viewSymbol : String -> Html msg
viewSymbol symbol =
    span [ class "text-purple-light" ] [ text symbol ]


viewString : String -> List (Html msg)
viewString string =
    [ viewSymbol "\""
    , text string
    , viewSymbol "\""
    ]


viewArrayItem : JsonValue -> Html msg
viewArrayItem value =
    li [ class "ml-4 text-purple-light" ] [ viewValue value ]


viewKeyValuePair : (String, JsonValue) -> List (Html msg)
viewKeyValuePair (key, value) =
  [ dt [ class "font-bold" ] ((viewString key) ++ [ viewSymbol ":" ])
  , dd [ class "ml-4" ] [ viewValue value ]
  ]

viewValue : JsonValue -> Html msg
viewValue json =
    let
        children =
            case json of
                StringValue string ->
                    (viewString string)

                NumericValue number ->
                    [ text (toString number) ]

                BoolValue bool ->
                    [ text (if bool then "true" else "false") ]

                ArrayValue items ->
                    [ viewSymbol "["
                    , ol [] (List.map viewArrayItem items)
                    , viewSymbol "]"
                    ]

                ObjectValue items ->
                    [ viewSymbol "{"
                    , dl [ class "ml-2" ] (List.concatMap viewKeyValuePair items)
                    , viewSymbol "}"
                    ]

                NullValue ->
                    [ text "null" ]
    in
        div [ class "text-black" ] children


{- Previews a JSON string as HTML -}
view : String -> Html msg
view source =
    let
        result =
            source
                |> Json.Decode.decodeString JsonValue.decoder
    in
        case result of
            Ok jsonValue ->
                viewValue jsonValue
            
            Err error ->
                div [] [ text error ]
