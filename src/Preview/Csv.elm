module Preview.Csv exposing (viewFromSource)

{-| Preview CSV


## Functions

@docs viewFromSource

-}

import Csv exposing (Csv)
import Html exposing (..)
import Html.Attributes exposing (class)


viewHeader : String -> Html msg
viewHeader headerName =
    div [ class "text-sm italic" ] [ text headerName ]


viewRecordsHeading : List (List String) -> Html msg
viewRecordsHeading records =
    let
        content =
            case List.length records of
                0 ->
                    "No row"

                1 ->
                    "1 row"

                n ->
                    String.fromInt n ++ " rows"
    in
    div [ class "mt-2 font-bold" ] [ text content ]


viewRow : List String -> List String -> Html msg
viewRow headers record =
    let
        viewItem value header =
            [ dt [ class "inline-block w-2/5 break-words whitespace-pre-wrap align-top px-1 border-t" ] [ text header ]
            , dd [ class "inline-block w-3/5 break-words whitespace-pre-wrap align-top px-1 border-t border-l" ] [ text value ]
            ]
    in
    List.map2 viewItem record headers
        |> List.concat
        |> dl [ class "my-2 border border-t-0 text-sm" ]


viewCsv : Csv -> Html msg
viewCsv { headers, records } =
    div []
        [ div []
            (List.map viewHeader headers)
        , viewRecordsHeading records
        , ol []
            (List.map (viewRow headers) records)
        ]


{-| Previews a CSV string as HTML
-}
viewFromSource : String -> Html msg
viewFromSource source =
    source
        |> Csv.parse
        |> viewCsv
