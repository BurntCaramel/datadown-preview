module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onInput)
import Datadown exposing (Document, Section)
import Datadown.Content exposing (Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process exposing (processDocument)


type alias Model =
    { input : String
    }

defaultInput : String
defaultInput = """
# Title

## firstName
Jane

## lastName
Doe

## fullName
{{ firstName }} {{ lastName }}

## html
```html
<div>Welcome, {{ fullName }}!</div>
```
""" |> String.trim

model : Model
model =
    { input = defaultInput
    }


type Message
    = ChangeInput String


update : Message -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            { model | input = newInput }


viewContent : Content -> Html Message
viewContent content =
    case content of
        Text s ->
            pre [ class "font-sans" ] [ text s ]
        
        Code language s ->
            pre [] [ code [] [ text s ] ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent item ]) items)


viewResult : (String, Result e Content) -> Html Message
viewResult (key, contentResult) =
    div []
        [ h2 [] [ text key ]
        , contentResult
            |> Result.toMaybe
            |> Maybe.map viewContent
            |> Maybe.withDefault (div [] [ text "?" ])
        ]


view : Model -> Html Message
view model =
    let
        document =
            parseDocument model.input

        results =
            processDocument document
        
        resultsEl =
            results
                |> Dict.toList
                |> List.map viewResult
    in
        div [ class "flex h-screen p-4" ]
            [ div [ class "flex-1" ]
                [ textarea [ class "w-full h-full font-sans", rows 20, onInput ChangeInput ] [ text model.input ]
                ]
            , div [ class "flex-1" ] resultsEl
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
