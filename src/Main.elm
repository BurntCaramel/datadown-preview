module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (rows)
import Html.Events exposing (onInput)
import Datadown exposing (Document, Section)
import Datadown.Content exposing (Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process exposing (processDocument)


type alias Model =
    { input : String
    , document : Document
    }

model : Model
model =
    { input = ""
    , document = parseDocument ""
    }


type Message
    = ChangeInput String


update : Message -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            { model | input = newInput, document = parseDocument newInput }


viewContent : Content -> Html Message
viewContent content =
    case content of
        Text s ->
            div [] [ text s ]
        
        Code language s ->
            pre [] [ code [] [ text s ] ]

        List items ->
            div [] (List.map viewContent items)


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
        div []
            [ div []
                [ textarea [ rows 20, onInput ChangeInput ] [ text model.input ]
                ]
            , div [] resultsEl
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
