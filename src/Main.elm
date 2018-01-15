module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onInput)
import Datadown exposing (Document, Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process exposing (processDocument)

import Parser exposing (Error)

import Expressions.Tokenize as Tokenize exposing (tokenize, Token(..))
import Expressions.Evaluate as Evaluate exposing (resolveTokens)


type Error
    = Parser Parser.Error
    | Evaluate Evaluate.Error
    -- | Process Datadown.Process.Error

parseExpressions : String -> Result Error (List (List Token))
parseExpressions input =
    case tokenize input of
        Err parserError ->
            Err (Parser parserError)
        
        Ok tokens ->
            Ok tokens


resolveExpressions : Result Error (List (List Token)) -> Result String (Result Error (List (List Token)))
resolveExpressions parsedExpressions =
    case parsedExpressions of
        Err error ->
            Err "Invalid expressions"
        
        Ok expressions ->
            case expressions of
                [] ->
                    Err "No input"

                hd :: [] -> 
                    resolveTokens (\_ -> Nothing) hd
                        |> Result.map List.singleton
                        |> Result.mapError Evaluate
                        |> Ok
                
                _ ->
                    Err "Can only handle one line"


type alias Model =
    { input : String
    }

defaultInput : String
defaultInput = """
# Welcome screen

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


viewExpressionToken : Token -> Html Message
viewExpressionToken token =
    case token of
        Identifier identifier ->
            div [] [ text identifier ]
        
        Value value ->
            div [] [ text (toString value) ]
        
        Operator operator ->
            div [] [ text (toString operator) ]


viewExpression : List Token -> Html Message
viewExpression tokens =
    div [] (tokens |> List.map viewExpressionToken)


viewContent : Content (Result Error (List (List Token))) -> Html Message
viewContent content =
    case content of
        Text s ->
            pre [ class "font-sans" ] [ text s ]
        
        Code language s ->
            pre [] [ code [] [ text s ] ]
        
        Expressions expressionsResult ->
            case expressionsResult of
                Err expressionsError ->
                    h3 [] [ text "expressions error" ]
                
                Ok expressions ->
                    pre [] [ code [] (List.map viewExpression expressions) ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent item ]) items)

        Quote document ->
            pre [] [ code [] [ text "quoted document" ] ]


viewResultInner : Result e (Content (Result Error (List (List Token)))) -> Html Message
viewResultInner contentResult =
    case contentResult of
        Err error ->
            div [] [ text "[error]" ]
        
        Ok content ->
            div [ class "mb-3" ] [ viewContent content ]


viewResult : (String, Result e (Content (Result Error (List (List Token))))) -> Html Message
viewResult (key, contentResult) =
    div []
        [ h2 [] [ text key ]
        , contentResult
            |> viewResultInner
        ]


view : Model -> Html Message
view model =
    let
        document : Document (Result Error (List (List Token)))
        document =
            parseDocument parseExpressions model.input

        --results : Dict String (Result Datadown.Process.Error (Content (Result Error (List (List Token)))))
        results =
            processDocument resolveExpressions document
        
        resultsEl =
            results
                |> Dict.toList
                |> List.map viewResult
    in
        div [ class "flex h-screen p-4" ]
            [ div [ class "flex-1" ]
                [ textarea [ class "w-full h-full font-sans", rows 20, onInput ChangeInput ] [ text model.input ]
                ]
            , div [ class "flex-1" ]
                [ h1 [ class "mb-4" ] [ text document.title ]
                , div [] resultsEl
                ]
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
