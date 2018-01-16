module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onInput)
import Datadown exposing (Document, Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process exposing (processDocument)
import HtmlParser
import HtmlParser.Util
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
defaultInput =
    """
# Welcome screen

## firstName
Jane

## lastName
Doe

## fullName
{{ firstName }} {{ lastName }}

## render
```html
<h1>Welcome, {{ fullName }}!</h1>
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


unsafeTagNames : List String
unsafeTagNames = ["script", "link", "iframe", "object", "embed"]


previewHTML : String -> List (Html Message)
previewHTML source =
    source
        |> HtmlParser.parse
        |> HtmlParser.Util.filterElements (\tagName attributes children -> not (List.member tagName unsafeTagNames))
        |> HtmlParser.Util.toVirtualDom


viewCodePreview : Maybe String -> String -> List (Html Message)
viewCodePreview language source =
    case language of
        Just "html" ->
            previewHTML source

        Just "svg" ->
            previewHTML source

        _ ->
            [ text (language |> Maybe.withDefault "none") ]


viewCode : Maybe String -> String -> Html Message
viewCode language source =
    div []
        ([ pre [ class "overflow-auto px-2 py-2 text-purple-darker bg-purple-lightest" ]
            [ code [ class "font-mono text-sm" ] [ text source ] ]
         ]
            ++ viewCodePreview language source
        )


viewContent : Content (Result Error (List (List Token))) -> Html Message
viewContent content =
    case content of
        Text s ->
            div [ class "font-sans w-full" ] [ text s ]

        Code language source ->
            viewCode language source

        Expressions expressionsResult ->
            case expressionsResult of
                Err expressionsError ->
                    h3 [] [ text "expressions error" ]

                Ok expressions ->
                    pre [ class "px-2 py-2 text-teal-darker bg-teal-lightest" ]
                        [ code [ class "font-mono text-sm" ] (List.map viewExpression expressions) ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent item ]) items)

        Quote document ->
            pre [] [ code [] [ text "quoted document" ] ]


viewResultInner : Result e (Content (Result Error (List (List Token)))) -> Html Message
viewResultInner contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [] [ text "[error]" ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent content ]


viewResult : ( String, Result e (Content (Result Error (List (List Token)))) ) -> Html Message
viewResult ( key, contentResult ) =
    div []
        [ h2 [ class "text-blue-dark" ] [ text key ]
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
                |> List.map viewResult
    in
        div []
            [ div [ class "flex flex-wrap h-screen" ]
                [ div [ class "flex-1 overflow-auto mb-8 p-4 pb-8 md:pl-6" ]
                    [ h1 [ class "mb-4 text-blue" ] [ text document.title ]
                    , div [] resultsEl
                    ]
                , div [ class "flex-1 min-w-full md:min-w-0" ]
                    [ textarea [ class "flex-1 w-full h-full pt-4 pl-4 font-mono text-sm text-blue-darkest bg-blue-lightest", rows 20, onInput ChangeInput ] [ text model.input ]
                    ]
                ]
            , div [ class "fixed pin-b pin-l flex pb-4 pl-4 md:pl-6" ]
                [ button [ class "px-2 py-1 text-purple-lightest bg-purple" ] [ text "Edit" ]
                , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Test" ]
                , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Export" ]
                ]
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
