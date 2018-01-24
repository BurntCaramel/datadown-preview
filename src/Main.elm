module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, rows, attribute)
import Html.Events exposing (onInput)
import Time exposing (Time)
import Date
import Datadown exposing (Document, Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process as Process exposing (processDocument, listVariablesInDocument, Error)
import JsonValue exposing (JsonValue(..))
import Parser exposing (Error)
import Preview.Json
import Preview.Html
import Expressions.Tokenize as Tokenize exposing (tokenize, Token(..))
import Expressions.Evaluate as Evaluate exposing (evaulateTokenLines)
import Samples.Clock


type alias Model =
    { input : String
    , now : Time
    }


type Error
    = Parser Parser.Error
    | Evaluate Evaluate.Error
    | Process Process.Error


parseExpressions : String -> Result Error (List (List Token))
parseExpressions input =
    case tokenize input of
        Err parserError ->
            Err (Parser parserError)

        Ok tokens ->
            Ok tokens


valueFromModel : Model -> String -> Maybe JsonValue
valueFromModel model key =
    case key of
        "now.seconds" ->
            model.now
                |> Time.inSeconds
                |> floor
                |> toFloat
                |> JsonValue.NumericValue >> Just
        
        "now.date.s" ->
            model.now
                |> Date.fromTime
                |> Date.second
                |> toFloat
                |> JsonValue.NumericValue >> Just
        
        "now.date.m" ->
            model.now
                |> Date.fromTime
                |> Date.minute
                |> toFloat
                |> JsonValue.NumericValue >> Just
        
        "now.date.h" ->
            model.now
                |> Date.fromTime
                |> Date.hour
                |> toFloat
                |> JsonValue.NumericValue >> Just
        
        _ ->
            Nothing


evaluateExpressions : Model -> (String -> Result (Process.Error Evaluate.Error) JsonValue) -> Result Error (List (List Token)) -> Result Evaluate.Error JsonValue
evaluateExpressions model resolveFromDocument parsedExpressions =
    let
        resolveWithModel key =
            case valueFromModel model key of
                Just value ->
                    Ok value
                
                Nothing ->
                    resolveFromDocument key
    in
        
    case parsedExpressions of
        Err error ->
            Err Evaluate.Parsing

        Ok expressions ->
            evaulateTokenLines resolveWithModel expressions


contentToJson : Model -> Content (Result Error (List (List Token))) -> Result Evaluate.Error JsonValue
contentToJson model content =
    case content of
        Text text ->
            Ok <| JsonValue.StringValue text

        Json json ->
            Ok json

        Expressions lines ->
            case lines of
                Ok (((Value value) :: []) :: []) ->
                    Ok value

                _ ->
                    Err Evaluate.CannotConvertToJson
        
        List items ->
            items
                |> List.filterMap ((contentToJson model) >> Result.toMaybe)
                |> JsonValue.ArrayValue
                |> Ok
        
        Code maybeLanguage source ->
            Ok <| JsonValue.StringValue <| String.trim source

        _ ->
            Err Evaluate.CannotConvertToJson


init : ( Model, Cmd Message )
init =
    { input = Samples.Clock.source
    , now = 0
    }
        ! [ Cmd.none
          ]


type Message
    = ChangeInput String
    | Time Time


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ChangeInput newInput ->
            ( { model | input = newInput }, Cmd.none )

        Time time ->
            ( { model | now = time }, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Time.every Time.second Time
        ]


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


showCodeForLanguage : Maybe String -> Bool
showCodeForLanguage language =
    case language of
        Just "json" ->
            False

        _ ->
            True


viewCodePreview : Maybe String -> String -> List (Html Message)
viewCodePreview language source =
    case language of
        Just "html" ->
            [ Preview.Html.view False source ]

        Just "svg" ->
            [ Preview.Html.view True source ]

        Just "json" ->
            [ Preview.Json.view source ]

        _ ->
            [ text (language |> Maybe.withDefault "none") ]


viewCode : Bool -> Maybe String -> String -> Html Message
viewCode compact language source =
    let
        codeHtmlList =
            if not compact && showCodeForLanguage language then
                [ pre [ class "overflow-auto px-2 py-2 text-purple-darker bg-purple-lightest" ]
                    [ code [ class "font-mono text-sm" ] [ text source ] ]
                ]
            else
                []
    in
        div []
            (codeHtmlList ++ viewCodePreview language source)


viewContent : Bool -> Content (Result Error (List (List Token))) -> Html Message
viewContent compact content =
    case content of
        Text s ->
            div [ class "font-sans w-full" ] [ text s ]

        Code language source ->
            viewCode compact language source

        Json json ->
            Preview.Json.viewJson json

        Expressions expressionsResult ->
            case expressionsResult of
                Err expressionsError ->
                    h3 [] [ text <| toString expressionsError ]

                Ok expressions ->
                    pre [ class "px-2 py-2 text-teal-darker bg-teal-lightest" ]
                        [ code [ class "font-mono text-sm" ] (List.map viewExpression expressions) ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent compact item ]) items)

        Quote document ->
            pre [] [ code [] [ text "quoted document" ] ]


viewContentResult : Bool -> Result e (Content (Result Error (List (List Token)))) -> Html Message
viewContentResult compact contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [] [ text (toString error) ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent compact content ]


type alias SectionViewModel e =
    { title : String
    , resolvedContent : Result e (Content (Result Error (List (List Token))))
    , variables : List String
    }


makeSectionViewModel : ( String, Result e (Content (Result Error (List (List Token)))) ) -> ( String, List String ) -> SectionViewModel e
makeSectionViewModel ( title, resolvedContent ) ( _, variables ) =
    SectionViewModel title resolvedContent variables


viewSection : SectionViewModel e -> Html Message
viewSection { title, resolvedContent, variables } =
    details [ attribute "open" "" ]
        [ summary []
            [ h2 [ class "text-xl text-blue-dark" ] [ text title ]
            ]
        , resolvedContent
            |> viewContentResult False

        -- , div [] [ text (variables |> toString) ]
        ]


view : Model -> Html Message
view model =
    let
        document : Document (Result Error (List (List Token)))
        document =
            parseDocument parseExpressions model.input

        resolved =
            processDocument (evaluateExpressions model) (contentToJson model) document

        sectionVariables =
            listVariablesInDocument document

        results =
            List.map2 makeSectionViewModel resolved.sections sectionVariables

        resultsEl =
            results
                |> List.map viewSection
        
        introEl =
            case resolved.intro of
                Ok content ->
                   viewContentResult True (Ok content)
                
                Err (Process.NoContentForSection _) ->
                    text ""

                Err error ->
                    div [] [ text <| toString error ]
    in
        div []
            [ div [ class "flex flex-wrap h-screen" ]
                [ div [ class "flex-1 overflow-auto mb-8 p-4 pb-8 md:pl-6" ]
                    [ h1 [ class "mb-4 text-3xl text-blue" ] [ text document.title ]
                    , introEl
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
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
