module Datadown.Process
    exposing
        ( processDocument
        , listVariablesInDocument
        , Error(..)
        )

{-| Process


## Types

@docs Error


## Functions

@docs processDocument

-}

import Dict exposing (Dict(..))
import Regex exposing (Regex)
import Datadown exposing (Document, Section, Content(..))
import JsonValue exposing (JsonValue)


{-| Error after processing, possibly from evaluating expressions
-}
type Error e
    = NoContentForSection String
    | NoValueForIdentifier String
    | CannotConvertContent
    | NoSection String
    | UnknownKind
    | Evaluate e
    | EvaluatingExpression String e
    | UnknownExpression


type alias Resolved e a =
    { resolvedValues : Dict String (Result (Error e) (Content a))
    , tests : List (Document a)
    }


mustacheVariableRegex : Regex
mustacheVariableRegex =
    Regex.regex "{{([^}]*)}}"


jsonToString : JsonValue -> String
jsonToString json =
    case json of
        JsonValue.StringValue s ->
            s

        JsonValue.NumericValue f ->
            toString f
        
        -- TODO: turn to real Markdown?
        JsonValue.ArrayValue items ->
            items
                |> List.map (jsonToString >> String.append "- ")
                |> String.join "\n"

        _ ->
            toString json


mustache : (String -> Maybe JsonValue) -> String -> String
mustache resolveVariable input =
    let
        replacer : Regex.Match -> String
        replacer match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault ""
                |> resolveVariable
                |> Maybe.map jsonToString
                |> Maybe.withDefault "?"
    in
        Regex.replace Regex.All mustacheVariableRegex replacer input


listMustacheVariables : String -> List String
listMustacheVariables input =
    let
        extractor : Regex.Match -> Maybe String
        extractor match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
    in
        Regex.find Regex.All mustacheVariableRegex input
            |> List.filterMap extractor


contentForKeyInResults : List ( a, r ) -> a -> Maybe r
contentForKeyInResults results key =
    results
        |> List.filter (\( key_, result ) -> key_ == key)
        |> List.head
        |> Maybe.map Tuple.second


processSection : (String -> Result (Error e) JsonValue) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> Section a -> Result (Error e) (Content a)
processSection valueForIdentifier evaluateExpression section =
    let
        expressionForString : String -> Maybe a
        expressionForString s =
            section.inlineExpressions
                |> Dict.get s

        resolveExpressionString : String -> Maybe JsonValue
        resolveExpressionString s =
            expressionForString s
                |> Maybe.andThen (evaluateExpression valueForIdentifier >> Result.toMaybe)
    in
        case section.mainContent of
            Just (Text text) ->
                Ok (Text (mustache resolveExpressionString text))

            Just (Code language codeText) ->
                Ok (Code language (mustache resolveExpressionString codeText))

            Just (Expressions input) ->
                case evaluateExpression valueForIdentifier input of
                    Ok json ->
                        Ok (Json json)

                    Err error ->
                        Err (Evaluate error)

            Just content ->
                Ok content

            Nothing ->
                Err (NoContentForSection section.title)


foldProcessedSections : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, Result (Error e) (Content a) ) -> List ( String, Result (Error e) (Content a) )
foldProcessedSections evaluateExpression contentToJson section prevResults =
    let
        contentForKey : String -> Result (Error e) (Content a)
        contentForKey key =
            case contentForKeyInResults prevResults key of
                Just (Ok c) ->
                    Ok c

                Just (Err e) ->
                    Err e

                Nothing ->
                    Err (NoValueForIdentifier key)

        valueForIdentifier : String -> Result (Error e) JsonValue
        valueForIdentifier key =
            contentForKey key
                |> Result.andThen (contentToJson >> Result.mapError (always CannotConvertContent))

        result : Result (Error e) (Content a)
        result =
            processSection valueForIdentifier evaluateExpression section
    in
        ( section.title, result ) :: prevResults


{-| Process a document and return a result
-}
processDocument : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Document a -> List ( String, Result (Error e) (Content a) )
processDocument evaluateExpression contentToJson document =
    document.sections
        |> List.foldl (foldProcessedSections evaluateExpression contentToJson) []
        |> List.reverse


listVariablesInSection : Section a -> ( String, List String )
listVariablesInSection section =
    let
        variables =
            case section.mainContent of
                Just (Text text) ->
                    listMustacheVariables text

                Just (Code language codeText) ->
                    listMustacheVariables codeText

                _ ->
                    []
    in
        ( section.title, variables )


{-| List all variables within sections in a document
-}
listVariablesInDocument : Document a -> List ( String, List String )
listVariablesInDocument document =
    document.sections
        |> List.map listVariablesInSection
