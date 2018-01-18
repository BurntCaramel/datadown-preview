module Datadown.Process
    exposing
        ( processDocument
        , listVariablesInDocument
        , Error
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


{-| Error after processing, possibly from evaluating expressions
-}
type Error e
    = NoContentForSection String
    | UnknownKind
    | Evaluate e


type alias Resolved e a =
    { resolvedValues : Dict String (Result (Error e) (Content a))
    , tests : List (Document a)
    }


mustacheVariableRegex : Regex
mustacheVariableRegex =
    Regex.regex "{{\\s?(.+?)\\s?}}"


mustache : (String -> Maybe String) -> String -> String
mustache resolveVariable input =
    let
        replacer : Regex.Match -> String
        replacer match =
            let
                key =
                    match.submatches
                        |> List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.withDefault ""
            in
                resolveVariable key
                    |> Maybe.withDefault ""
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


resolveStringForResults : (Content a -> Maybe String) -> List ( String, Result (Error e) (Content a) ) -> String -> Maybe String
resolveStringForResults stringForContent results keyToFind =
    let
        found =
            results
                |> List.filter (\( key, result ) -> key == keyToFind)
                |> List.head
    in
        case found of
            Just ( key, Ok content ) ->
                stringForContent content

            _ ->
                Nothing


processSection : (String -> Maybe String) -> (a -> Result e a) -> Section a -> Result (Error e) (Content a)
processSection resolve evaluateExpressions section =
    case section.mainContent of
        Just (Text text) ->
            Ok (Text (mustache resolve text))

        Just (Code language codeText) ->
            Ok (Code language (mustache resolve codeText))

        Just (Expressions input) ->
            case evaluateExpressions input of
                Ok output ->
                    Ok (Expressions output)

                Err error ->
                    Err (Evaluate error)

        Just content ->
            Ok content

        Nothing ->
            Err (NoContentForSection section.title)


foldProcessedSections : (a -> Result e a) -> (Content a -> Maybe String) -> Section a -> List ( String, Result (Error e) (Content a) ) -> List ( String, Result (Error e) (Content a) )
foldProcessedSections evaluateExpressions stringForContent section prevResults =
    let
        resolve : String -> Maybe String
        resolve =
            resolveStringForResults stringForContent prevResults

        result : Result (Error e) (Content a)
        result =
            processSection resolve evaluateExpressions section
    in
        ( section.title, result ) :: prevResults


{-| Process a document and return a result
-}
processDocument : (a -> Result e a) -> (Content a -> Maybe String) -> Document a -> List ( String, Result (Error e) (Content a) )
processDocument evaluateExpressions stringForContent document =
    document.sections
        |> List.foldl (foldProcessedSections evaluateExpressions stringForContent) []
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
