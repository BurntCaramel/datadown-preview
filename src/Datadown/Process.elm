module Datadown.Process
    exposing
        ( processDocument
        , Error(..)
        , Resolved
        , ResolvedSection(..)
        )

{-| Process


## Types

@docs Error


## Functions

@docs processDocument

-}

import Dict exposing (Dict(..))
import Regex exposing (Regex)
import Datadown exposing (Document, Section(..), Content(..))
import JsonValue exposing (JsonValue)


{-| Error after processing, possibly from evaluating expressions
-}
type Error e
    = NoValueForIdentifier String
    | CannotConvertContent
    | NoSection String
    | UnknownKind
    | Evaluate e
    | EvaluatingExpression String e
    | UnknownExpression
    | Multiple (List (Error e))


type alias Resolved e a =
    { sections : List ( String, ResolvedSection (Error e) a )
    , intro : List (Result (Error e) (Content a))
    , tests : List (Document a)
    }


type ResolvedSection e a
    = ResolvedSection
        { mainContent : List (Result e (Content a))
        , subsections : List ( String, ResolvedSection e a )
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
                |> List.map jsonToString
                |> String.join "\n"

        JsonValue.BoolValue bool ->
            if bool then
                "👍"
            else
                "👎"

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


contentForKeyPathInResolvedSections : List ( String, ResolvedSection (Error e) a ) -> List String -> Maybe (List (Result (Error e) (Content a)))
contentForKeyPathInResolvedSections resolvedSections keyPath =
    case keyPath of
        firstKey :: otherKeys ->
            let
                findContentInSection : ( String, ResolvedSection (Error e) a ) -> Maybe (List (Result (Error e) (Content a)))
                findContentInSection ( key, resolvedSection ) =
                    case resolvedSection of
                        ResolvedSection record ->
                            if key == firstKey then
                                if otherKeys == [] then
                                    case record.mainContent of
                                        [] ->
                                            Nothing

                                        _ ->
                                            Just record.mainContent
                                else
                                    contentForKeyPathInResolvedSections record.subsections otherKeys
                            else
                                Nothing

                findInSections resolvedSections =
                    case resolvedSections of
                        [] ->
                            Nothing

                        head :: tail ->
                            case findContentInSection head of
                                Just content ->
                                    Just content

                                Nothing ->
                                    findInSections tail
            in
                findInSections resolvedSections

        [] ->
            Nothing


processSection : (String -> Result (Error e) (List JsonValue)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> Section a -> List (Result (Error e) (Content a))
processSection valueListForIdentifier evaluateExpression sectionWrapper =
    let
        section =
            case sectionWrapper of
                Section section ->
                    section

        expressionForString : String -> Maybe a
        expressionForString s =
            section.inlineExpressions
                |> Dict.get s

        valueForIdentifier id =
            valueListForIdentifier id
                |> Result.map (JsonValue.ArrayValue)

        resolveExpressionString : String -> Maybe JsonValue
        resolveExpressionString s =
            expressionForString s
                |> Maybe.andThen (evaluateExpression valueForIdentifier >> Result.toMaybe)

        processContent content =
            case content of
                Text text ->
                    Ok (Text (mustache resolveExpressionString text))

                List contentItems ->
                    -- Ok (List contentItems)
                    let
                        reduceItem item result =
                            case result of
                                Ok items ->
                                    case processContent item of
                                        Ok processedItem ->
                                            Ok (processedItem :: items)

                                        Err error ->
                                            Err error

                                Err error ->
                                    Err error

                        processedItems =
                            contentItems
                                |> List.foldl reduceItem (Ok [])
                                |> Result.map List.reverse
                    in
                        case processedItems of
                            Ok items ->
                                Ok (List items)

                            Err error ->
                                Err error

                Code language codeText ->
                    Ok (Code language (mustache resolveExpressionString codeText))

                Expressions input ->
                    case evaluateExpression valueForIdentifier input of
                        Ok json ->
                            Ok (Json json)

                        Err error ->
                            Err (Evaluate error)

                content ->
                    Ok content

        processNextContent content results =
            (processContent content) :: results
    in
        section.mainContent
            |> List.foldl processNextContent []
            |> List.reverse


flattenResults : List (Result e a) -> Result e (List a)
flattenResults inItems =
    case inItems of
        (Ok c) :: tail ->
            case flattenResults tail of
                Ok d ->
                    Ok (c :: d)

                Err e ->
                    Err e

        (Err e) :: tail ->
            Err e

        [] ->
            Ok []


nextProcessedSection : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> List (Result (Error e) (Content a))
nextProcessedSection evaluateExpression contentToJson section prevResults =
    let
        contentForKey : String -> Result (Error e) (List (Content a))
        contentForKey key =
            key
                |> String.split "."
                |> contentForKeyPathInResolvedSections prevResults
                |> Maybe.map flattenResults
                |> Maybe.withDefault (Err (NoValueForIdentifier key))

        valueForIdentifier : String -> Result (Error e) (List JsonValue)
        valueForIdentifier key =
            contentForKey key
                |> Result.andThen (List.map (contentToJson >> Result.mapError (always CannotConvertContent)) >> flattenResults)
    in
        processSection valueForIdentifier evaluateExpression section


foldProcessedSections : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> List ( String, ResolvedSection (Error e) a )
foldProcessedSections evaluateExpression contentToJson sectionWrapper prevResults =
    let
        title =
            case sectionWrapper of
                Section section ->
                    section.title

        subsections =
            case sectionWrapper of
                Section section ->
                    section.subsections

        resolvedMainContent : List (Result (Error e) (Content a))
        resolvedMainContent =
            nextProcessedSection evaluateExpression contentToJson sectionWrapper prevResults

        resolvedSubsections =
            subsections
                |> List.foldl (foldProcessedSections evaluateExpression contentToJson) prevResults
                |> List.take (List.length subsections)

        resolvedSection =
            ResolvedSection
                { mainContent = resolvedMainContent
                , subsections = resolvedSubsections
                }
    in
        ( title, resolvedSection ) :: prevResults


{-| Process a document and return a result
-}
processDocument : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Document a -> Resolved e a
processDocument evaluateExpression contentToJson document =
    let
        resolvedSections : List ( String, ResolvedSection (Error e) a )
        resolvedSections =
            document.sections
                |> List.foldl (foldProcessedSections evaluateExpression contentToJson) []
                |> List.reverse

        resolvedIntro =
            let
                introSection =
                    Section
                        { title = "intro"
                        , mainContent = document.introContent
                        , subsections = []
                        , inlineExpressions = document.introInlineExpressions
                        }
            in
                nextProcessedSection evaluateExpression contentToJson introSection resolvedSections
                    |> List.reverse
    in
        { sections = resolvedSections
        , intro = resolvedIntro
        , tests = []
        }
