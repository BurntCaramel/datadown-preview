module Datadown.Process
    exposing
        ( processDocumentWith
        , processDocument
        , processComponent
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
import Datadown.Rpc as Rpc exposing (Rpc)
import JsonValue exposing (JsonValue)
import Json.Decode
import Http


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
    | DecodingJson String
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
        , rpcs : List Rpc
        }


type alias CustomElement =
    { language : Maybe String
    , componentName : String
    , props : List ( String, JsonValue )
    }


mustacheVariableRegex : Regex
mustacheVariableRegex =
    Regex.regex "{{([^}]*)}}"


mustacheSectionRegex : Regex
mustacheSectionRegex =
    Regex.regex "{{#\\s*([^}]+?)\\s*}}([\\S\\s]+?){{/\\s*\\1\\s*}}"


mustacheSectionNegativeRegex : Regex
mustacheSectionNegativeRegex =
    Regex.regex "{{^\\s*([^}]+?)\\s*}}([\\S\\s]+?){{/\\s*\\1\\s*}}"


customElementOpenRegex : Regex
customElementOpenRegex =
    Regex.regex "<([A-Z]\\w*)([^>]*)>([\\S\\s]+)"


attributeRegex : Regex
attributeRegex =
    Regex.regex "\\s*(\\w[^=\"\\s]+)(?:=\"(\\w*)\")?"


customElementCloseRegex : String -> Regex
customElementCloseRegex name =
    "</"
        ++ (Regex.escape name)
        ++ ">"
        |> Regex.regex


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


matchToAttribute : Regex.Match -> Maybe ( String, JsonValue )
matchToAttribute match =
    case match.submatches of
        (Just key) :: (Just value) :: [] ->
            Just ( key, JsonValue.StringValue value )

        (Just key) :: Nothing :: [] ->
            Just ( key, JsonValue.BoolValue True )

        _ ->
            Nothing


processCustomElements : (CustomElement -> Result (Error e) (Content a)) -> String -> String
processCustomElements evaluateComponent input =
    let
        replacer : Regex.Match -> String
        replacer match =
            case match.submatches of
                (Just componentName) :: maybeAttributesSource :: maybeContent :: [] ->
                    let
                        props =
                            case maybeAttributesSource of
                                Just source ->
                                    source
                                        |> Regex.find Regex.All attributeRegex
                                        |> List.filterMap matchToAttribute

                                Nothing ->
                                    []

                        ( customElement, rest ) =
                            case maybeContent of
                                Just content ->
                                    case Regex.split (Regex.AtMost 1) (customElementCloseRegex componentName) content of
                                        childrenSource :: restInput :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource

                                                el =
                                                    CustomElement Nothing componentName (props ++ [ ( "children", JsonValue.StringValue children ) ])

                                                rest =
                                                    restInput
                                                        |> processCustomElements evaluateComponent
                                            in
                                                ( el, rest )

                                        childrenSource :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource

                                                el =
                                                    CustomElement Nothing componentName (props ++ [ ( "children", JsonValue.StringValue children ) ])
                                            in
                                                ( el, "" )

                                        _ ->
                                            ( CustomElement Nothing componentName props, "" )

                                Nothing ->
                                    ( CustomElement Nothing componentName props, "" )

                        result =
                            evaluateComponent customElement
                    in
                        case result of
                            Ok (Code maybeLanguage source) ->
                                source ++ rest

                            Ok _ ->
                                rest

                            Err error ->
                                (toString error)

                _ ->
                    "?"
    in
        Regex.replace (Regex.AtMost 1) customElementOpenRegex replacer input


mustache : (CustomElement -> Result (Error e) (Content a)) -> (String -> Maybe JsonValue) -> String -> String
mustache evaluateComponent resolveVariable input =
    let
        sectionReplacer : (Bool -> Bool) -> Regex.Match -> String
        sectionReplacer transformBool match =
            let
                value : Maybe JsonValue
                value =
                    match.submatches
                        |> List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.andThen resolveVariable

                inner : String
                inner =
                    case match.submatches of
                        _ :: Just string :: [] ->
                            string
                        
                        _ ->
                            ""

                truthy =
                    case value of
                        Nothing ->
                            False
                        
                        Just (JsonValue.BoolValue False) ->
                            False
                        
                        Just JsonValue.NullValue ->
                            False
                        
                        Just (JsonValue.ArrayValue [JsonValue.BoolValue False]) ->
                            False
                        
                        Just (JsonValue.ArrayValue [JsonValue.NullValue]) ->
                            False
                        
                        _ ->
                            True
                
                open =
                    transformBool truthy
            in
                if open then inner else ""

        variableReplacer : Regex.Match -> String
        variableReplacer match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault ""
                |> resolveVariable
                |> Maybe.map jsonToString
                |> Maybe.withDefault "?"
    in
        input
            |> processCustomElements evaluateComponent
            |> Regex.replace Regex.All mustacheSectionRegex (sectionReplacer identity)
            |> Regex.replace Regex.All mustacheSectionNegativeRegex (sectionReplacer not)
            |> Regex.replace Regex.All mustacheVariableRegex variableReplacer


contentForKeyPathInResolvedSections : List ( String, ResolvedSection (Error e) a ) -> List String -> Maybe (List (Result (Error e) (Content a)))
contentForKeyPathInResolvedSections resolvedSections keyPath =
    case keyPath of
        firstKey :: otherKeys ->
            let
                magicContent : List (Result (Error e) (Content a)) -> Maybe (Content a)
                magicContent content =
                    case content of
                        [ Ok (Json json) ] ->
                            case Rpc.fromJsonValue json of
                                Just rpc ->
                                    Just (Reference rpc.id otherKeys)
                                
                                Nothing ->
                                    Nothing
                        
                        _ ->
                            Nothing

                findContentInSection : ( String, ResolvedSection (Error e) a ) -> Maybe (List (Result (Error e) (Content a)))
                findContentInSection ( key, resolvedSection ) =
                    case resolvedSection of
                        ResolvedSection record ->
                            if key == firstKey then
                                case magicContent record.mainContent of
                                    Just content ->
                                        Ok content
                                            |> List.singleton
                                            |> Just

                                    Nothing ->
                                        if otherKeys == [] then
                                            case record.mainContent of
                                                [] ->
                                                    Nothing

                                                _ ->
                                                    Just record.mainContent
                                        else
                                            case record.mainContent of
                                                [ Ok (Json json) ] ->
                                                    json
                                                        |> JsonValue.getIn otherKeys
                                                        |> Result.toMaybe
                                                        |> Maybe.map
                                                            (Json >> Ok >> List.singleton)

                                                _ ->
                                                    otherKeys
                                                        |> contentForKeyPathInResolvedSections record.subsections
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


processSection : (String -> Result (Error e) (List JsonValue)) -> (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> ResolvedSection (Error e) a
processSection valueListForIdentifier evaluateComponent evaluateExpression sectionWrapper resolvedSubsections =
    let
        sectionRecord =
            case sectionWrapper of
                Section section ->
                    section

        expressionForString : String -> Maybe a
        expressionForString s =
            sectionRecord.inlineExpressions
                |> Dict.get s

        valueForIdentifier : String -> Result (Error e) JsonValue
        valueForIdentifier id =
            valueListForIdentifier id
                |> Result.map (JsonValue.ArrayValue)

        resolveExpressionString : String -> Maybe JsonValue
        resolveExpressionString s =
            expressionForString s
                |> Maybe.andThen (evaluateExpression valueForIdentifier >> Result.toMaybe)

        noRpcs : Content a -> ( Content a, List Rpc )
        noRpcs content =
            ( content, [] )

        processContent : Content a -> Result (Error e) ( Content a, List Rpc )
        processContent content =
            case content of
                Text text ->
                    Ok ( Text (mustache evaluateComponent resolveExpressionString text), [] )

                List contentItems ->
                    let
                        reduceItem : Content a -> Result (Error e) ( List (Content a), List Rpc ) -> Result (Error e) ( List (Content a), List Rpc )
                        reduceItem item result =
                            case result of
                                Ok ( items, rpcs ) ->
                                    case processContent item of
                                        Ok ( processedItem, newRpcs ) ->
                                            Ok ( processedItem :: items, newRpcs ++ rpcs )

                                        Err error ->
                                            Err error

                                Err error ->
                                    Err error

                        processedItems =
                            contentItems
                                |> List.foldl reduceItem (Ok ( [], [] ))
                    in
                        case processedItems of
                            Ok ( items, rpcs ) ->
                                Ok ( List items, rpcs )

                            Err error ->
                                Err error

                Code (Just "json") jsonSource ->
                    let
                        jsonResult =
                            jsonSource
                                |> Json.Decode.decodeString JsonValue.decoder
                    in
                        jsonResult
                            |> Result.map (Json >> noRpcs)
                            |> Result.mapError DecodingJson

                Code language codeText ->
                    Ok ( Code language (mustache evaluateComponent resolveExpressionString codeText), [] )

                Expressions input ->
                    case evaluateExpression valueForIdentifier input of
                        Ok json ->
                            case Rpc.fromJsonValue json of
                                Just rpc ->
                                    Ok ( Json json, [ rpc ] )

                                Nothing ->
                                    Ok ( Json json, [] )

                        Err error ->
                            Err (Evaluate error)

                content ->
                    Ok ( content, [] )

        processNextContent : Content a -> ( List (Result (Error e) (Content a)), List Rpc ) -> ( List (Result (Error e) (Content a)), List Rpc )
        processNextContent content ( results, rpcs ) =
            case processContent content of
                Ok ( newResult, newRpcs ) ->
                    ( Ok newResult :: results, newRpcs ++ rpcs )

                Err error ->
                    ( Err error :: results, [] )

        ( mainContent, rpcs ) =
            sectionRecord.mainContent
                |> List.foldl processNextContent ( [], [] )
                |> Tuple.mapFirst List.reverse
                |> Tuple.mapSecond List.reverse
    in
        ResolvedSection
            { mainContent = mainContent
            , subsections = resolvedSubsections
            , rpcs = rpcs
            }


flattenResults : List (Result e a) -> Result e (List a)
flattenResults results =
    case results of
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


nextProcessedSection : (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> ResolvedSection (Error e) a
nextProcessedSection evaluateComponent evaluateExpression contentToJson section prevResults =
    let
        contentForKey : String -> Result (Error e) (List (Content a))
        contentForKey key =
            key
                |> String.split "."
                |> contentForKeyPathInResolvedSections prevResults
                |> Maybe.map flattenResults
                |> Maybe.withDefault (Err (NoValueForIdentifier key))

        valueListForIdentifier : String -> Result (Error e) (List JsonValue)
        valueListForIdentifier key =
            contentForKey key
                |> Result.andThen (List.map (contentToJson >> Result.mapError (always CannotConvertContent)) >> flattenResults)

        subsections =
            case section of
                Section sectionRecord ->
                    sectionRecord.subsections

        resolvedSubsections =
            subsections
                |> List.foldl (foldProcessedSections evaluateComponent evaluateExpression contentToJson) prevResults
                |> List.take (List.length subsections)
    in
        processSection valueListForIdentifier evaluateComponent evaluateExpression section resolvedSubsections


foldProcessedSections : (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> List ( String, ResolvedSection (Error e) a )
foldProcessedSections evaluateComponent evaluateExpression contentToJson sectionWrapper prevResults =
    let
        title =
            case sectionWrapper of
                Section section ->
                    section.title

        resolvedSection : ResolvedSection (Error e) a
        resolvedSection =
            nextProcessedSection evaluateComponent evaluateExpression contentToJson sectionWrapper prevResults
    in
        ( title, resolvedSection ) :: prevResults


{-| Process a document with the inputted content and return a result
-}
processDocumentWith : (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Document a -> Dict String JsonValue -> Resolved e a
processDocumentWith evaluateComponent evaluateExpression contentToJson document inputtedContent =
    let
        resolvedSections : List ( String, ResolvedSection (Error e) a )
        resolvedSections =
            document.sections
                |> List.foldl (foldProcessedSections evaluateComponent evaluateExpression contentToJson) []
                |> List.reverse

        resolvedIntro =
            let
                introSection =
                    Section
                        { title = "intro"
                        , mainContent = document.introContent
                        , subsections = []
                        , inlineExpressions = document.introInlineExpressions
                        , urls = []
                        }
            in
                case nextProcessedSection evaluateComponent evaluateExpression contentToJson introSection resolvedSections of
                    ResolvedSection { mainContent } ->
                        mainContent
    in
        { sections = resolvedSections
        , intro = resolvedIntro
        , tests = []
        }


defaultEvaluateComponent : CustomElement -> Result (Error e) (Content a)
defaultEvaluateComponent element =
    let
        propsDict =
            element.props
                |> Dict.fromList

        getBoolProp name =
            case Dict.get name propsDict of
                Just (JsonValue.BoolValue bool) ->
                    bool

                _ ->
                    False

        getStringProp name =
            case Dict.get name propsDict of
                Just (JsonValue.StringValue string) ->
                    string

                _ ->
                    ""
    in
        case element.componentName of
            "Row" ->
                let
                    content =
                        getStringProp "children"
                in
                    Ok <| Code (Just "html") <| "<div class=\"flex flex-row\">" ++ content ++ "</div>"

            "Col" ->
                let
                    content =
                        getStringProp "children"
                in
                    Ok <| Code (Just "html") <| "<div class=\"flex flex-col flex-grow\">" ++ content ++ "</div>"

            "Button" ->
                let
                    primary =
                        getBoolProp "primary"

                    content =
                        getStringProp "children"

                    class : String
                    class =
                        [ "px-2 py-2 rounded"
                        , if primary then
                            "bg-blue-light"
                          else
                            "bg-grey-light"
                        ]
                            |> String.join " "
                in
                    Ok <| Code (Just "html") <| "<button class=\"" ++ class ++ "\">" ++ content ++ "</button>"

            "orange-button" ->
                Ok <| Code (Just "html") """<button class="bg-orange-light px-2 py-1 rounded">Click me</button>
"""

            _ ->
                Err <| NoValueForIdentifier element.componentName


{-| Process a document and return a result
-}
processDocument : ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Document a -> Resolved e a
processDocument evaluateExpression contentToJson document =
    processDocumentWith defaultEvaluateComponent evaluateExpression contentToJson document Dict.empty


findSectionContent : String -> Resolved e a -> Maybe (List (Result (Error e) (Content a)))
findSectionContent sectionTitle resolvedDocument =
    let
        hasTitle ( title, _ ) =
            title == sectionTitle

        maybeSection =
            resolvedDocument.sections
                |> List.filter hasTitle
                |> List.head
                |> Maybe.map (Tuple.second)
    in
        case maybeSection of
            Just (ResolvedSection record) ->
                Just record.mainContent

            _ ->
                Nothing


{-| Process a document with inputted content and return a result
-}
processComponent : String -> (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Document a -> Dict String JsonValue -> Maybe (List (Result (Error e) (Content a)))
processComponent sectionTitle evaluateComponent evaluateExpression contentToJson document inputtedContent =
    processDocumentWith evaluateComponent evaluateExpression contentToJson document inputtedContent
        |> findSectionContent sectionTitle
