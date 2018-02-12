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
    | Http Http.Error
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


type alias CustomElement =
    { language : Maybe String
    , componentName : String
    , props : List (String, JsonValue)
    }


mustacheVariableRegex : Regex
mustacheVariableRegex =
    Regex.regex "{{([^}]*)}}"


customElementOpenRegex : Regex
customElementOpenRegex =
    Regex.regex "<([A-Z]\\w*)([^>]*)>([\\S\\s]+)"


attributeRegex : Regex
attributeRegex =
    Regex.regex "\\s*(\\w[^=\"\\s]+)(?:=\"(\\w*)\")?"


customElementCloseRegex : String -> Regex
customElementCloseRegex name =
    "</" ++ (Regex.escape name) ++ ">"
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


matchToAttribute : Regex.Match -> Maybe (String, JsonValue)
matchToAttribute match =
    case match.submatches of
        Just key :: Just value :: [] ->
            Just (key, JsonValue.StringValue value)
        
        Just key :: Nothing :: [] ->
            Just (key, JsonValue.BoolValue True)
        
        _ ->
            Nothing


processCustomElements : (CustomElement -> Result (Error e) (Content a)) -> String -> String
processCustomElements evaluateComponent input =
    let
        replacer : Regex.Match -> String
        replacer match =
            case match.submatches of
                Just componentName :: maybeAttributesSource :: maybeContent :: [] ->
                    let
                        props =
                            case maybeAttributesSource of
                                Just source ->
                                    source
                                        |> Regex.find Regex.All attributeRegex
                                        |> List.filterMap matchToAttribute

                                Nothing ->
                                    []

                        (customElement, rest) =
                            case maybeContent of
                                Just content ->
                                    case Regex.split (Regex.AtMost 1) (customElementCloseRegex componentName) content of
                                        childrenSource :: restInput :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource

                                                el =
                                                    CustomElement Nothing componentName (props ++ [("children", JsonValue.StringValue children)])

                                                rest =
                                                    restInput
                                                        |> Debug.log ("rest input | " ++ content)
                                                        |> processCustomElements evaluateComponent
                                            in
                                                (el, rest)
                                        
                                        childrenSource :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource
                                                
                                                el =
                                                    CustomElement Nothing componentName (props ++ [("children", JsonValue.StringValue children)])
                                            in
                                                (el, "")
                                        
                                        _ ->
                                            (CustomElement Nothing componentName props, "")
                                
                                Nothing ->
                                    (CustomElement Nothing componentName props, "")
                        
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
        input
            |> processCustomElements evaluateComponent
            |> Regex.replace Regex.All mustacheVariableRegex replacer


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


processSection : (String -> Result (Error e) (List JsonValue)) -> (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> Section a -> List (Result (Error e) (Content a))
processSection valueListForIdentifier evaluateComponent evaluateExpression sectionWrapper =
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
                    Ok (Text (mustache evaluateComponent resolveExpressionString text))

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

                Code (Just "json") jsonSource ->
                    let
                        jsonResult =
                            jsonSource
                                |> Json.Decode.decodeString JsonValue.decoder
                    in
                        jsonResult
                            |> Result.map Json
                            |> Result.mapError DecodingJson

                Code language codeText ->
                    Ok (Code language (mustache evaluateComponent resolveExpressionString codeText))

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


nextProcessedSection : (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> List (Result (Error e) (Content a))
nextProcessedSection evaluateComponent evaluateExpression contentToJson section prevResults =
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
        processSection valueForIdentifier evaluateComponent evaluateExpression section


foldProcessedSections : (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> (Content a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> List ( String, ResolvedSection (Error e) a )
foldProcessedSections evaluateComponent evaluateExpression contentToJson sectionWrapper prevResults =
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
            nextProcessedSection evaluateComponent evaluateExpression contentToJson sectionWrapper prevResults

        resolvedSubsections =
            subsections
                |> List.foldl (foldProcessedSections evaluateComponent evaluateExpression contentToJson) prevResults
                |> List.take (List.length subsections)

        resolvedSection =
            ResolvedSection
                { mainContent = resolvedMainContent
                , subsections = resolvedSubsections
                }
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
                nextProcessedSection evaluateComponent evaluateExpression contentToJson introSection resolvedSections
                    |> List.reverse
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
                content = getStringProp "children"
            in
                Ok <| Code (Just "html") <| "<div class=\"flex flex-row\">" ++ content ++ "</div>"
        
        "Col" ->
            let
                content = getStringProp "children"
            in
                Ok <| Code (Just "html") <| "<div class=\"flex flex-col flex-grow\">" ++ content ++ "</div>"
        
        "Button" ->
            let 
                primary = getBoolProp "primary"
                
                content = getStringProp "children"
                
                class : String
                class =
                    [ "px-2 py-2 rounded"
                    , if primary then "bg-blue-light" else "bg-grey-light"
                    ] |> String.join " "
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
        hasTitle (title, _) =
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
