module Datadown.Process exposing
    ( Error(..)
    , processDocument
    , Resolved, ResolvedSection(..), processComponent, processDocumentWith
    )

{-| Process


## Types

@docs Error


## Functions

@docs processDocument

-}

import Datadown exposing (Content(..), Document, ListItemQualifier, Section(..))
import Datadown.Rpc as Rpc exposing (Rpc)
import Dict exposing (Dict(..))
import Json.Decode
import Json.Value exposing (JsonValue)
import Regex exposing (Regex)


{-| Error after processing, possibly from evaluating expressions
-}
type Error e
    = NoValueForIdentifier String
    | NoValueForKeyPath (List String)
    | CannotConvertContent
    | NoSection String
    | UnknownKind
    | Evaluate e
    | EvaluatingExpression String e
    | UnknownExpression
    | DecodingJson Json.Decode.Error
    | Multiple (List (Error e))


type alias Resolved e a =
    { title : String
    , sections : List ( String, ResolvedSection (Error e) a )
    , intro : List (Result (Error e) (Content a))
    , tests : List (Document a)
    }


type ResolvedSection e a
    = ResolvedSection
        { mainContent : List (Result e (Content a))
        , subsections : List ( String, ResolvedSection e a )
        , rpcs : List (Rpc String)
        }


type alias CustomElement =
    { language : Maybe String
    , componentName : String
    , props : List ( String, JsonValue )
    }


mustacheVariableRegex : Regex
mustacheVariableRegex =
    Regex.fromString "{{([^}]*)}}"
        |> Maybe.withDefault Regex.never


mustacheSectionRegex : Regex
mustacheSectionRegex =
    Regex.fromString "{{#\\s*([^}]+?)\\s*}}([\\S\\s]+?){{/\\s*\\1\\s*}}"
        |> Maybe.withDefault Regex.never


mustacheSectionNegativeRegex : Regex
mustacheSectionNegativeRegex =
    Regex.fromString "{{^\\s*([^}]+?)\\s*}}([\\S\\s]+?){{/\\s*\\1\\s*}}"
        |> Maybe.withDefault Regex.never


customElementOpenRegex : Regex
customElementOpenRegex =
    Regex.fromString "<([A-Z]\\w*)([^>]*)>([\\S\\s]+)"
        |> Maybe.withDefault Regex.never


attributeRegex : Regex
attributeRegex =
    Regex.fromString "\\s*(\\w[^=\"\\s]+)(?:=\"(\\w*)\")?"
        |> Maybe.withDefault Regex.never


customElementCloseRegex : String -> Regex
customElementCloseRegex name =
    "</"
        ++ name
        ++ ">"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


jsonToString : JsonValue -> String
jsonToString json =
    case json of
        Json.Value.StringValue s ->
            s

        Json.Value.NumericValue f ->
            String.fromFloat f

        -- TODO: turn to real Markdown?
        Json.Value.ArrayValue items ->
            items
                |> List.map jsonToString
                |> String.join "\n"

        Json.Value.BoolValue bool ->
            if bool then
                "👍"

            else
                "👎"

        _ ->
            Debug.toString json


matchToAttribute : Regex.Match -> Maybe ( String, JsonValue )
matchToAttribute match =
    case match.submatches of
        (Just key) :: (Just value) :: [] ->
            Just ( key, Json.Value.StringValue value )

        (Just key) :: Nothing :: [] ->
            Just ( key, Json.Value.BoolValue True )

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
                                        |> Regex.find attributeRegex
                                        |> List.filterMap matchToAttribute

                                Nothing ->
                                    []

                        ( customElement, rest ) =
                            case maybeContent of
                                Just content ->
                                    case Regex.splitAtMost 1 (customElementCloseRegex componentName) content of
                                        childrenSource :: restInput :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource

                                                el =
                                                    CustomElement Nothing componentName (props ++ [ ( "children", Json.Value.StringValue children ) ])

                                                newRest =
                                                    restInput
                                                        |> processCustomElements evaluateComponent
                                            in
                                                ( el, newRest )

                                        childrenSource :: [] ->
                                            let
                                                children =
                                                    processCustomElements evaluateComponent childrenSource

                                                el =
                                                    CustomElement Nothing componentName (props ++ [ ( "children", Json.Value.StringValue children ) ])
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
                            Debug.toString error

                _ ->
                    "?"
    in
    Regex.replaceAtMost 1 customElementOpenRegex replacer input


mustache : (CustomElement -> Result (Error e) (Content a)) -> (Maybe JsonValue -> String -> Maybe JsonValue) -> Maybe JsonValue -> String -> String
mustache evaluateComponent resolveVariable =
    let
        sectionReplacer : Maybe JsonValue -> (Bool -> Bool) -> Regex.Match -> String
        sectionReplacer context transformBool match =
            let
                maybeValue : Maybe JsonValue
                maybeValue =
                    match.submatches
                        |> List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.andThen (resolveVariable context)

                inner : String
                inner =
                    case match.submatches of
                        _ :: (Just string) :: [] ->
                            string

                        _ ->
                            ""

                passes json =
                    case json of
                        Json.Value.BoolValue False ->
                            False

                        Json.Value.NullValue ->
                            False

                        _ ->
                            True

                values =
                    case maybeValue of
                        Nothing ->
                            []

                        Just (Json.Value.ArrayValue items) ->
                            items
                                |> List.filter passes

                        Just (Json.Value.BoolValue False) ->
                            []

                        Just Json.Value.NullValue ->
                            []

                        Just value ->
                            [ value ]
            in
            values
                |> List.map (\value -> process (Just value) inner)
                |> String.join "\n"

        variableReplacer : Maybe JsonValue -> Regex.Match -> String
        variableReplacer context match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault ""
                |> resolveVariable context
                |> Maybe.map jsonToString
                |> Maybe.withDefault "?"

        process context input =
            input
                |> processCustomElements evaluateComponent
                |> Regex.replace mustacheSectionRegex (sectionReplacer context identity)
                |> Regex.replace mustacheSectionNegativeRegex (sectionReplacer context not)
                |> Regex.replace mustacheVariableRegex (variableReplacer context)
                |> String.trim
    in
    process


contentForKeyPathInResolvedSections : List ( String, ResolvedSection (Error e) a ) -> List String -> Maybe (List (Result (Error e) (Content a)))
contentForKeyPathInResolvedSections resolvedSections keyPath =
    case keyPath of
        firstKey :: otherKeys ->
            let
                magicContent : Result (Error e) (Content a) -> Maybe (Content a)
                magicContent content =
                    case content of
                        Ok (Json json) ->
                            case Rpc.fromJsonValue json of
                                Just rpc ->
                                    Just (Reference rpc.id otherKeys json)

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                resolveContentResult : Result (Error e) (Content a) -> List (Result (Error e) (Content a))
                resolveContentResult contentResult =
                    case magicContent contentResult of
                        Just content ->
                            [ Ok content ]

                        Nothing ->
                            case contentResult of
                                Ok (Json json) ->
                                    json
                                        |> Json.Value.getIn otherKeys
                                        |> Result.mapError (always (NoValueForKeyPath keyPath))
                                        |> Result.map Json
                                        |> List.singleton

                                Ok (List items) ->
                                    items
                                        |> List.map (Tuple.first >> Ok)

                                _ ->
                                    if otherKeys == [] then
                                        [ contentResult ]

                                    else
                                        [ Err (NoValueForKeyPath keyPath) ]

                findContentInSection : ( String, ResolvedSection (Error e) a ) -> Maybe (List (Result (Error e) (Content a)))
                findContentInSection ( fullKey, resolvedSection ) =
                    case resolvedSection of
                        ResolvedSection record ->
                            let
                                maybeBaseKey =
                                    case String.split ":" fullKey of
                                        baseKey :: kind :: [] ->
                                            Just baseKey

                                        baseKey :: [] ->
                                            Just baseKey

                                        _ ->
                                            Nothing
                            in
                            case maybeBaseKey of
                                Just baseKey ->
                                    if baseKey == firstKey then
                                        case record.mainContent of
                                            [] ->
                                                otherKeys
                                                    |> contentForKeyPathInResolvedSections record.subsections

                                            _ ->
                                                Just (List.concatMap resolveContentResult record.mainContent)

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                findInSections innerResolvedSections =
                    case innerResolvedSections of
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


processSection : (Maybe JsonValue -> String -> Result (Error e) (List JsonValue)) -> (CustomElement -> Result (Error e) (Content a)) -> ((String -> Result (Error e) JsonValue) -> a -> Result e JsonValue) -> Section a -> List ( String, ResolvedSection (Error e) a ) -> ResolvedSection (Error e) a
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

        valueForIdentifier : Maybe JsonValue -> String -> Result (Error e) JsonValue
        valueForIdentifier context id =
            valueListForIdentifier context id
                |> Result.map Json.Value.ArrayValue

        resolveExpressionString : Maybe JsonValue -> String -> Maybe JsonValue
        resolveExpressionString context s =
            expressionForString s
                |> Maybe.andThen (evaluateExpression (valueForIdentifier context) >> Result.toMaybe)

        noRpcs : Content a -> ( Content a, List (Rpc String) )
        noRpcs content =
            ( content, [] )

        processContent : Content a -> Result (Error e) ( Content a, List (Rpc String) )
        processContent content =
            case content of
                Text text ->
                    Ok ( Text (mustache evaluateComponent resolveExpressionString Nothing text), [] )

                List contentItems ->
                    let
                        reduceItem : ( Content a, ListItemQualifier a ) -> Result (Error e) ( List ( Content a, ListItemQualifier a ), List (Rpc String) ) -> Result (Error e) ( List ( Content a, ListItemQualifier a ), List (Rpc String) )
                        reduceItem ( item, qualifier ) result =
                            case result of
                                Ok ( items, rpcs ) ->
                                    let
                                        skip =
                                            case qualifier of
                                                Datadown.Always ->
                                                    False

                                                Datadown.Flag bool ->
                                                    not bool

                                                Datadown.Expression expression ->
                                                    True
                                    in
                                        if skip then
                                            Ok ( items, rpcs )

                                    else
                                        case processContent item of
                                            Ok ( processedItem, processedRpcs ) ->
                                                Ok ( ( processedItem, Datadown.Always ) :: items, processedRpcs ++ rpcs )

                                            Err error ->
                                                Err error

                                Err error ->
                                    Err error

                        processedItems =
                            contentItems
                                |> List.foldr reduceItem (Ok ( [], [] ))
                    in
                        case processedItems of
                            Ok ( items, rpcs ) ->
                                Ok ( List items, rpcs )

                            Err error ->
                                Err error

                Code (Just "graphql") graphqlSource ->
                    let
                        rpc =
                            Rpc.graphQL graphqlSource
                    in
                    Ok ( content, [ rpc ] )

                Code (Just "json") jsonSource ->
                    let
                        jsonResult =
                            jsonSource
                                |> Json.Decode.decodeString Json.Value.decoder
                    in
                    jsonResult
                        |> Result.map (Json >> noRpcs)
                        |> Result.mapError DecodingJson

                Code language codeText ->
                    Ok ( Code language (mustache evaluateComponent resolveExpressionString Nothing codeText), [] )

                Expressions input ->
                    case evaluateExpression (valueForIdentifier Nothing) input of
                        Ok json ->
                            case Rpc.fromJsonValue json of
                                Just rpc ->
                                    Ok ( Json json, [ rpc ] )

                                Nothing ->
                                    Ok ( Json json, [] )

                        Err error ->
                            Err (Evaluate error)

                otherContent ->
                    Ok ( otherContent, [] )

        processNextContent : Content a -> ( List (Result (Error e) (Content a)), List (Rpc String) ) -> ( List (Result (Error e) (Content a)), List (Rpc String) )
        processNextContent content ( results, rpcs ) =
            case processContent content of
                Ok ( newResult, processedRpcs ) ->
                    ( Ok newResult :: results, processedRpcs ++ rpcs )

                Err error ->
                    ( Err error :: results, [] )

        ( mainContent, newRpcs ) =
            sectionRecord.mainContent
                |> List.foldl processNextContent ( [], [] )
                |> Tuple.mapFirst List.reverse
                |> Tuple.mapSecond List.reverse
    in
    ResolvedSection
        { mainContent = mainContent
        , subsections = resolvedSubsections
        , rpcs = newRpcs
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
        contentForKey : Maybe JsonValue -> String -> Result (Error e) (List (Content a))
        contentForKey maybeContext key =
            let
                keyPath =
                    key
                        |> String.split "."

                conformPath pathIn =
                    case pathIn of
                        [ "" ] ->
                            []

                        path ->
                            path
            in
            case keyPath of
                "" :: otherKeys ->
                    case maybeContext of
                        Just context ->
                            context
                                |> Json.Value.getIn (conformPath otherKeys)
                                |> Result.mapError (always (NoValueForKeyPath keyPath))
                                |> Result.map (Json >> List.singleton)

                        Nothing ->
                            Err (NoValueForKeyPath keyPath)

                _ ->
                    keyPath
                        |> contentForKeyPathInResolvedSections prevResults
                        |> Maybe.map flattenResults
                        |> Maybe.withDefault (Err (NoValueForKeyPath keyPath))

        valueListForIdentifier : Maybe JsonValue -> String -> Result (Error e) (List JsonValue)
        valueListForIdentifier context key =
            contentForKey context key
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
    { title = document.title
    , sections = resolvedSections
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
                Just (Json.Value.BoolValue bool) ->
                    bool

                _ ->
                    False

        getStringProp name =
            case Dict.get name propsDict of
                Just (Json.Value.StringValue string) ->
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
                |> Maybe.map Tuple.second
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
