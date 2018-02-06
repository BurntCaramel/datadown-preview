module Datadown.Parse
    exposing
        ( parseDocument
        )

{-| Parse Datadown documents


# Run Parsers

@docs parseDocument

-}

import Datadown exposing (Document, Section(..), Content(..))
import Dict
import Regex exposing (Regex)
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


mustacheExpressionRegex : Regex
mustacheExpressionRegex =
    Regex.regex "{{([^}]*)}}"


listMustacheExpressions : String -> List String
listMustacheExpressions input =
    let
        extractor : Regex.Match -> Maybe String
        extractor match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
    in
        Regex.find Regex.All mustacheExpressionRegex input
            |> List.filterMap extractor


processContentBlock : (String -> a) -> Block b i -> Maybe ( Content a, List ( String, a ) )
processContentBlock parseExpressions block =
    let
        maybeText =
            case block of
                PlainInlines inlines ->
                    Just (Inline.extractText inlines)

                Paragraph rawText inlines ->
                    Just (Inline.extractText inlines)

                _ ->
                    Nothing
    in
        case maybeText of
            Just text ->
                let
                    expressionPairs =
                        listMustacheExpressions text
                            |> List.map (\s -> ( s, parseExpressions s ))
                in
                    Just ( Datadown.Text text, expressionPairs )

            Nothing ->
                Nothing


addContentToDocument : Content a -> List ( String, a ) -> Document a -> Document a
addContentToDocument content expressions document =
    case document.sections of
        [] ->
            { document
                | introContent = content :: document.introContent
                , introInlineExpressions = Dict.union document.introInlineExpressions (Dict.fromList expressions)
            }

        (Section sectionRecord) :: sectionsTail ->
            case sectionRecord.subsections of
                [] ->
                    let
                        newSection =
                            Section
                                { sectionRecord
                                    | mainContent = content :: sectionRecord.mainContent
                                    , inlineExpressions = Dict.union sectionRecord.inlineExpressions (Dict.fromList expressions)
                                }
                    in
                        { document
                            | sections = newSection :: sectionsTail
                        }

                (Section subsectionRecord) :: subsectionsTail ->
                    let
                        newSubsection =
                            Section
                                { subsectionRecord
                                    | mainContent = content :: subsectionRecord.mainContent
                                    , inlineExpressions = Dict.union subsectionRecord.inlineExpressions (Dict.fromList expressions)
                                }

                        newSection =
                            Section
                                { sectionRecord
                                    | subsections = newSubsection :: subsectionsTail
                                }
                    in
                        { document
                            | sections = newSection :: sectionsTail
                        }


addSubsectionToDocument : Section a -> Document a -> Document a
addSubsectionToDocument subsection document =
    case document.sections of
        [] ->
            document

        (Section section) :: sectionsTail ->
            let
                newSection =
                    Section
                        { section
                            | subsections = subsection :: section.subsections
                        }
            in
                { document
                    | sections = newSection :: sectionsTail
                }


sectionWithTitle : String -> Section a
sectionWithTitle title =
    Section
        { title = title
        , mainContent = []
        , subsections = []
        , inlineExpressions = Dict.empty
        }


processDocumentBlock : (String -> a) -> Block b i -> Document a -> Document a
processDocumentBlock parseExpressions block document =
    case block of
        Heading text 1 inlines ->
            { document | title = Inline.extractText inlines }

        Heading text 2 inlines ->
            let
                title : String
                title =
                    inlines
                        |> Inline.extractText
                        |> String.trim
            in
                { document
                    | sections = (sectionWithTitle title) :: document.sections
                }

        Heading text 3 inlines ->
            let
                title : String
                title =
                    inlines
                        |> Inline.extractText
                        |> String.trim
            in
                addSubsectionToDocument (sectionWithTitle title) document

        Block.List listBlock items ->
            let
                contentAndExpressions : List ( Content a, List ( String, a ) )
                contentAndExpressions =
                    List.map (List.filterMap (processContentBlock parseExpressions)) items
                        |> List.concat

                contentItems =
                    contentAndExpressions
                        |> List.map Tuple.first

                expressions =
                    contentAndExpressions
                        |> List.concatMap Tuple.second
            in
                addContentToDocument (Datadown.List contentItems) expressions document

        BlockQuote blocks ->
            let
                innerDocument : Document a
                innerDocument =
                    processDocument parseExpressions blocks
            in
                addContentToDocument (Datadown.Quote innerDocument) [] document

        CodeBlock codeBlock text ->
            let
                expressionPairsFor text =
                    listMustacheExpressions text
                        |> List.map (\s -> ( s, parseExpressions s ))

                ( content, expressionPairs ) =
                    case codeBlock of
                        Block.Fenced isOpen fence ->
                            case fence.language of
                                Nothing ->
                                    ( Expressions (parseExpressions text), [] )

                                _ ->
                                    ( Code fence.language text, expressionPairsFor text )

                        _ ->
                            ( Code Nothing text, expressionPairsFor text )
            in
                addContentToDocument content expressionPairs document

        _ ->
            case processContentBlock parseExpressions block of
                Just ( content, expressions ) ->
                    addContentToDocument content expressions document

                Nothing ->
                    document


processDocument : (String -> a) -> List (Block b i) -> Document a
processDocument parseExpressions blocks =
    let
        initialDocument =
            { title = ""
            , introContent = []
            , introInlineExpressions = Dict.empty
            , sections = []
            }

        postSection section =
            case section of
                Section record ->
                    Section
                        { record
                            | mainContent = List.reverse record.mainContent
                        }

        postDocument document =
            { document
                | sections =
                    document.sections
                        |> List.map postSection
                        |> List.reverse
            }
    in
        blocks
            |> List.foldl (processDocumentBlock parseExpressions) initialDocument
            |> postDocument


{-| Parses a Datadown document

    parseDocument """


# Title

"""

-}
parseDocument : (String -> a) -> String -> Document a
parseDocument parseExpressions input =
    input
        |> Block.parse Nothing
        -- using Config.defaultOptions
        |> processDocument parseExpressions
