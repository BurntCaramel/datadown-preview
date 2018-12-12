module Datadown.Parse exposing (parseDocument)

{-| Parse Datadown documents


# Run Parsers

@docs parseDocument

-}

import Datadown exposing (Content(..), Document, Section(..))
import Dict
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))
import Regex exposing (Regex)


mustacheExpressionRegex : Regex
mustacheExpressionRegex =
    Regex.fromString "{{[#/^]?([^}]*)}}"
        |> Maybe.withDefault Regex.never


listMustacheExpressions : String -> List String
listMustacheExpressions input =
    let
        extractor : Regex.Match -> Maybe String
        extractor match =
            match.submatches
                |> List.head
                |> Maybe.withDefault Nothing
    in
    Regex.find mustacheExpressionRegex input
        |> List.filterMap extractor


type alias ProcessedInlines a =
    { content : Content a
    , expressionPairs : List ( String, a )
    , urls : List String
    }


processContentBlock : (String -> a) -> Block b i -> Maybe (ProcessedInlines a)
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

        urlFromInline inline =
            case inline of
                Link url maybeTitle innerInlines ->
                    [ url ]

                _ ->
                    []

        urls =
            case block of
                PlainInlines inlines ->
                    List.concatMap urlFromInline inlines

                Paragraph rawText inlines ->
                    List.concatMap urlFromInline inlines

                _ ->
                    []
    in
    case maybeText of
        Just text ->
            let
                expressionPairs : List ( String, a )
                expressionPairs =
                    listMustacheExpressions text
                        |> List.map (\s -> ( s, parseExpressions s ))
            in
            { content = Datadown.Text text
            , expressionPairs = expressionPairs
            , urls = urls
            }
                |> Just

        Nothing ->
            Nothing


addContentToSection : Content a -> List ( String, a ) -> List String -> Section a -> Section a
addContentToSection content expressions urls section =
    case section of
        Section sectionRecord ->
            case sectionRecord.subsections of
                [] ->
                    Section
                        { sectionRecord
                            | mainContent = content :: sectionRecord.mainContent
                            , inlineExpressions = Dict.union sectionRecord.inlineExpressions (Dict.fromList expressions)
                            , urls = List.append sectionRecord.urls urls
                        }

                subsection :: subsectionsTail ->
                    let
                        updatedSubsection =
                            addContentToSection content expressions urls subsection

                        newSection =
                            Section
                                { sectionRecord
                                    | subsections = updatedSubsection :: subsectionsTail
                                }
                    in
                    newSection


addContentToDocument : Content a -> List ( String, a ) -> List String -> Document a -> Document a
addContentToDocument content expressions urls document =
    case document.sections of
        [] ->
            { document
                | introContent = content :: document.introContent
                , introInlineExpressions = Dict.union document.introInlineExpressions (Dict.fromList expressions)
            }

        section :: sectionsTail ->
            let
                updatedSection =
                    addContentToSection content expressions urls section
            in
            { document
                | sections = updatedSection :: sectionsTail
            }


addSubsectionTo : Int -> Section a -> List (Section a) -> List (Section a)
addSubsectionTo depthRemaining newSection sections =
    if depthRemaining == 0 then
        newSection :: sections

    else
        case sections of
            [] ->
                sections

            (Section section) :: sectionsTail ->
                let
                    updatedSubsections =
                        addSubsectionTo (depthRemaining - 1) newSection section.subsections

                    updatedSection =
                        Section
                            { section
                                | subsections = updatedSubsections
                            }
                in
                updatedSection :: sectionsTail


addSubsectionToDocument : Section a -> Document a -> Document a
addSubsectionToDocument subsection document =
    case document.sections of
        [] ->
            document

        (Section section) :: sectionsTail ->
            let
                updatedSection =
                    Section
                        { section
                            | subsections = subsection :: section.subsections
                        }
            in
            { document
                | sections = updatedSection :: sectionsTail
            }


sectionWithTitle : String -> Section a
sectionWithTitle title =
    Section
        { title = title
        , mainContent = []
        , subsections = []
        , inlineExpressions = Dict.empty
        , urls = []
        }


parseMarkdownBlock : (String -> a) -> Block b i -> Document a -> Document a
parseMarkdownBlock parseExpressions block document =
    case block of
        Heading text 1 inlines ->
            { document | title = Inline.extractText inlines }

        Heading text level inlines ->
            let
                title : String
                title =
                    inlines
                        |> Inline.extractText
                        |> String.trim

                newSection =
                    sectionWithTitle title

                updatedSections =
                    addSubsectionTo (level - 2) newSection document.sections
            in
            { document
                | sections = updatedSections
            }

        Block.List listBlock items ->
            let
                contentAndExpressions : List (ProcessedInlines a)
                contentAndExpressions =
                    List.map (List.filterMap (processContentBlock parseExpressions)) items
                        |> List.concat

                parseContentAndQualifier content =
                    case content of
                        Datadown.Text text ->
                            if String.startsWith "[x] " text then
                                ( Datadown.Text <| String.dropLeft 4 text, Datadown.Flag True )

                            else if String.startsWith "[ ] " text then
                                ( Datadown.Text <| String.dropLeft 4 text, Datadown.Flag False )

                            else
                                ( Datadown.Text text, Datadown.Always )

                        _ ->
                            ( content, Datadown.Always )

                contentItems =
                    contentAndExpressions
                        |> List.map (.content >> parseContentAndQualifier)

                expressions =
                    contentAndExpressions
                        |> List.concatMap .expressionPairs

                urls =
                    contentAndExpressions
                        |> List.concatMap .urls
            in
            addContentToDocument (Datadown.List contentItems) expressions urls document

        BlockQuote blocks ->
            let
                innerDocument : Document a
                innerDocument =
                    parseMarkdown parseExpressions blocks
            in
            addContentToDocument (Datadown.Quote innerDocument) [] [] document

        CodeBlock codeBlock text ->
            let
                expressionPairsFor inputText =
                    listMustacheExpressions inputText
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
            addContentToDocument content expressionPairs [] document

        _ ->
            case processContentBlock parseExpressions block of
                Just { content, expressionPairs, urls } ->
                    addContentToDocument content expressionPairs urls document

                Nothing ->
                    document


parseMarkdown : (String -> a) -> List (Block b i) -> Document a
parseMarkdown parseExpressions blocks =
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
                , introContent =
                    document.introContent
                        |> List.reverse
            }
    in
    blocks
        |> List.foldl (parseMarkdownBlock parseExpressions) initialDocument
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
        |> parseMarkdown parseExpressions
