module Datadown.Parse
    exposing
        ( parseDocument
        , parseSection
        )

{-| Parse Datadown documents


# Run Parsers

@docs parseDocument, parseSection

-}

import Datadown exposing (Document, Section, Content(..))
import Dict
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


{-| Parses a Datadown section

    parseSection """


## Section

  - first
  - second
  - third
    """

-}
parseSection : List (Block b i) -> Section a
parseSection blocks =
    { title = ""
    , mainContent = Nothing
    , secondaryContent = Dict.empty
    }


processContentBlock : Block b i -> Maybe (Content a)
processContentBlock block =
    case block of
        PlainInlines inlines ->
            Just (Datadown.Text (Inline.extractText inlines))

        Paragraph rawText inlines ->
            Just (Datadown.Text (Inline.extractText inlines))

        _ ->
            Nothing


addContentToDocument : Content a -> Document a -> Document a
addContentToDocument content document =
    let
        newSections : List (Section a)
        newSections =
            case document.sections of
                [] ->
                    []

                -- Error, content must belong to an open section
                section :: sectionsTail ->
                    { section | mainContent = Just content } :: sectionsTail
    in
        { document
            | sections = newSections
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
                    | sections = { title = title, mainContent = Nothing, secondaryContent = Dict.empty } :: document.sections
                }

        Block.List listBlock items ->
            let
                contentItems : List (Content a)
                contentItems =
                    List.map (List.filterMap processContentBlock) items
                        |> List.concat
            in
                addContentToDocument (Datadown.List contentItems) document

        BlockQuote blocks ->
            let
                innerDocument : Document a
                innerDocument =
                    processDocument parseExpressions blocks
            in
                addContentToDocument (Datadown.Quote innerDocument) document

        CodeBlock codeBlock text ->
            let
                code =
                    case codeBlock of
                        Block.Fenced isOpen fence ->
                            case fence.language of
                                Nothing ->
                                    Expressions (parseExpressions text)

                                _ ->
                                    Code fence.language text

                        _ ->
                            Code Nothing text
            in
                addContentToDocument code document

        _ ->
            case processContentBlock block of
                Just content ->
                    addContentToDocument content document

                Nothing ->
                    document


processDocument : (String -> a) -> List (Block b i) -> Document a
processDocument parseExpressions blocks =
    let
        initialDocument =
            { title = ""
            , sections = []
            }
    in
        blocks
            |> List.foldl (processDocumentBlock parseExpressions) initialDocument
            |> \d -> { d | sections = d.sections |> List.reverse }


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
