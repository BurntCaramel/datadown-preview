module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, rows, attribute, value)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time)
import Date
import Array exposing (Array)
import Dict exposing (Dict)
import Datadown exposing (Document, Section(..), Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process as Process exposing (processDocument, Error, Resolved, ResolvedSection(..))
import JsonValue exposing (JsonValue(..))
import Parser exposing (Error)
import Preview.Json
import Preview.Html
import Expressions.Tokenize as Tokenize exposing (tokenize, Token(..))
import Expressions.Evaluate as Evaluate exposing (evaulateTokenLines)
import Samples.Welcome
import Samples.Clock


type alias Model =
    { documentSources : Array String
    , nav : Nav
    , now : Time
    , sectionInputs : Dict String JsonValue
    }


type Nav
    = DocumentsList
    | Document Int


type Error
    = Parser Parser.Error
    | Evaluate Evaluate.Error
    | Process Process.Error


type alias DisplayOptions =
    { compact : Bool
    , hideNoContent : Bool
    , processDocument : Document (Result Error (List (List Token))) -> Resolved Evaluate.Error (Result Error (List (List Token)))
    , sectionInputs : Dict String JsonValue
    }


parseExpressions : String -> Result Error (List (List Token))
parseExpressions input =
    case tokenize input of
        Err parserError ->
            Err (Parser parserError)

        Ok tokens ->
            Ok tokens


builtInValueFromModel : Model -> String -> Maybe JsonValue
builtInValueFromModel model key =
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


valueFromModel : Model -> String -> Maybe JsonValue
valueFromModel model key =
    case Dict.get key model.sectionInputs of
        Just value ->
            Just value
        
        Nothing ->
            builtInValueFromModel model key


evaluateExpressions : Model -> (String -> Result (Process.Error Evaluate.Error) JsonValue) -> Result Error (List (List Token)) -> Result Evaluate.Error JsonValue
evaluateExpressions model resolveFromDocument parsedExpressions =
    let
        resolveWithModel key =
            case resolveFromDocument key of
                Ok value ->
                    Ok value
                
                Err error ->
                    valueFromModel model key
                        |> Result.fromMaybe (Err error)
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
    { documentSources =
        [ Samples.Welcome.source
        , Samples.Clock.source
        , "# Now your turn!"
        ]
            |> Array.fromList
    , nav = Document 0
    , now = 0
    , sectionInputs = Dict.empty
    }
        ! [ Cmd.none
          ]


type Message
    = ChangeDocumentSource String
    | GoToDocumentsList
    | GoToPreviousDocument
    | GoToNextDocument
    | GoToDocumentAtIndex Int
    | NewDocument
    | ChangeSectionInput String String
    | Time Time


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ChangeDocumentSource newInput ->
            let
                documentSources =
                    case model.nav of
                        Document index ->
                            model.documentSources
                                |> Array.set index newInput
                    
                        _ ->
                            model.documentSources
            in
                ( { model | documentSources = documentSources }, Cmd.none )
        
        GoToDocumentsList ->
            ( { model | nav = DocumentsList }, Cmd.none )
        
        GoToPreviousDocument ->
            let
                newIndex =
                    case model.nav of
                        DocumentsList ->
                            0
                        
                        Document index ->
                            max 0 (index - 1)
            in
                ( { model | nav = Document newIndex }, Cmd.none )
        
        GoToNextDocument ->
            let
                maxIndex =
                    Array.length model.documentSources - 1
                
                newIndex =
                    case model.nav of
                        DocumentsList ->
                            maxIndex
                        
                        Document index ->
                            min maxIndex (index + 1)
            in
                ( { model | nav = Document newIndex }, Cmd.none )
        
        GoToDocumentAtIndex index ->
            ( { model | nav = Document index }, Cmd.none )
        
        NewDocument ->
            let
                currentIndex =
                    case model.nav of
                        DocumentsList ->
                            0
                        
                        Document index ->
                            index

                newDocumentSource =
                    "# Untitled"

                prefix =
                    Array.slice 0 currentIndex model.documentSources
                        |> Array.push newDocumentSource
                
                suffix =
                    Array.slice currentIndex (Array.length model.documentSources) model.documentSources
                
                documentSources =
                    Array.append prefix suffix
            in
                ( { model | documentSources = documentSources }, Cmd.none )

        ChangeSectionInput sectionTitle newInput ->
            let
                newValue =
                    StringValue newInput
                
                newSectionInputs =
                    Dict.insert sectionTitle newValue model.sectionInputs
            in
                ( { model | sectionInputs = newSectionInputs }, Cmd.none )
                

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


viewCode : DisplayOptions -> Maybe String -> String -> Html Message
viewCode options language source =
    let
        previewHtml =
            div [] (viewCodePreview language source)
    in
        if not options.compact && showCodeForLanguage language then
            div []
                [ previewHtml
                , details [ class "mt-2" ] 
                    [ summary [ class "px-2 py-1 font-mono text-xs italic text-purple-darker bg-purple-lightest" ]
                        [ text "Source" ]
                    , pre [ class "overflow-auto px-2 py-2 text-purple-darker bg-purple-lightest" ]
                        [ code [ class "font-mono text-xs" ] [ text source ] ]
                    ]
                ]
        else
            div [] [ previewHtml ]


viewContent : DisplayOptions -> Content (Result Error (List (List Token))) -> Html Message
viewContent options content =
    case content of
        Text s ->
            div [ class "font-sans w-full" ] [ text s ]

        Code language source ->
            viewCode options language source

        Json json ->
            Preview.Json.viewJson json

        Expressions expressionsResult ->
            case expressionsResult of
                Err expressionsError ->
                    h3 [ class "px-2 py-1 text-white bg-red-dark" ] [ text <| toString expressionsError ]

                Ok expressions ->
                    pre [ class "px-2 py-2 text-teal-darker bg-teal-lightest" ]
                        [ code [ class "font-mono text-sm" ] (List.map viewExpression expressions) ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent options item ]) items)

        Quote document ->
            let
                resolved =
                    options.processDocument document
            in
                resolved.sections
                    |> List.map makeSectionViewModel
                    |> List.map (viewSection [] options)
                    |> div [ class "pl-6 border-l border-teal" ]


viewContentResult : DisplayOptions -> Result (Process.Error Evaluate.Error) (Content (Result Error (List (List Token)))) -> Html Message
viewContentResult options contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [ class "mb-3 px-2 py-1 text-white bg-red-dark" ] [ text (toString error) ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent options content ]


viewContentResults : DisplayOptions -> List String -> String -> List (Result (Process.Error Evaluate.Error) (Content (Result Error (List (List Token))))) -> List (Html Message)
viewContentResults options parentPath sectionTitle contentResults =
    case contentResults of
        [] ->
            if options.hideNoContent then
                [ text "" ]
            else
                let
                    key =
                        sectionTitle :: parentPath
                            |> List.reverse
                            |> String.join "."

                    stringValue =
                        case Dict.get key options.sectionInputs of
                            Nothing ->
                                ""
                            
                            Just (StringValue s) ->
                                s
                            
                            json ->
                                toString json
                in
                    [ textarea [ value stringValue, onInput (ChangeSectionInput key), rows 3, class "w-full px-2 py-2 bg-blue-lightest border border-blue" ] []
                    ]

        _ ->
            contentResults
                |> List.map (viewContentResult options)


type alias SectionViewModel e =
    { title : String
    , mainContent : List (Result e (Content (Result Error (List (List Token)))))
    , subsections : List (String, ResolvedSection e (Result Error (List (List Token))))
    }


makeSectionViewModel : ( String, ResolvedSection e (Result Error (List (List Token))) ) -> SectionViewModel e
makeSectionViewModel ( title, resolvedSection ) =
    case resolvedSection of
        ResolvedSection record ->
            SectionViewModel title record.mainContent record.subsections


viewSectionTitle : Int -> List (Html.Html msg) -> Html.Html msg
viewSectionTitle level =
    case level of
        0 ->
            h2 [ class "mb-2 text-xl text-blue-dark" ]
        
        1 ->
            h3 [ class "mb-2 text-lg text-blue-dark" ]
        
        2 ->
            h4 [ class "mb-2 text-base text-blue-dark" ]
        
        _ ->
            h4 [ class "mb-2 text-sm text-blue-dark" ]


viewSection : List String -> DisplayOptions -> SectionViewModel (Process.Error Evaluate.Error) -> Html Message
viewSection sectionPath options { title, mainContent, subsections } =
    details [ attribute "open" "" ]
        [ summary []
            [ viewSectionTitle (List.length sectionPath) [ text title ]
            ]
        , mainContent
            |> viewContentResults options sectionPath title
            |> div []
        , subsections
            |> List.map makeSectionViewModel
            |> List.map (viewSection (title :: sectionPath) options)
            |> div [ class "ml-2" ]

        -- , div [] [ text (variables |> toString) ]
        ]


viewFontAwesomeIcon : String -> Html Message
viewFontAwesomeIcon id =
    i [ class ("fas fa-" ++ id) ] []


viewDocumentNavigation : Model -> Html Message
viewDocumentNavigation model =
    div [ class "" ]
        [ case model.nav of
            DocumentsList ->
                div [ class "flex justify-between" ]
                    [ button [ onClick NewDocument, class "px-2 py-1 text-green-dark bg-green-lightest border border-green-lighter rounded-sm" ] [ viewFontAwesomeIcon "plus", text " New" ]
                    , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest border border-purple-lighter rounded-sm" ] [ viewFontAwesomeIcon "share", text " Export" ]
                    ]
            
            Document index ->
                div [ class "self-end flex-shrink flex items-center border border-green-lighter rounded-sm" ]
                    [ button [ onClick GoToPreviousDocument, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "arrow-left" ]
                    , div [ class "py-1 text-center font-bold text-green-dark bg-green-lightest" ] [ text (index + 1 |> toString) ]
                    , button [ onClick GoToNextDocument, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "arrow-right" ]
                    , button [ onClick GoToDocumentsList, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "bars" ]
                    ]
        ]


viewDocuments : Model -> Html Message
viewDocuments model =
    let
        viewDocument index documentSource =
            let
                firstLine =
                    documentSource
                        |> String.trim
                        |> String.lines
                        |> List.head
                
                maybeTitle =
                    Maybe.map (parseDocument parseExpressions >> .title) firstLine
                        |> Maybe.andThen (\line -> if String.isEmpty line then Nothing else Just line)
                
                titleHtml =
                    case maybeTitle of
                        Just input ->
                            text input
                        
                        Nothing ->
                            em [] [ text "Untitled" ]

                document =
                    parseDocument parseExpressions documentSource
            in
                h2 [ class "" ]
                    [ button [ class "w-full px-4 py-2 text-left text-3xl font-bold text-blue bg-white border-b border-blue-lighter", onClick (GoToDocumentAtIndex index) ]
                        [ titleHtml ]
                    ]
    in
        div [ class "max-w-lg mx-auto flex-1" ]
            [ div [ class "p-4" ] [ viewDocumentNavigation model ]
            , div [ class "border-t border-blue-lighter" ]
                (Array.indexedMap viewDocument model.documentSources |> Array.toList)
            ]


viewDocumentSource : Model -> String -> Html Message
viewDocumentSource model documentSource =
    let
        document : Document (Result Error (List (List Token)))
        document =
            parseDocument parseExpressions documentSource
        
        processDocument_ =
            processDocument (evaluateExpressions model) (contentToJson model)

        resolved : Resolved Evaluate.Error (Result Error (List (List Token)))
        resolved =
            processDocument_ document
        
        displayOptions =
            { compact = False
            , hideNoContent = False
            , processDocument = processDocument_
            , sectionInputs = model.sectionInputs
            }

        resultsHtml : List (Html Message)
        resultsHtml =
            resolved.sections
                |> List.map makeSectionViewModel
                |> List.map (viewSection [] displayOptions)
        
        introHtml : List (Html Message)
        introHtml =
            resolved.intro
                |> viewContentResults
                    { compact = True
                    , hideNoContent = True
                    , processDocument = processDocument_
                    , sectionInputs = model.sectionInputs
                    }
                    []
                    "intro"
    in
        div [ class "flex-1 flex flex-wrap h-screen" ]
            [ div [ class "flex-1 overflow-auto mb-8 p-4 pb-8 md:pl-6 leading-tight" ]
                [ div [ class "flex mb-4" ]
                    [ h1 [ class "flex-1 text-3xl text-blue" ] [ text document.title ]
                    , viewDocumentNavigation model
                    ]
                , div [] introHtml
                , div [] resultsHtml
                ]
            , div [ class "flex-1 min-w-full md:min-w-0" ]
                [ textarea [ value documentSource, onInput ChangeDocumentSource, class "flex-1 w-full h-full pt-4 pl-4 font-mono text-sm leading-normal text-indigo-darkest bg-indigo-lightest", rows 20 ] []
                ]
            ]


view : Model -> Html Message
view model =
    div [ class "flex flex-1" ]
        [ case model.nav of
            DocumentsList ->
                viewDocuments model
            
            Document index ->
                Array.get index model.documentSources
                    |> Maybe.map (viewDocumentSource model)
                    |> Maybe.withDefault (div [] [ text "No document" ])
        -- , div [ class "fixed pin-b pin-l flex pb-4 pl-4 md:pl-6" ]
        --     [ button [ class "px-2 py-1 text-purple-lightest bg-purple" ] [ text "Edit" ]
        --     , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Test" ]
        --     , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Export" ]
        --     ]
        ]


main : Program Never Model Message
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
