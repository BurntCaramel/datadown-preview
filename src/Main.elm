module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, rows, attribute, value)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time)
import Date
import Array exposing (Array)
import Datadown exposing (Document, Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process as Process exposing (processDocument, listVariablesInDocument, Error)
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
    , currentDocumentIndex : Int
    , nav : Nav
    , now : Time
    }


type Nav
    = DocumentsList
    | Document Int


type Error
    = Parser Parser.Error
    | Evaluate Evaluate.Error
    | Process Process.Error


parseExpressions : String -> Result Error (List (List Token))
parseExpressions input =
    case tokenize input of
        Err parserError ->
            Err (Parser parserError)

        Ok tokens ->
            Ok tokens


valueFromModel : Model -> String -> Maybe JsonValue
valueFromModel model key =
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


evaluateExpressions : Model -> (String -> Result (Process.Error Evaluate.Error) JsonValue) -> Result Error (List (List Token)) -> Result Evaluate.Error JsonValue
evaluateExpressions model resolveFromDocument parsedExpressions =
    let
        resolveWithModel key =
            case valueFromModel model key of
                Just value ->
                    Ok value
                
                Nothing ->
                    resolveFromDocument key
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
    , currentDocumentIndex = 0
    , nav = Document 0
    , now = 0
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
    | Time Time


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ChangeDocumentSource newInput ->
            let
                documentSources =
                    model.documentSources
                        |> Array.set model.currentDocumentIndex newInput
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


viewCode : Bool -> Maybe String -> String -> Html Message
viewCode compact language source =
    let
        codeHtmlList =
            if not compact && showCodeForLanguage language then
                [ pre [ class "overflow-auto px-2 py-2 text-purple-darker bg-purple-lightest" ]
                    [ code [ class "font-mono text-sm" ] [ text source ] ]
                ]
            else
                []
    in
        div []
            (codeHtmlList ++ viewCodePreview language source)


viewContent : Bool -> Content (Result Error (List (List Token))) -> Html Message
viewContent compact content =
    case content of
        Text s ->
            div [ class "font-sans w-full" ] [ text s ]

        Code language source ->
            viewCode compact language source

        Json json ->
            Preview.Json.viewJson json

        Expressions expressionsResult ->
            case expressionsResult of
                Err expressionsError ->
                    h3 [] [ text <| toString expressionsError ]

                Ok expressions ->
                    pre [ class "px-2 py-2 text-teal-darker bg-teal-lightest" ]
                        [ code [ class "font-mono text-sm" ] (List.map viewExpression expressions) ]

        List items ->
            ul [] (List.map (\item -> li [] [ viewContent compact item ]) items)

        Quote document ->
            pre [] [ code [] [ text "quoted document" ] ]


viewContentResult : Bool -> Result e (Content (Result Error (List (List Token)))) -> Html Message
viewContentResult compact contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [] [ text (toString error) ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent compact content ]


type alias SectionViewModel e =
    { title : String
    , resolvedContent : Result e (Content (Result Error (List (List Token))))
    , variables : List String
    }


makeSectionViewModel : ( String, Result e (Content (Result Error (List (List Token)))) ) -> ( String, List String ) -> SectionViewModel e
makeSectionViewModel ( title, resolvedContent ) ( _, variables ) =
    SectionViewModel title resolvedContent variables


viewSection : SectionViewModel e -> Html Message
viewSection { title, resolvedContent, variables } =
    details [ attribute "open" "" ]
        [ summary []
            [ h2 [ class "mb-2 text-xl text-blue-dark" ] [ text title ]
            ]
        , resolvedContent
            |> viewContentResult False

        -- , div [] [ text (variables |> toString) ]
        ]


viewFontAwesomeIcon : String -> Html Message
viewFontAwesomeIcon id =
    i [ class ("fas fa-" ++ id) ] []


viewDocumentNavigation : Model -> Html Message
viewDocumentNavigation model =
    div []
        ([ case model.nav of
            DocumentsList ->
                [ button [ onClick NewDocument, class "px-2 py-1 text-teal-dark bg-teal-lightest" ] [ viewFontAwesomeIcon "plus" ]
                -- button [ onClick GoToDocumentsList, class "px-2 py-1 text-purple-lightest bg-purple-dark" ] [ viewFontAwesomeIcon "bars" ]
                ]
            
            Document index ->
                [ button [ onClick GoToPreviousDocument, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "arrow-left" ]
                , div [ class "inline-block w-3 py-1 text-center font-bold text-green-dark bg-green-lightest" ] [ text (index + 1 |> toString) ]
                , button [ onClick GoToNextDocument, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "arrow-right" ]
                , button [ onClick GoToDocumentsList, class "px-2 py-1 text-green-dark bg-green-lightest" ] [ viewFontAwesomeIcon "bars" ]
                ]
        ] |> List.concat)


viewDocuments : Model -> Html Message
viewDocuments model =
    let
        viewDocument index documentSource =
            let
                document =
                    parseDocument parseExpressions documentSource
            in
                h2 [ class "mb-4" ]
                    [ button [ class "text-3xl font-bold text-blue", onClick (GoToDocumentAtIndex index) ] [ text document.title ] ]
    in
        div [ class "p-4" ]
            [ viewDocumentNavigation model
            , div [ class "mt-4" ]
                (Array.indexedMap viewDocument model.documentSources |> Array.toList)
            ]


viewDocumentSource : Model -> String -> Html Message
viewDocumentSource model documentSource =
    let
        document : Document (Result Error (List (List Token)))
        document =
            parseDocument parseExpressions documentSource

        resolved =
            processDocument (evaluateExpressions model) (contentToJson model) document

        sectionVariables =
            listVariablesInDocument document

        results =
            List.map2 makeSectionViewModel resolved.sections sectionVariables

        resultsEl =
            results
                |> List.map viewSection
        
        introEl =
            case resolved.intro of
                Ok content ->
                   viewContentResult True (Ok content)
                
                Err (Process.NoContentForSection _) ->
                    text ""

                Err error ->
                    div [] [ text <| toString error ]
    in
        div [ class "flex flex-wrap h-screen" ]
            [ div [ class "flex-1 overflow-auto mb-8 p-4 pb-8 md:pl-6" ]
                [ div [ class "flex" ]
                    [ h1 [ class "flex-1 mb-4 text-3xl text-blue" ] [ text document.title ]
                    , viewDocumentNavigation model
                    ]
                , introEl
                , div [] resultsEl
                ]
            , div [ class "flex-1 min-w-full md:min-w-0" ]
                [ textarea [ value documentSource, onInput ChangeDocumentSource, class "flex-1 w-full h-full pt-4 pl-4 font-mono text-sm text-blue-darkest bg-blue-lightest", rows 20 ] []
                ]
            ]


view : Model -> Html Message
view model =
    let
        documentSourceMaybe =
            Array.get model.currentDocumentIndex model.documentSources
    in
        div []
            [ case model.nav of
                DocumentsList ->
                    viewDocuments model
                
                Document index ->
                    Array.get index model.documentSources
                        |> Maybe.map (viewDocumentSource model)
                        |> Maybe.withDefault (div [] [ text "No document" ])
            , div [ class "fixed pin-b pin-l flex pb-4 pl-4 md:pl-6" ]
                [ button [ class "px-2 py-1 text-purple-lightest bg-purple" ] [ text "Edit" ]
                , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Test" ]
                , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Export" ]
                ]
            ]


main : Program Never Model Message
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
