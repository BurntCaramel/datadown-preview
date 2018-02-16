module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, id, rows, attribute, value)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time)
import Date
import Array exposing (Array)
import Dict exposing (Dict)
import Http
import Datadown exposing (Document, Section(..), Content(..))
import Datadown.Parse exposing (parseDocument)
import Datadown.Process as Process exposing (processDocument, Error, Resolved, ResolvedSection(..))
import Datadown.Rpc exposing (Rpc)
import JsonValue exposing (JsonValue(..))
import Parser exposing (Error)
import Preview
import Preview.Json
import Preview.Decorate
import Expressions.Tokenize as Tokenize exposing (tokenize, Token(..))
import Expressions.Evaluate as Evaluate exposing (evaluateTokenLines)
import Samples.Welcome
import Samples.Clock
import Samples.Button
import Samples.Images
import Samples.API


type EditMode
    = Off
    | WithPreview
    | Only


editModeFromInt : Int -> Maybe EditMode
editModeFromInt int =
    case int of
        0 ->
            Just Off

        1 ->
            Just WithPreview

        2 ->
            Just Only

        _ ->
            Nothing


type alias Model =
    { documentSources : Array String
    , nav : Nav
    , editMode : EditMode
    , now : Time
    , sectionInputs : Dict String JsonValue
    , rpcResponses : Dict Datadown.Rpc.Id (Maybe Datadown.Rpc.Response)
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
    , getRpcResponse : Datadown.Rpc.Id -> Maybe (Maybe Datadown.Rpc.Response)
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
                |> JsonValue.NumericValue
                >> Just

        "now.date.s" ->
            model.now
                |> Date.fromTime
                |> Date.second
                |> toFloat
                |> JsonValue.NumericValue
                >> Just

        "now.date.m" ->
            model.now
                |> Date.fromTime
                |> Date.minute
                |> toFloat
                |> JsonValue.NumericValue
                >> Just

        "now.date.h" ->
            model.now
                |> Date.fromTime
                |> Date.hour
                |> toFloat
                |> JsonValue.NumericValue
                >> Just

        _ ->
            Nothing


valueFromModel : Model -> String -> Maybe JsonValue
valueFromModel model key =
    let
        keyPath =
            key
                |> String.split "."
    in
        case Dict.get key model.sectionInputs of
            Just value ->
                Just value

            Nothing ->
                builtInValueFromModel model key


evaluateExpressions : Model -> (String -> Result (Process.Error Evaluate.Error) JsonValue) -> Result Error (List (List Token)) -> Result Evaluate.Error JsonValue
evaluateExpressions model resolveFromDocument parsedExpressions =
    let
        resolveWithModel : String -> Result (Process.Error Evaluate.Error) JsonValue
        resolveWithModel key =
            case resolveFromDocument key of
                Ok value ->
                    Ok value

                Err error ->
                    case valueFromModel model key of
                        Just value ->
                            Ok value

                        Nothing ->
                            Err error
    in
        case parsedExpressions of
            Err error ->
                Err Evaluate.Parsing

            Ok expressions ->
                evaluateTokenLines resolveWithModel expressions


valueForRpcID : Model -> String -> List String -> Result Evaluate.Error JsonValue
valueForRpcID model id keyPath =
    let
        maybeMaybeResponse =
            Dict.get id model.rpcResponses
    in
        case maybeMaybeResponse of
            Nothing ->
                Ok (JsonValue.NullValue)

            Just Nothing ->
                Ok (JsonValue.StringValue "Loading…")

            Just (Just response) ->
                case keyPath of
                    "result" :: otherKeys ->
                        response.result
                            |> Result.mapError Evaluate.Rpc
                            |> Result.andThen (JsonValue.getIn otherKeys >> Result.mapError Evaluate.NoValueForIdentifier)
                    
                    "error" :: otherKeys ->
                        case response.result of
                            Err error ->
                                error
                                    |> Datadown.Rpc.errorToJsonValue
                                    |> JsonValue.getIn otherKeys >> Result.mapError Evaluate.NoValueForIdentifier
                            
                            _ ->
                                Ok (JsonValue.NullValue)
                    
                    _ ->
                        Err (Evaluate.NoValueForIdentifier (String.join "." keyPath))


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
        
        Reference id keyPath ->
            valueForRpcID model id keyPath

        _ ->
            Err Evaluate.CannotConvertToJson


type alias Flags =
    { editModeInt : Maybe Int
    }


init : Flags -> ( Model, Cmd Message )
init flags =
    { documentSources =
        [ Samples.Welcome.source
        , Samples.Clock.source
        , Samples.Button.source
        , Samples.Images.source
        , Samples.API.source
        , "# Now your turn!"
        ]
            |> Array.fromList
    , nav = Document 0
    , editMode =
        flags.editModeInt
            |> Maybe.andThen editModeFromInt
            |> Maybe.withDefault WithPreview
    , now = 0
    , sectionInputs = Dict.empty
    , rpcResponses = Dict.empty
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
    | BeginLoading
    | RpcResponded Datadown.Rpc.Response


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
                    model.sectionInputs
                        |> Dict.insert sectionTitle newValue
            in
                ( { model | sectionInputs = newSectionInputs }, Cmd.none )

        Time time ->
            ( { model | now = time }, Cmd.none )

        BeginLoading ->
            let
                maybeDocument =
                    case model.nav of
                        Document index ->
                            Array.get index model.documentSources
                                |> Maybe.map (parseDocument parseExpressions)

                        _ ->
                            Nothing

                maybeResolvedDocument =
                    maybeDocument
                        |> Maybe.map (processDocumentWithModel model)

                rpcsFromSection : ( String, ResolvedSection (Process.Error Evaluate.Error) (Result Error (List (List Token))) ) -> List Rpc
                rpcsFromSection ( title, section ) =
                    case section of
                        ResolvedSection { rpcs } ->
                            rpcs

                rpcs =
                    case maybeResolvedDocument of
                        Just resolved ->
                            resolved.sections
                                |> List.concatMap rpcsFromSection

                        Nothing ->
                            []

                commands =
                    rpcs
                        |> List.filterMap (Datadown.Rpc.toCommand RpcResponded)
            in
                ( model, Cmd.batch commands )

        RpcResponded response ->
            let
                rpcResponses =
                    Dict.insert response.id (Just response) model.rpcResponses
            in
                ( { model | rpcResponses = rpcResponses }, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Time.every Time.second Time
        ]


row : Html.Attribute msg
row =
    class "flex flex-row"


col : Html.Attribute msg
col =
    class "flex flex-col"


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


viewCode : DisplayOptions -> Maybe String -> String -> Html Message
viewCode options maybeLanguage source =
    let
        previewHtml =
            Preview.view maybeLanguage source
    in
        if not options.compact && showCodeForLanguage maybeLanguage then
            div []
                [ div [] [ previewHtml ]
                , details [ class "mt-2" ]
                    [ summary [ class "px-2 py-1 font-mono text-xs italic text-purple-darker bg-purple-lightest cursor-default" ]
                        [ text ("Source" ++ (Maybe.map ((++) " ") maybeLanguage |> Maybe.withDefault "")) ]
                    , pre [ class "overflow-auto px-2 py-2 text-purple-darker bg-purple-lightest" ]
                        [ code [ class "font-mono text-xs" ] [ text source ] ]
                    ]
                ]
        else
            div [] [ previewHtml ]


viewRpc : Rpc -> Maybe (Maybe Datadown.Rpc.Response) -> Html Message
viewRpc rpc maybeResponse =
    let
        (loadingClasses, responseStatusHtml, maybeResponseHtml) =
            case maybeResponse of
                Nothing ->
                    ("bg-grey-lighter", text "Not loaded", Nothing)
                
                Just Nothing ->
                    ("bg-orange-lighter", text "Loading…", Nothing)
                
                Just (Just response) ->
                    case response.result of
                        Ok json ->
                            ("bg-green-lighter", span [ class "summary-indicator-inline" ] [ text "Success" ], Just <| Preview.Json.viewJson json)
                        
                        Err error ->
                            let
                                statusText =
                                    [ "Error:"
                                    , toString error.code
                                    , error.message
                                    ]
                                        |> String.join " "
                            in
                                ("bg-red-lighter", span [ class "summary-indicator-inline" ] [ text statusText ], Maybe.map Preview.Json.viewJson error.data)
    in
        div []
            [ details []
                [ summary [ class "flex justify-between px-2 py-1 font-mono text-xs italic text-white bg-grey-darker cursor-pointer" ]
                    [ span [ class "pr-2 summary-indicator-inline" ] [ text rpc.method]
                    , span [] [ text rpc.id ]
                    ]
                , rpc.params
                    |> Maybe.map Preview.Json.viewJson
                    |> Maybe.withDefault (text "")
                ]
            , case maybeResponseHtml of
                    Just responseHtml ->
                        details []
                            [ summary [ class "px-2 py-1 font-mono text-xs italic cursor-pointer", class loadingClasses ]
                                [ responseStatusHtml ]
                            , responseHtml
                            ]

                    Nothing ->
                        div [ class "px-2 py-1 font-mono text-xs italic", class loadingClasses ]
                            [ responseStatusHtml ]
            ]


viewContent : DisplayOptions -> Content (Result Error (List (List Token))) -> Html Message
viewContent options content =
    case content of
        Text s ->
            div [ class "font-sans w-full" ] (Preview.Decorate.view s)

        Code language source ->
            viewCode options language source

        Json json ->
            let
                maybeRpc =
                    Datadown.Rpc.fromJsonValue json
            in
                case maybeRpc of
                    Just rpc ->
                        viewRpc rpc (options.getRpcResponse rpc.id)

                    Nothing ->
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
        
        Reference id keyPath ->
            div [] [ text "Reference: ", text <| toString id]


viewContentResult : DisplayOptions -> Result (Process.Error Evaluate.Error) (Content (Result Error (List (List Token)))) -> Html Message
viewContentResult options contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [ class "mb-3 px-2 py-1 text-white bg-red-dark" ] [ text (toString error) ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent options content ]


viewContentResults : DisplayOptions -> List String -> String -> List (Result (Process.Error Evaluate.Error) (Content (Result Error (List (List Token))))) -> List b -> List (Html Message)
viewContentResults options parentPath sectionTitle contentResults subsections =
    case contentResults of
        [] ->
            if options.hideNoContent then
                []
            else
                let
                    baseTitle =
                        sectionTitle
                            |> String.split ":"
                            |> List.head
                            |> Maybe.withDefault ""

                    key =
                        baseTitle
                            :: parentPath
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

                    hasSubsections =
                        not <| List.isEmpty subsections
                in
                    if hasSubsections then
                        []
                    else
                        [ textarea [ value stringValue, onInput (ChangeSectionInput key), rows 3, class "w-full px-2 py-2 bg-blue-lightest border border-blue" ] []
                        ]

        _ ->
            contentResults
                |> List.map (viewContentResult options)


type alias SectionViewModel e =
    { title : String
    , mainContent : List (Result e (Content (Result Error (List (List Token)))))
    , subsections : List ( String, ResolvedSection e (Result Error (List (List Token))) )
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
            h2 [ class "mb-2 text-xl text-blue-dark cursor-pointer summary-indicator-absolute" ]

        1 ->
            h3 [ class "mb-2 text-lg text-blue-dark cursor-pointer summary-indicator-absolute" ]

        2 ->
            h4 [ class "mb-2 text-base text-blue-dark cursor-pointer summary-indicator-absolute" ]

        _ ->
            h4 [ class "mb-2 text-sm text-blue-dark cursor-pointer summary-indicator-absolute" ]


viewSection : List String -> DisplayOptions -> SectionViewModel (Process.Error Evaluate.Error) -> Html Message
viewSection sectionPath options { title, mainContent, subsections } =
    details [ attribute "open" "" ]
        [ summary []
            [ viewSectionTitle (List.length sectionPath) [ text title ]
            ]
        , viewContentResults options sectionPath title mainContent subsections
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
    div [ class "fixed w-full h-8 bg-indigo-darkest", class "bg-red" ]
        [ case model.nav of
            DocumentsList ->
                div [ row, class "h-8 justify-between" ]
                    [ button [ onClick NewDocument, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "plus", text " New" ]
                    , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest border border-purple-lighter rounded-sm" ] [ viewFontAwesomeIcon "share", text " Export" ]
                    ]

            Document index ->
                div [ row, class "h-8 self-end flex-shrink items-center" ]
                    [ button [ onClick GoToDocumentsList, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "list" ]
                    , button [ onClick GoToPreviousDocument, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "caret-left" ]
                    , div [ class "py-1 text-center font-bold text-indigo-lightest" ] [ text (index + 1 |> toString) ]
                    , button [ onClick GoToNextDocument, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "caret-right" ]
                    , button [ onClick BeginLoading, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "arrow-circle-down" ]
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
                        |> Maybe.andThen
                            (\line ->
                                if String.isEmpty line then
                                    Nothing
                                else
                                    Just line
                            )

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
        div [ col, class "flex-1 justify-center" ]
            [ div [ class "mb-8" ] [ viewDocumentNavigation model ]
            , div [ class "flex-1 w-full max-w-lg mx-auto" ]
                [ div [ class "border-t border-blue-lighter" ]
                    (Array.indexedMap viewDocument model.documentSources |> Array.toList)
                ]
            ]


processDocumentWithModel : Model -> Document (Result Error (List (List Token))) -> Resolved Evaluate.Error (Result Error (List (List Token)))
processDocumentWithModel model document =
    processDocument (evaluateExpressions model) (contentToJson model) document


viewDocumentPreview : Model -> String -> Html Message
viewDocumentPreview model documentSource =
    let
        document : Document (Result Error (List (List Token)))
        document =
            parseDocument parseExpressions documentSource

        resolved : Resolved Evaluate.Error (Result Error (List (List Token)))
        resolved =
            processDocumentWithModel model document

        displayOptions : DisplayOptions
        displayOptions =
            { compact = False
            , hideNoContent = False
            , processDocument = processDocumentWithModel model
            , sectionInputs = model.sectionInputs
            , getRpcResponse = (flip Dict.get) model.rpcResponses
            }

        sectionsHtml : List (Html Message)
        sectionsHtml =
            resolved.sections
                |> List.map makeSectionViewModel
                |> List.map (viewSection [] displayOptions)

        introHtml : List (Html Message)
        introHtml =
            viewContentResults
                { displayOptions
                    | compact = True
                    , hideNoContent = True
                }
                []
                "intro"
                resolved.intro
                []
    in
        div [ class "flex-1 overflow-auto mb-8 pl-4 pb-8 md:pl-6 leading-tight" ]
            [ div [ row, class "mb-4" ]
                [ h1 [ class "flex-1 pt-4 text-3xl text-blue" ] [ text document.title ]
                ]
            , div [ class "pr-4" ] introHtml
            , div [ class "pr-4" ] sectionsHtml
            ]


viewDocumentSource : Model -> String -> Html Message
viewDocumentSource model documentSource =
    let
        editorHtml =
            case model.editMode of
                Off ->
                    text ""

                _ ->
                    div [ class "flex-1 min-w-full md:min-w-0" ]
                        [ textarea [ value documentSource, onInput ChangeDocumentSource, class "flex-1 w-full min-h-full overflow-auto pt-4 pl-4 font-mono text-sm leading-normal text-indigo-darkest bg-indigo-lightest", rows 20 ] []
                        ]

        previewHtml =
            case model.editMode of
                Only ->
                    text ""

                _ ->
                    viewDocumentPreview model documentSource
    in
        div [ col, class "flex-1 h-screen" ]
            [ div [ row, class "mb-8 bg-indigo-darkest" ]
                [ viewDocumentNavigation model
                ]
            , div [ row, class "flex-1 flex-wrap h-screen" ]
                [ editorHtml
                , previewHtml
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


main : Program Flags Model Message
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
