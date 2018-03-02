module Main exposing (main)

import Html exposing (..)
import Navigation exposing (Location)
import Html.Attributes exposing (class, id, rows, attribute, value, placeholder, disabled, href)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time)
import Date
import Dict exposing (Dict)
import Task
import Http
import Routes exposing (Route(..), CollectionSource(..), EditMode(..))
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
import Samples.UserProfile
import Services.GitHub


type alias Expressions =
    Result Error (List (List Token))


type alias Model =
    { documentSources : Dict String String
    , parsedDocuments : Dict String (Document Expressions)
    , processedDocuments : Dict String (Resolved Evaluate.Error Expressions)
    , route : Route
    , now : Time
    , sectionInputs : Dict String JsonValue
    , rpcResponses : Dict Datadown.Rpc.Id (Maybe Datadown.Rpc.Response)
    }


type Error
    = Parser Parser.Error
    | Evaluate Evaluate.Error
    | Process Process.Error


type alias DisplayOptions =
    { compact : Bool
    , hideNoContent : Bool
    , processDocument : Document Expressions -> Resolved Evaluate.Error Expressions
    , sectionInputs : Dict String JsonValue
    , getRpcResponse : Datadown.Rpc.Id -> Maybe (Maybe Datadown.Rpc.Response)
    , contentToJson : Content Expressions -> Result Evaluate.Error JsonValue
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
        "time:seconds" ->
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
                Err <| Evaluate.Parsing (toString error)

            Ok expressions ->
                evaluateTokenLines resolveWithModel expressions


valueForRpcID : Model -> String -> List String -> JsonValue -> Result Evaluate.Error JsonValue
valueForRpcID model id keyPath json =
    let
        maybeMaybeResponse =
            Dict.get id model.rpcResponses
    in
        case keyPath of
            "params" :: otherKeys ->
                json
                    |> JsonValue.getIn keyPath
                    >> Result.mapError Evaluate.NoValueForIdentifier

            "result" :: otherKeys ->
                case maybeMaybeResponse of
                    Just (Just response) ->
                        response.result
                            |> Result.mapError Evaluate.Rpc
                            |> Result.andThen (JsonValue.getIn otherKeys >> Result.mapError Evaluate.NoValueForIdentifier)

                    _ ->
                        Ok (JsonValue.NullValue)

            "error" :: otherKeys ->
                case maybeMaybeResponse of
                    Just (Just response) ->
                        case response.result of
                            Err error ->
                                error
                                    |> Datadown.Rpc.errorToJsonValue
                                    |> JsonValue.getIn otherKeys
                                    >> Result.mapError Evaluate.NoValueForIdentifier

                            _ ->
                                Ok (JsonValue.NullValue)

                    _ ->
                        Ok (JsonValue.NullValue)

            _ ->
                Err (Evaluate.NoValueForIdentifier (String.join "." keyPath))


contentToJson : Model -> Content Expressions -> Result Evaluate.Error JsonValue
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
                |> List.filterMap (Tuple.first >> (contentToJson model) >> Result.toMaybe)
                |> JsonValue.ArrayValue
                |> Ok

        Code maybeLanguage source ->
            String.trim source
                |> JsonValue.StringValue
                |> Ok

        Reference id keyPath json ->
            valueForRpcID model id keyPath json

        _ ->
            Err Evaluate.CannotConvertToJson


type alias Flags =
    {}


modelWithDocumentProcessed : String -> Model -> Model
modelWithDocumentProcessed key model =
    case Dict.get key model.documentSources of
        Just source ->
            let
                parsed =
                    parseDocument parseExpressions source

                processed =
                    processDocumentWithModel model parsed

                parsedDocuments =
                    Dict.insert key parsed model.parsedDocuments

                processedDocuments =
                    Dict.insert key processed model.processedDocuments
            in
                { model
                    | parsedDocuments = parsedDocuments
                    , processedDocuments = processedDocuments
                }

        Nothing ->
            model


modelWithCurrentDocumentProcessed : Model -> Model
modelWithCurrentDocumentProcessed model =
    case model.route of
        CollectionItem source key _ ->
            modelWithDocumentProcessed key model

        _ ->
            model


init : Flags -> Location -> ( Model, Cmd Message )
init flags location =
    let
        documentSources =
            [ ( "welcome", Samples.Welcome.source )
            , ( "clock", Samples.Clock.source )
            , ( "button", Samples.Button.source )
            , ( "images", Samples.Images.source )
            , ( "api", Samples.API.source )
            , ( "user-profile", Samples.UserProfile.source )
            , ( "try", "# Now your turn!" )
            ]
                |> Dict.fromList

        route =
            Routes.parseLocation location

        model =
            { documentSources = documentSources
            , parsedDocuments = Dict.empty
            , processedDocuments = Dict.empty
            , route = route
            , now = 0
            , sectionInputs = Dict.empty
            , rpcResponses = Dict.empty
            }

        maybeCollection =
            case route of
                Collection collection ->
                    Just collection
                
                CollectionItem collection _ _ ->
                    Just collection
                
                _ ->
                    Nothing
        
        commands =
            case maybeCollection of
                Just (GitHubRepo owner repo branch) ->
                    [ Services.GitHub.listDocuments owner repo
                        |> Task.attempt (LoadedGitHubComponents owner repo)
                    ]
                
                _ ->
                    []
    in
        case route of
            CollectionItem source key _ ->
                modelWithDocumentProcessed key model ! commands

            _ ->
                model ! commands


type Message
    = NavigateTo Location
    | ChangeDocumentSource String
    | GoToDocumentsList
    | GoToPreviousDocument
    | GoToNextDocument
    | GoToDocumentWithKey CollectionSource String
    | NewDocument
    | ChangeSectionInput String String
    | Time Time
    | BeginLoading
    | BeginRpcWithID String Bool
    | RpcResponded Datadown.Rpc.Response
    | LoadedGitHubComponents String String (Result Http.Error (List Services.GitHub.ContentInfo))


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NavigateTo location ->
            model ! []

        ChangeDocumentSource newInput ->
            let
                newModel =
                    case model.route of
                        CollectionItem source key _ ->
                            let
                                documentSources =
                                    model.documentSources
                                        |> Dict.insert key newInput
                            in
                                { model | documentSources = documentSources }
                                    |> modelWithDocumentProcessed key

                        _ ->
                            model
            in
                newModel ! []

        GoToDocumentsList ->
            let
                route =
                    case model.route of
                        Collection collection ->
                            Collection collection

                        CollectionItem collection _ _ ->
                            Collection collection

                        _ ->
                            Landing
            in
                { model | route = route } ! [ Navigation.modifyUrl <| Routes.toPath route ]

        GoToPreviousDocument ->
            case model.route of
                CollectionItem collection key editMode ->
                    case String.toInt key of
                        Ok index ->
                            let
                                newIndex =
                                    max 0 (index - 1)
                                
                                newKey =
                                    toString newIndex

                                newRoute =
                                    CollectionItem collection newKey editMode

                                newModel =
                                    { model | route = newRoute }
                                        |> modelWithDocumentProcessed newKey
                            in
                                newModel ! [ Navigation.modifyUrl <| Routes.toPath newRoute ]

                        _ ->
                            model ! []

                _ ->
                    model ! []

        GoToNextDocument ->
            case model.route of
                CollectionItem collection key editMode ->
                    case String.toInt key of
                        Ok index ->
                            let
                                maxIndex =
                                    Dict.size model.documentSources - 1

                                newIndex =
                                    min maxIndex (index + 1)
                                
                                newKey =
                                    toString newIndex

                                newRoute =
                                    CollectionItem collection newKey editMode

                                newModel =
                                    { model | route = newRoute }
                                        |> modelWithDocumentProcessed newKey
                            in
                                newModel ! [ Navigation.modifyUrl <| Routes.toPath newRoute ]

                        _ ->
                            model ! []

                _ ->
                    model ! []

        GoToDocumentWithKey collection key ->
            let
                newRoute =
                    CollectionItem collection key WithPreview

                newModel =
                    { model | route = newRoute }
                        |> modelWithDocumentProcessed key
            in
                newModel ! [ Navigation.modifyUrl (Routes.toPath newRoute) ]

        NewDocument ->
            let
                currentIndex =
                    case model.route of
                        CollectionItem collection key editMode ->
                            case String.toInt key of
                                Ok index ->
                                    index

                                _ ->
                                    0

                        _ ->
                            0

                newDocumentSource =
                    "# Untitled"

                documentSources =
                    model.documentSources
                        |> Dict.insert "new" newDocumentSource
            in
                { model
                    | documentSources = documentSources
                    , parsedDocuments = Dict.empty
                    , processedDocuments = Dict.empty
                }
                    ! []

        ChangeSectionInput sectionTitle newInput ->
            let
                newValue =
                    StringValue newInput

                newSectionInputs =
                    case String.split ":" sectionTitle of
                        head :: tail ->
                            model.sectionInputs
                                |> Dict.insert head newValue
                        
                        _ ->
                            model.sectionInputs

                newModel =
                    { model | sectionInputs = newSectionInputs }
            in
                modelWithCurrentDocumentProcessed newModel ! []

        Time time ->
            let
                newModel =
                    case model.route of
                        CollectionItem collection key _ ->
                            let
                                modelWithTime =
                                    { model | now = time }

                                maybeParsed =
                                    case Dict.get key model.parsedDocuments of
                                        Just parsed ->
                                            Just ( True, parsed )

                                        Nothing ->
                                            case Dict.get key model.documentSources of
                                                Just source ->
                                                    Just ( False, parseDocument parseExpressions source )

                                                Nothing ->
                                                    Nothing
                            in
                                case maybeParsed of
                                    Just ( cached, parsed ) ->
                                        let
                                            processed =
                                                processDocumentWithModel model parsed

                                            parsedDocuments =
                                                if cached then
                                                    model.parsedDocuments
                                                else
                                                    Dict.insert key parsed model.parsedDocuments

                                            processedDocuments =
                                                Dict.insert key processed model.processedDocuments
                                        in
                                            { model
                                                | parsedDocuments = parsedDocuments
                                                , processedDocuments = processedDocuments
                                                , now = time
                                            }

                                    Nothing ->
                                        model

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        BeginLoading ->
            let
                maybeResolvedDocument =
                    case model.route of
                        CollectionItem collection key _ ->
                            Dict.get key model.processedDocuments

                        _ ->
                            Nothing

                rpcsFromSection : ( String, ResolvedSection (Process.Error Evaluate.Error) Expressions ) -> List Rpc
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

                rpcToCommandAndId rpc =
                    case Datadown.Rpc.toCommand RpcResponded rpc of
                        Just command ->
                            Just ( rpc.id, command )

                        Nothing ->
                            Nothing

                ( ids, commands ) =
                    rpcs
                        |> List.filterMap rpcToCommandAndId
                        |> List.unzip

                emptyResponses =
                    List.repeat (List.length ids) Nothing
                        |> List.map2 (,) ids
                        |> Dict.fromList

                rpcResponses =
                    model.rpcResponses
                        |> Dict.union emptyResponses

                newModel =
                    modelWithCurrentDocumentProcessed { model | rpcResponses = rpcResponses }
            in
                newModel ! commands

        BeginRpcWithID id reload ->
            let
                maybeResolvedDocument =
                    case model.route of
                        CollectionItem collection key _ ->
                            Dict.get key model.processedDocuments

                        _ ->
                            Nothing

                rpcsFromSection : ( String, ResolvedSection (Process.Error Evaluate.Error) Expressions ) -> List Rpc
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

                maybeRpc =
                    rpcs
                        |> List.filter (\rpc -> rpc.id == id)
                        |> List.head
            in
                case maybeRpc of
                    Just rpc ->
                        case Datadown.Rpc.toCommand RpcResponded rpc of
                            Just command ->
                                let
                                    rpcResponses =
                                        Dict.insert id Nothing model.rpcResponses
                                in
                                    modelWithCurrentDocumentProcessed { model | rpcResponses = rpcResponses } ! [ command ]

                            Nothing ->
                                model ! []

                    Nothing ->
                        model ! []

        RpcResponded response ->
            let
                rpcResponses =
                    Dict.insert response.id (Just response) model.rpcResponses
            in
                modelWithCurrentDocumentProcessed { model | rpcResponses = rpcResponses } ! []

        LoadedGitHubComponents owner repo result ->
            case result of
                Ok contentInfos ->
                    let
                        documentSources =
                            contentInfos
                                |> List.filterMap (\r -> r.content |> Maybe.map (\content -> ( r.path, content )))
                                |> Dict.fromList
                    in
                        { model | documentSources = documentSources } ! []

                Err error ->
                    model ! []


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

        Url url ->
            div [] [ text (toString url) ]


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
        loadButton =
            button [ onClick <| BeginRpcWithID rpc.id True, class "w-full text-left" ] [ text "Load" ]

        ( loadingClasses, responseStatusHtml, maybeResponseHtml ) =
            case maybeResponse of
                Nothing ->
                    ( "bg-yellow-lighter", loadButton, Nothing )

                Just Nothing ->
                    ( "bg-orange-lighter", text "Loading…", Nothing )

                Just (Just response) ->
                    case response.result of
                        Ok json ->
                            ( "bg-green-lighter", span [ class "summary-indicator-inline" ] [ text "Success" ], Just <| Preview.Json.viewJson json )

                        Err error ->
                            let
                                statusText =
                                    [ "Error:"
                                    , toString error.code
                                    , error.message
                                    ]
                                        |> String.join " "
                            in
                                ( "bg-red-lighter", span [ class "summary-indicator-inline" ] [ text statusText ], Maybe.map Preview.Json.viewJson error.data )
    in
        div []
            [ details []
                [ summary [ class "flex justify-between px-2 py-1 font-mono text-xs italic text-white bg-grey-darker cursor-pointer" ]
                    [ span [ class "pr-2 summary-indicator-inline" ] [ text rpc.method ]
                    , span [] [ text rpc.id ]
                    ]
                , rpc.params
                    |> Maybe.map Preview.Json.viewJson
                    |> Maybe.withDefault (text "")
                ]
            , case maybeResponseHtml of
                Just responseHtml ->
                    details []
                        [ summary [ class "flex justify-between px-2 py-1 font-mono text-xs italic cursor-pointer", class loadingClasses ]
                            [ responseStatusHtml ]
                        , responseHtml
                        ]

                Nothing ->
                    div [ class "flex justify-between px-2 py-1 font-mono text-xs italic", class loadingClasses ]
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
            ul [] (List.map (\( item, qualifier ) -> li [] [ viewContent options item ]) items)

        Quote document ->
            let
                resolved =
                    options.processDocument document
            in
                resolved.sections
                    |> List.map makeSectionViewModel
                    |> List.map (viewSection [] options)
                    |> div [ class "pl-6 border-l border-teal" ]

        Reference id keyPath json ->
            div [] [ text "Reference: ", cite [] [ text <| toString id ] ]


viewContentResult : DisplayOptions -> Result (Process.Error Evaluate.Error) (Content (Result Error (List (List Token)))) -> Html Message
viewContentResult options contentResult =
    case contentResult of
        Err error ->
            case error of
                _ ->
                    div [ class "mb-3 px-2 py-1 text-white bg-red-dark" ] [ text (toString error) ]

        Ok content ->
            div [ class "mb-3" ] [ viewContent options content ]


viewContentResults : DisplayOptions -> List String -> String -> List (Result (Process.Error Evaluate.Error) (Content Expressions)) -> List b -> List (Html Message)
viewContentResults options parentPath sectionTitle contentResults subsections =
    let
        showNothing =
            List.isEmpty contentResults && options.hideNoContent

        hasSubsections =
            not <| List.isEmpty subsections
        
        showEditor =
            if String.contains ":" sectionTitle then
                True
            else if List.isEmpty contentResults then
                not hasSubsections
            else
                False
        
        -- FIXME: total hack
        isSingular =
            String.endsWith ": text" sectionTitle
    in
        if showNothing then
            []
        else if showEditor then
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
                
                jsonToString json =
                    case json of
                        StringValue s ->
                            s
                        
                        NumericValue n ->
                            toString n
                        
                        BoolValue b ->
                            if b then "✅" else "❎"
                        
                        ArrayValue items ->
                            if isSingular then
                                items
                                    |> List.head
                                    |> Maybe.map jsonToString
                                    |> Maybe.withDefault ""
                            else
                                items
                                    |> List.map jsonToString
                                    |> String.join "\n"
                        
                        _ ->
                            ""

                defaultValue =
                    contentResults
                        |> List.filterMap (Result.toMaybe)
                        |> List.filterMap (options.contentToJson >> Result.toMaybe)
                        |> List.map jsonToString
                        |> String.join "\n"

                stringValue =
                    case Dict.get key options.sectionInputs of
                        Nothing ->
                            ""

                        Just (StringValue s) ->
                            s

                        Just json ->
                            jsonToString json

                hasSubsections =
                    not <| List.isEmpty subsections
            in
                if hasSubsections then
                    []
                else
                    [ textarea [ value stringValue, placeholder defaultValue, onInput (ChangeSectionInput key), rows 3, class "w-full px-2 py-2 bg-blue-lightest border border-blue" ] []
                    ]
        else
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
    div [ class "fixed z-50 w-full h-8 bg-indigo-darkest", class "bg-red" ]
        [ case model.route of
            Collection collection ->
                div [ row, class "px-2 h-8 justify-between" ]
                    [ button [ onClick NewDocument, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "plus", text " New" ]
                    , button [ class "px-2 py-1 text-indigo-lightest rounded-sm" ] [ viewFontAwesomeIcon "share", text " Export" ]
                    ]

            CollectionItem collection key editMode ->
                div [ row, class "h-8 self-end flex-shrink items-center" ]
                    [ button [ onClick GoToDocumentsList, class "px-2 py-1 text-indigo-lightest" ] [ viewFontAwesomeIcon "list" ]
                    , div [ class "py-1 text-center font-bold text-indigo-lightest" ] [ text key ]
                    , button [ onClick BeginLoading, class "px-2 py-1 text-yellow-lighter" ] [ viewFontAwesomeIcon "arrow-circle-down" ]
                    ]

            _ ->
                div [] []
        ]


viewDocuments : CollectionSource -> Model -> Html Message
viewDocuments collection model =
    let
        viewDocument key documentSource =
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
            in
                h2 [ class "" ]
                    [ button [ class "w-full px-4 py-2 text-left text-2xl font-bold text-blue bg-white border-b border-blue-lighter", onClick (GoToDocumentWithKey collection key) ]
                        [ titleHtml ]
                    ]
    in
        div [ col, class "flex-1 justify-center" ]
            [ div [ class "mb-8" ] [ viewDocumentNavigation model ]
            , div [ class "flex-1 w-full max-w-lg mx-auto" ]
                [ div [ class "border-t border-blue-lighter" ]
                    (Dict.map viewDocument model.documentSources |> Dict.values)
                ]
            ]


processDocumentWithModel : Model -> Document (Result Error (List (List Token))) -> Resolved Evaluate.Error (Result Error (List (List Token)))
processDocumentWithModel model document =
    processDocument (evaluateExpressions model) (contentToJson model) document


viewDocumentPreview : Model -> Resolved Evaluate.Error Expressions -> Html Message
viewDocumentPreview model resolved =
    let
        displayOptions : DisplayOptions
        displayOptions =
            { compact = False
            , hideNoContent = False
            , processDocument = processDocumentWithModel model
            , sectionInputs = model.sectionInputs
            , getRpcResponse = (flip Dict.get) model.rpcResponses
            , contentToJson = contentToJson model
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
                [ h1 [ class "flex-1 pt-4 text-3xl text-blue" ] [ text resolved.title ]
                ]
            , div [ class "pr-4" ] introHtml
            , div [ class "pr-4" ] sectionsHtml
            ]


viewDocumentSource : Model -> String -> Resolved Evaluate.Error Expressions -> Html Message
viewDocumentSource model documentSource resolvedDocument =
    let
        editorHtml =
            case model.route of
                CollectionItem _ _ Off ->
                    text ""

                _ ->
                    div [ class "flex-1 min-w-full md:min-w-0" ]
                        [ textarea [ value documentSource, onInput ChangeDocumentSource, class "flex-1 w-full min-h-full overflow-auto pt-4 pl-4 font-mono text-sm leading-normal text-indigo-darkest bg-indigo-lightest", rows 20 ] []
                        ]

        previewHtml =
            case model.route of
                CollectionItem _ _ Only ->
                    text ""

                _ ->
                    viewDocumentPreview model resolvedDocument
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
    div [ class "flex justify-center flex-1" ]
        [ case model.route of
            Collection collection ->
                viewDocuments collection model

            CollectionItem _ key _ ->
                Maybe.map2
                    (viewDocumentSource model)
                    (Dict.get key model.documentSources)
                    (Dict.get key model.processedDocuments)
                    |> Maybe.withDefault (div [] [ text <| "No document #" ++ key ])

            _ ->
                div [ col, class "flex flex-row flex-1 max-w-lg p-4 text-center text-grey-darkest" ]
                    [ h1 [ class "mb-2 text-center" ]
                        [ text "Datadown" ]
                    , h2 [ class "mb-4 text-center" ]
                        [ text "Make components & prototypes with Markdown." ]
                    , h3 [] [ a [ href "/example" ] [ text "See examples" ] ]
                    ]

        -- , div [ class "fixed pin-b pin-l flex pb-4 pl-4 md:pl-6" ]
        --     [ button [ class "px-2 py-1 text-purple-lightest bg-purple" ] [ text "Edit" ]
        --     , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Test" ]
        --     , button [ class "px-2 py-1 text-purple-dark bg-purple-lightest" ] [ text "Export" ]
        --     ]
        ]


main : Program Flags Model Message
main =
    Navigation.programWithFlags
        NavigateTo
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
