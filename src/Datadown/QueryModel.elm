module Datadown.QueryModel
    exposing
        ( QueryModel
        , StringFieldError(..)
        , FieldKind(..)
        , FieldValue(..)
        , FieldDefinition
        , ArgsDefinition(..)
        , queryFieldDefinition
        , parseFieldDefinitionFromTitleAndSection
        , parseQueryModel
        , applyValuesToModel
        )

{-| QueryModel

@docs parseQueryModel, applyValuesToModel


## Types

@docs QueryModel, FieldKind(..), FieldValue(..), FieldDefinition, queryFieldDefinition

-}

import Regex
import JsonValue exposing (JsonValue)
import Datadown exposing (Content(..))
import Datadown.Process exposing (ResolvedSection(..))
import Datadown.Expressions exposing (parseExpression, evaluateAsInt)


type alias StringFieldConstraints =
    { choices : Maybe (List String)
    }


type StringFieldError
    = NotInChoices String (List String)


type FieldKind
    = String StringFieldConstraints
    | Bool
    | Int
    | StringsArray
    | Query


type FieldValue
    = StringValue (Result StringFieldError (Maybe String)) StringFieldConstraints
    | BoolValue Bool
    | IntValue Int
    | StringsArrayValue (List String)
    | QueryValue


type alias FieldDefinition =
    { name : String
    , kind : FieldKind
    , value : FieldValue
    , argDefinitions : ArgsDefinition
    }


type ArgsDefinition =
    ArgsDefinition (List FieldDefinition)


type alias QueryModel =
    { fields : List FieldDefinition
    }


defaultValueForKind : FieldKind -> FieldValue
defaultValueForKind kind =
    case kind of
        String constraints ->
            let
                current =
                    Maybe.andThen List.head constraints.choices
            in
                StringValue (Ok current) constraints

        Bool ->
            BoolValue False

        Int ->
            IntValue 0
        
        StringsArray ->
            StringsArrayValue []

        Query ->
            QueryValue


queryFieldDefinition : String -> List FieldDefinition -> FieldDefinition
queryFieldDefinition name args =
    { name = name
    , kind = Query
    , value = QueryValue
    , argDefinitions = ArgsDefinition args
    }


contentListToJsonList : (Content a -> Result e JsonValue) -> List (Result e (Content a)) -> List JsonValue
contentListToJsonList contentToJson contentList =
    contentList
        |> List.filterMap (Result.toMaybe >> Maybe.andThen (contentToJson >> Result.toMaybe))


jsonListToStringListHelper : JsonValue -> Maybe (List String) -> Maybe (List String)
jsonListToStringListHelper jsonList maybeStringList =
    case ( jsonList, maybeStringList ) of
        ( JsonValue.StringValue s, Just stringList ) ->
            Just (s :: stringList)

        _ ->
            Nothing


jsonListToStringList : List JsonValue -> Maybe (List String)
jsonListToStringList jsonList =
    List.foldr jsonListToStringListHelper (Just []) jsonList


parseNameAndKindFrom : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> String -> ResolvedSection e a -> Maybe ( String, FieldKind )
parseNameAndKindFrom contentToJson sectionDefiningType rawTitle (ResolvedSection section) =
    case String.split ":" rawTitle of
        "" :: _ ->
            Nothing
        
        name :: rawKind :: _ ->
            let
                kindString =
                    rawKind
                        |> Regex.replace Regex.All (Regex.regex "`") (\_ -> "")
                        |> String.trim

                maybeKind : Maybe FieldKind
                maybeKind =
                    case kindString of
                        "Bool" ->
                            Just Bool

                        "String" ->
                            let
                                jsonList =
                                    contentListToJsonList contentToJson section.mainContent

                                maybeChoices =
                                    case jsonList of
                                        (JsonValue.ArrayValue items) :: [] ->
                                            jsonListToStringList items

                                        _ ->
                                            Nothing

                                constraints =
                                    maybeChoices
                                        |> StringFieldConstraints
                            in
                                Just (String constraints)

                        "Int" ->
                            Just Int
                        
                        "[String]" ->
                            Just StringsArray

                        customType ->
                            let
                                maybeSection =
                                    sectionDefiningType customType

                                maybeFieldDefinition =
                                    maybeSection
                                        |> Maybe.andThen (parseFieldDefinitionFromTitleAndSection contentToJson sectionDefiningType)
                                        |> Maybe.map .kind
                            in
                                maybeFieldDefinition
            in
                case maybeKind of
                    Nothing ->
                        Nothing
                    
                    Just kind ->
                        Just ( name, kind )
        _ ->
            Nothing


parseFieldDefinitionFromTitleAndSection : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ( String, ResolvedSection e a ) -> Maybe FieldDefinition
parseFieldDefinitionFromTitleAndSection contentToJson sectionDefiningType ( rawTitle, ResolvedSection section ) =
    case parseNameAndKindFrom contentToJson sectionDefiningType rawTitle (ResolvedSection section) of
        Just (name, kind) ->
            let 
                args =
                    section.subsections
                        |> List.filterMap (parseFieldDefinitionFromTitleAndSection contentToJson sectionDefiningType)
            in
                Just
                    { name = name
                    , kind = kind
                    , value = defaultValueForKind kind
                    , argDefinitions = ArgsDefinition args
                    }

        _ ->
            Nothing


parseQueryModel : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ResolvedSection e a -> QueryModel
parseQueryModel contentToJson sectionDefiningType (ResolvedSection section) =
    let
        fields =
            section.subsections
                |> List.filterMap (parseFieldDefinitionFromTitleAndSection contentToJson sectionDefiningType)
    in
        QueryModel fields


resolveExpression : (String -> Maybe FieldDefinition) -> FieldKind -> String -> Maybe FieldValue
resolveExpression sourceFieldWithName kind expressionSource =
    let
        resolveIdentifier identifier =
            case sourceFieldWithName identifier of
                Just field ->
                    case field.value of
                        IntValue i ->
                            Just i

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        exp =
            expressionSource
                |> parseExpression

        maybeInt =
            exp
                |> Result.toMaybe
                |> Maybe.andThen (evaluateAsInt resolveIdentifier >> Result.toMaybe)
    in
        case kind of
            Int ->
                Maybe.map IntValue maybeInt

            _ ->
                Nothing


applyValuesToModel : (Content a -> Result e JsonValue) -> ResolvedSection e a -> QueryModel -> QueryModel
applyValuesToModel contentToJson sectionWrapper model =
    let
        contentInSection : ResolvedSection e a -> List (Result e (Content a))
        contentInSection sectionWrapper =
            case sectionWrapper of
                ResolvedSection section ->
                    section.mainContent

        section =
            case sectionWrapper of
                ResolvedSection section ->
                    section

        contentListForField : FieldDefinition -> Maybe (List (Result e (Content a)))
        contentListForField field =
            section.subsections
                |> List.filter (\( title, subsection ) -> title == field.name)
                |> List.head
                |> Maybe.map (Tuple.second >> contentInSection)

        sourceFieldWithName name =
            model.fields
                |> List.filter (\field -> field.name == name)
                |> List.head

        jsonListToValue : FieldKind -> List JsonValue -> Maybe FieldValue
        jsonListToValue kind jsonList =
            case ( kind, jsonList ) of
                ( String constraints, (JsonValue.StringValue ".empty") :: [] ) ->
                    Just <| StringValue (Ok <| Just "") constraints

                ( String constraints, (JsonValue.StringValue s) :: [] ) ->
                    case constraints.choices of
                        Just choices ->
                            if List.member s choices then
                                Just <| StringValue (Ok <| Just s) constraints
                            else
                                Just <| StringValue (Err <| NotInChoices s choices) constraints

                        Nothing ->
                            Just <| StringValue (Ok <| Just s) constraints
                
                ( StringsArray, (JsonValue.StringValue s) :: jsonList ) ->
                    let
                        strings =
                            List.foldr (\v l ->
                                case v of
                                    JsonValue.StringValue s ->
                                        s :: l
                                    
                                    _ ->
                                        l
                            ) [s] jsonList
                    in
                        Just <| StringsArrayValue strings

                ( Bool, (JsonValue.BoolValue s) :: [] ) ->
                    Just <| BoolValue s

                ( Bool, (JsonValue.StringValue ".false") :: [] ) ->
                    Just <| BoolValue False

                ( Bool, (JsonValue.StringValue ".true") :: [] ) ->
                    Just <| BoolValue True

                ( kind, (JsonValue.StringValue s) :: [] ) ->
                    resolveExpression sourceFieldWithName kind s

                _ ->
                    Nothing

        updateField field =
            let
                maybeNewValue =
                    field
                        |> contentListForField
                        |> Maybe.map (contentListToJsonList contentToJson)
                        |> Maybe.andThen (jsonListToValue field.kind)
            in
                case maybeNewValue of
                    Just value ->
                        { field
                            | value = value
                        }

                    Nothing ->
                        field

        fields =
            List.map updateField model.fields
    in
        QueryModel fields
