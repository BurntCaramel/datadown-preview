module Datadown.QueryModel
    exposing
        ( QueryModel
        , StringFieldError(..)
        , FieldKind(..)
        , FieldValue(..)
        , FieldDefinition
        , queryFieldDefinition
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
    | Query


type FieldValue
    = StringValue (Result StringFieldError (Maybe String)) StringFieldConstraints
    | BoolValue Bool
    | IntValue Int
    | QueryValue


type alias FieldDefinition =
    { name : String
    , kind : FieldKind
    , value : FieldValue
    }


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

        Query ->
            QueryValue


queryFieldDefinition : String -> FieldDefinition
queryFieldDefinition name =
    { name = name
    , kind = Query
    , value = QueryValue
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


parseFieldDefinition : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ( String, ResolvedSection e a ) -> Maybe FieldDefinition
parseFieldDefinition contentToJson sectionDefiningType ( rawTitle, sectionWrapper ) =
    case String.split ":" rawTitle of
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
                                section =
                                    case sectionWrapper of
                                        ResolvedSection section ->
                                            section

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

                        customType ->
                            let
                                maybeSection =
                                    sectionDefiningType customType

                                maybeFieldDefinition =
                                    maybeSection
                                        |> Maybe.andThen (parseFieldDefinition contentToJson sectionDefiningType)
                                        |> Maybe.map .kind
                            in
                                maybeFieldDefinition
            in
                case ( name, maybeKind ) of
                    ( "", _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing

                    ( name, Just kind ) ->
                        Just
                            { name = name
                            , kind = kind
                            , value = defaultValueForKind kind
                            }

        _ ->
            Nothing


parseQueryModel : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ResolvedSection e a -> QueryModel
parseQueryModel contentToJson sectionDefiningType sectionWrapper =
    let
        section =
            case sectionWrapper of
                ResolvedSection section ->
                    section

        fields =
            List.filterMap (parseFieldDefinition contentToJson sectionDefiningType) section.subsections
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
