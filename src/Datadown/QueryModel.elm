module Datadown.QueryModel
    exposing
        ( QueryModel
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


type FieldKind
    = String
    | Bool
    | Int
    | Query


type FieldValue
    = StringValue String
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
        String ->
            StringValue ""

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


parseFieldDefinition : ( String, ResolvedSection e a ) -> Maybe FieldDefinition
parseFieldDefinition ( rawTitle, _ ) =
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
                            Just String
                        
                        "Int" ->
                            Just Int

                        _ ->
                            Nothing
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


parseQueryModel : ResolvedSection e a -> QueryModel
parseQueryModel sectionWrapper =
    let
        section =
            case sectionWrapper of
                ResolvedSection section ->
                    section

        fields =
            List.filterMap parseFieldDefinition section.subsections
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

        contentListToJsonList : List (Result e (Content a)) -> List JsonValue
        contentListToJsonList contentList =
            contentList
                |> List.filterMap (Result.toMaybe >> Maybe.andThen (contentToJson >> Result.toMaybe))

        sourceFieldWithName name =
            model.fields
                |> List.filter (\field -> field.name == name)
                |> List.head

        jsonListToValue : FieldKind -> List JsonValue -> Maybe FieldValue
        jsonListToValue kind jsonList =
            case ( kind, jsonList ) of
                ( String, (JsonValue.StringValue ".empty") :: [] ) ->
                    Just <| StringValue ""

                ( String, (JsonValue.StringValue s) :: [] ) ->
                    case String.split "$" s of
                        "" :: sourceFieldName :: [] ->
                            case sourceFieldWithName sourceFieldName of
                                Just field ->
                                    if field.kind == kind then
                                        Just field.value
                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        _ ->
                            Just <| StringValue s

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
                        |> Maybe.map contentListToJsonList
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
