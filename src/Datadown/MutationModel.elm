module Datadown.MutationModel
    exposing
        ( MutationModel
        , parseMutationModel
        )

{-| MutationModel

@docs parseMutationModel


## Types

@docs MutationModel

-}

import JsonValue exposing (JsonValue)
import Datadown exposing (Content(..))
import Datadown.Process exposing (ResolvedSection(..))
import Datadown.QueryModel exposing (FieldKind(..), FieldDefinition, queryFieldDefinition, parseFieldDefinitionFromTitleAndSection)


type alias MutationModel =
    { fields : List FieldDefinition
    }


parseFieldDefinition : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ( String, ResolvedSection e a ) -> Maybe FieldDefinition
parseFieldDefinition contentToJson sectionDefiningType ( title, ResolvedSection section ) =
    case (String.trim title) of
        "" ->
            Nothing

        name ->
            let 
                args =
                    section.subsections
                        |> List.filterMap (parseFieldDefinitionFromTitleAndSection contentToJson sectionDefiningType)
            in
                Just <| queryFieldDefinition name args


parseMutationModel : (Content a -> Result e JsonValue) -> (String -> Maybe ( String, ResolvedSection e a )) -> ResolvedSection e a -> MutationModel
parseMutationModel contentToJson sectionDefiningType (ResolvedSection section) =
    let
        fields =
            section.subsections
                |> List.filterMap (parseFieldDefinition contentToJson sectionDefiningType)
    in
        MutationModel fields
