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

import Datadown.Process exposing (ResolvedSection(..))
import Datadown.QueryModel exposing (FieldKind(..), FieldDefinition, queryFieldDefinition)


type alias MutationModel =
    { fields : List FieldDefinition
    }


parseFieldDefinition : ( String, ResolvedSection e a ) -> Maybe FieldDefinition
parseFieldDefinition ( title, _ ) =
    case (String.trim title) of
        "" ->
            Nothing

        name ->
            Just <| queryFieldDefinition name


parseMutationModel : ResolvedSection e a -> MutationModel
parseMutationModel sectionWrapper =
    let
        section =
            case sectionWrapper of
                ResolvedSection section ->
                    section

        fields =
            List.filterMap parseFieldDefinition section.subsections
    in
        MutationModel fields
