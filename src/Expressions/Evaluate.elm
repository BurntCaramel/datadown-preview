module Expressions.Evaluate
    exposing
        ( resolveFloat
        , resolveTokens
        , Error(..)
        )

import Expressions.Tokenize exposing (Operator(..), Value(..), Token(..))


type Error
    = InvalidNumberExpression
    | NotFloat
    | NoValueForIdentifier
    | Unknown


floatOperator : Operator -> Float -> Float -> Float
floatOperator op a b =
    case op of
        Add ->
            a + b

        Subtract ->
            a - b


requireFloat : Token -> (String -> Maybe Value) -> Result Error Float
requireFloat token resolveIdentifier =
    case token of
        Value (Float f) ->
            Ok f

        Identifier identifier ->
            case resolveIdentifier identifier of
                Just (Float f) ->
                    Ok f

                Nothing ->
                    Err NoValueForIdentifier

        _ ->
            Err NotFloat


resolveFloat : Float -> (String -> Maybe Value) -> List Token -> Result Error Float
resolveFloat a resolveIdentifier tokens =
    case tokens of
        b :: c :: rest ->
            case b of
                Operator operator ->
                    case requireFloat c resolveIdentifier of
                        Ok c ->
                            Ok (floatOperator operator a c)

                        Err error ->
                            Err error

                _ ->
                    Err InvalidNumberExpression

        [] ->
            Ok a

        _ ->
            Err InvalidNumberExpression


resolveTokens : (String -> Maybe Value) -> List Token -> Result Error (List Token)
resolveTokens resolveIdentifier tokens =
    case tokens of
        hd :: tl ->
            case requireFloat hd resolveIdentifier of
                Ok f ->
                    resolveFloat f resolveIdentifier tl
                        |> Result.map (\f -> [ Value (Float f) ])

                Err error ->
                    Err error

        [] ->
            Ok []
