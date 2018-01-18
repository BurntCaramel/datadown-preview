module Expressions.Evaluate
    exposing
        ( resolveFloat
        , resolveTokens
        , evaulateTokenLines
        , Error(..)
        )

import Expressions.Tokenize exposing (Operator(..), Value(..), Token(..))


type Error
    = InvalidNumberExpression
    | NoInput
    | NotFloat
    | NoValueForIdentifier
    | Unknown


identityForOperator : Operator -> Float
identityForOperator op =
    case op of
        Add ->
            0
        
        Subtract ->
            0
        
        _ ->
            1
        



floatOperator : Operator -> Float -> Float -> Float
floatOperator op =
    case op of
        Add ->
            (+)

        Subtract ->
            (-)

        Multiply ->
            (*)

        Divide ->
            (/)

        Exponentiate ->
            (^)


floatOperatorOnList : Operator -> Float -> List Float -> Float
floatOperatorOnList op a rest =
    List.foldl (floatOperator op |> flip) a rest


requireFloat : (String -> Maybe Value) -> Token -> Result Error Float
requireFloat resolveIdentifier token =
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


requireFloatList : (String -> Maybe Value) -> List Token -> Result Error (List Float)
requireFloatList resolveIdentifier tokens =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])

        -- From Result.Extra
    in
        tokens
            |> List.map (requireFloat resolveIdentifier)
            |> combineResults


resolveFloat : Float -> (String -> Maybe Value) -> List Token -> Result Error Float
resolveFloat a resolveIdentifier tokens =
    case tokens of
        b :: c :: rest ->
            case b of
                Operator operator ->
                    case requireFloat resolveIdentifier c of
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
            case requireFloat resolveIdentifier hd of
                Ok f ->
                    resolveFloat f resolveIdentifier tl
                        |> Result.map (\f -> [ Value (Float f) ])

                Err error ->
                    case hd of
                        Operator operator ->
                            -- Support e.g. * 5 5 5 == 125
                            requireFloatList resolveIdentifier tl
                                |> Result.map (floatOperatorOnList operator (identityForOperator operator))
                                |> Result.map (\f -> [ Value (Float f) ])

                        _ ->
                            Err error

        [] ->
            Ok []


evaluateTokens : (String -> Maybe Value) -> Maybe Value -> List Token -> Result Error Value
evaluateTokens resolveIdentifier previousValue tokens =
    case tokens of
        hd :: tl ->
            case requireFloat resolveIdentifier hd of
                Ok f ->
                    resolveFloat f resolveIdentifier tl
                        |> Result.map Float

                Err error ->
                    case hd of
                        Operator operator ->
                            let
                                start : Result Error Float
                                start =
                                    case previousValue of
                                        Just (Float f) ->
                                            Ok f
                                        
                                        Nothing ->
                                            Ok (identityForOperator operator)
                                        
                                        -- _ ->
                                        --     Err NotFloat
                                
                                floatList : Result Error (List Float)
                                floatList =
                                    requireFloatList resolveIdentifier tl
                            in          
                                -- Support e.g. * 5 5 5 == 125
                                Result.map2 (floatOperatorOnList operator) start floatList
                                    |> Result.map Float

                        _ ->
                            Err error

        [] ->
            Err NoInput


evaulateTokenLines : (String -> Maybe Value) -> List (List Token) -> Result Error Value
evaulateTokenLines resolveIdentifier lines =
    let
        reducer : List Token -> Maybe (Result Error Value) -> Maybe (Result Error Value)
        reducer =
            \tokens previousResult ->
                case previousResult of
                    Nothing ->
                        Just (evaluateTokens resolveIdentifier Nothing tokens)
                    
                    Just (Ok value) ->
                        Just (evaluateTokens resolveIdentifier (Just value) tokens)
                    
                    Just (Err error) ->
                        Just (Err error)
    in
        List.foldl reducer Nothing lines
            |> Maybe.withDefault (Err NoInput)
