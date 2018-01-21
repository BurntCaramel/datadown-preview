module Expressions.Evaluate
    exposing
        ( evaulateTokenLines
        , Error(..)
        )

import Expressions.Tokenize exposing (Operator(..), Value(..), Token(..))


type Error
    = InvalidNumberExpression
    | NoInput
    | NotValue
    | NoValueForIdentifier
    | Unknown
    | InvalidValuesForOperator Operator Value Value


identityForOperator : Operator -> Maybe Value
identityForOperator op =
    case op of
        Add ->
            Just <| Float 0
        
        Subtract ->
            Just <| Float 0
        
        Multiply ->
            Just <| Float 1
        
        _ ->
            Nothing


processOperator : Operator -> Value -> Value -> Result Error Value
processOperator op a b =
    case op of
        Add ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok <| Float <| a + b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Subtract ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok <| Float <| a - b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Multiply ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok <| Float <| a * b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Divide ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok <| Float <| a / b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Exponentiate ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok <| Float <| a ^ b

                _ ->
                    Err <| InvalidValuesForOperator op a b
        
        EqualTo ->
            Ok (Bool (a == b))
        
        LessThan orEqual ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok << Bool <|
                        if orEqual then
                            a <= b
                        else
                            a < b

                _ ->
                    Err <| InvalidValuesForOperator op a b
        
        GreaterThan orEqual ->
            case (a, b) of
                (Float a, Float b) ->
                    Ok << Bool <|
                        if orEqual then
                            a >= b
                        else
                            a > b

                _ ->
                    Err <| InvalidValuesForOperator op a b


valueOperatorOnList : Operator -> Value -> List Value -> Result Error Value
valueOperatorOnList op a rest =
    List.foldl ((processOperator op |> flip) >> Result.andThen) (Ok a) rest


requireValue : (String -> Maybe Value) -> Token -> Result Error Value
requireValue resolveIdentifier token =
    case token of
        Value v ->
            Ok v

        Identifier identifier ->
            case resolveIdentifier identifier of
                Just v ->
                    Ok v

                Nothing ->
                    Err NoValueForIdentifier

        _ ->
            Err NotValue


requireValueList : (String -> Maybe Value) -> List Token -> Result Error (List Value)
requireValueList resolveIdentifier tokens =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        tokens
            |> List.map (requireValue resolveIdentifier)
            |> combineResults


processValueExpression : Value -> (String -> Maybe Value) -> List Token -> Result Error Value
processValueExpression left resolveIdentifier tokens =
    case tokens of
        [] ->
            Ok left

        Operator operator :: right :: [] ->
            requireValue resolveIdentifier right
                |> Result.andThen (processOperator operator left)

        _ ->
            Err InvalidNumberExpression


evaluateTokens : (String -> Maybe Value) -> Maybe Value -> List Token -> Result Error Value
evaluateTokens resolveIdentifier previousValue tokens =
    case tokens of
        hd :: tl ->
            case requireValue resolveIdentifier hd of
                Ok value ->
                    processValueExpression value resolveIdentifier tl

                Err error ->
                    case hd of
                        Operator operator ->
                            let
                                start : Result Error Value
                                start =
                                    case previousValue of
                                        Just v ->
                                            Ok v
                                        
                                        Nothing ->
                                            identityForOperator operator
                                                |> Result.fromMaybe NoInput
                                
                                values : Result Error (List Value)
                                values =
                                    requireValueList resolveIdentifier tl
                            in          
                                -- Support e.g. * 5 5 5 == 125
                                Result.map2 (,) start values
                                    |> Result.andThen (uncurry <| valueOperatorOnList operator)

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
