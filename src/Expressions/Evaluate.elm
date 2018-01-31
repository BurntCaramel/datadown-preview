module Expressions.Evaluate
    exposing
        ( evaulateTokenLines
        , Error(..)
        )

import Expressions.Tokenize exposing (Operator(..), Token(..), MathFunction(..))
import JsonValue exposing (JsonValue(..))


type Error
    = InvalidNumberExpression
    | Parsing
    | NoInput
    | NotValue
    | NoValueForIdentifier String
    | Unknown
    | InvalidValuesForOperator Operator JsonValue JsonValue
    | CannotConvertToJson


identityForOperator : Operator -> Maybe JsonValue
identityForOperator op =
    case op of
        Add ->
            Just <| NumericValue 0

        Subtract ->
            Just <| NumericValue 0

        _ ->
            Just <| NumericValue 1


toFloat : JsonValue -> Maybe Float
toFloat value =
    case value of
        NumericValue f ->
            Just f

        StringValue s ->
            s
                |> String.toFloat
                |> Result.toMaybe

        _ ->
            Nothing


processOperator : Operator -> JsonValue -> JsonValue -> Result Error JsonValue
processOperator op a b =
    case op of
        Add ->
            case ( toFloat a, toFloat b ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a + b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Subtract ->
            case ( toFloat a, toFloat b ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a - b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Multiply ->
            case ( toFloat a, toFloat b ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a * b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Divide ->
            case ( toFloat a, toFloat b ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a / b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        Exponentiate ->
            case ( toFloat a, toFloat b ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a ^ b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        EqualTo ->
            Ok (BoolValue (a == b))

        LessThan orEqual ->
            case ( a, b ) of
                ( NumericValue a, NumericValue b ) ->
                    Ok <|
                        BoolValue <|
                            if orEqual then
                                a <= b
                            else
                                a < b

                _ ->
                    Err <| InvalidValuesForOperator op a b

        GreaterThan orEqual ->
            case ( a, b ) of
                ( NumericValue a, NumericValue b ) ->
                    Ok <|
                        BoolValue <|
                            if orEqual then
                                a >= b
                            else
                                a > b

                _ ->
                    Err <| InvalidValuesForOperator op a b
        
        Math function ->
            case ( a, b ) of
                ( NumericValue a, NumericValue b ) ->
                    Ok <| NumericValue <|
                        case function of
                            Sine ->
                                a * (sin b)

                            Cosine ->
                                a * (cos b)
                            
                            Tangent ->
                                a * (tan b)
                            
                            Turns ->
                                a * (turns b)


                _ ->
                    Err <| InvalidValuesForOperator op a b



valueOperatorOnList : Operator -> JsonValue -> List JsonValue -> Result Error JsonValue
valueOperatorOnList op a rest =
    List.foldl ((processOperator op |> flip) >> Result.andThen) (Ok a) rest


requireValue : (String -> Result e JsonValue) -> Token -> Result Error JsonValue
requireValue resolveIdentifier token =
    case token of
        Value v ->
            Ok v

        Identifier identifier ->
            case resolveIdentifier identifier of
                Ok v ->
                    Ok v

                Err e ->
                    Err (NoValueForIdentifier identifier)

        _ ->
            Err NotValue


requireValueList : (String -> Result e JsonValue) -> List Token -> Result Error (List JsonValue)
requireValueList resolveIdentifier tokens =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        tokens
            |> List.map (requireValue resolveIdentifier)
            |> combineResults


processValueExpression : JsonValue -> (String -> Result e JsonValue) -> List Token -> Result Error JsonValue
processValueExpression left resolveIdentifier tokens =
    case tokens of
        [] ->
            Ok left

        (Operator operator) :: right :: [] ->
            requireValue resolveIdentifier right
                |> Result.andThen (processOperator operator left)

        _ ->
            Err InvalidNumberExpression


evaluateTokens : (String -> Result e JsonValue) -> Maybe JsonValue -> List Token -> Result Error JsonValue
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
                                start : Result Error JsonValue
                                start =
                                    case previousValue of
                                        Just v ->
                                            Ok v

                                        Nothing ->
                                            identityForOperator operator
                                                |> Result.fromMaybe NoInput

                                values : Result Error (List JsonValue)
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


evaulateTokenLines : (String -> Result e JsonValue) -> List (List Token) -> Result Error JsonValue
evaulateTokenLines resolveIdentifier lines =
    let
        reducer : List Token -> Maybe (Result Error JsonValue) -> Maybe (Result Error JsonValue)
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
