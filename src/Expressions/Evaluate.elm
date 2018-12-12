module Expressions.Evaluate exposing
    ( Error(..)
    , evaluateTokenLines
    )

import Datadown.Rpc
import Expressions.Tokenize exposing (HttpFunction(..), MathFunction(..), Operator(..), Token(..), Url(..), urlToString)
import Json.Value exposing (JsonValue(..))


type Error
    = InvalidNumberExpression
    | Parsing String
    | NoInput
    | NotValue
    | NoValueForIdentifier String
    | Unknown
    | CannotJoinUrl String
    | InvalidValuesForOperator Operator JsonValue JsonValue
    | CannotConvertToJson
    | Rpc Datadown.Rpc.Error


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

        ArrayValue (item :: []) ->
            toFloat item

        _ ->
            Nothing


useString : JsonValue -> Maybe String
useString value =
    case value of
        NumericValue f ->
            Just <| String.fromFloat f

        StringValue s ->
            Just s

        ArrayValue (item :: []) ->
            useString item

        _ ->
            Nothing


rpcJson : String -> Maybe JsonValue -> JsonValue -> JsonValue
rpcJson method maybeParams id =
    [ Just ( "jsonrpc", StringValue "2.0" )
    , Just ( "method", StringValue method )
    , Maybe.map (\b -> ( "params", b )) maybeParams
    , Just ( "id", id )
    ]
        |> List.filterMap identity
        |> ObjectValue


rpcJsonForHttpGet : String -> JsonValue
rpcJsonForHttpGet url =
    rpcJson "HTTP"
        (Just <|
            ObjectValue
                [ ( "method", StringValue "GET" )
                , ( "type", StringValue "JSON" )
                , ( "url", StringValue url )
                ]
        )
        (StringValue <| "GET json " ++ url)


processOperator : Operator -> JsonValue -> JsonValue -> Result Error JsonValue
processOperator op jsonA jsonB =
    case op of
        Add ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a + b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        Subtract ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a - b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        Multiply ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a * b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        Divide ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a / b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        Exponentiate ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <| NumericValue <| a ^ b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        EqualTo ->
            Ok (BoolValue (jsonA == jsonB))

        LessThan orEqual ->
            case ( jsonA, jsonB ) of
                ( NumericValue a, NumericValue b ) ->
                    Ok <|
                        BoolValue <|
                            if orEqual then
                                a <= b

                            else
                                a < b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        GreaterThan orEqual ->
            case ( jsonA, jsonB ) of
                ( NumericValue a, NumericValue b ) ->
                    Ok <|
                        BoolValue <|
                            if orEqual then
                                a >= b

                            else
                                a > b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        MathModule function ->
            case ( toFloat jsonA, toFloat jsonB ) of
                ( Just a, Just b ) ->
                    Ok <|
                        NumericValue <|
                            case function of
                                Sine ->
                                    a * sin b

                                Cosine ->
                                    a * cos b

                                Tangent ->
                                    a * tan b

                                Turns ->
                                    a * turns b

                _ ->
                    Err <| InvalidValuesForOperator op jsonA jsonB

        HttpModule function ->
            case function of
                GetJson ->
                    case useString jsonB of
                        Just url ->
                            Ok <| rpcJsonForHttpGet url

                        _ ->
                            Err <| InvalidValuesForOperator op jsonA jsonB


valueOperatorOnList : Operator -> JsonValue -> List JsonValue -> Result Error JsonValue
valueOperatorOnList op json rest =
    List.foldl ((processOperator op |> (\f b a -> f a b)) >> Result.andThen) (Ok json) rest


urlWithPathsList : String -> List JsonValue -> Result Error String
urlWithPathsList prefix pathValues =
    case pathValues of
        [] ->
            Ok prefix

        hd :: tl ->
            case useString hd of
                Just string ->
                    let
                        newPrefix =
                            case String.right 1 prefix of
                                "/" ->
                                    prefix ++ string

                                _ ->
                                    prefix ++ "/" ++ string
                    in
                    urlWithPathsList newPrefix tl

                Nothing ->
                    Err <| CannotJoinUrl prefix


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

        Url (Math string) ->
            case string of
                "pi" ->
                    Ok <| NumericValue pi

                "e" ->
                    Ok <| NumericValue e

                _ ->
                    Err NotValue
                        |> Debug.log (Debug.toString string)

        Url (Time string) ->
            case resolveIdentifier ("time:" ++ string) of
                Ok v ->
                    Ok v

                Err e ->
                    Err (NoValueForIdentifier ("time:" ++ string))

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
        (Url (Https baseString)) :: tl ->
            requireValueList resolveIdentifier tl
                |> Result.andThen (urlWithPathsList ("https:" ++ baseString))
                |> Result.map rpcJsonForHttpGet

        (Url (Math "turns")) :: a1 :: [] ->
            requireValue resolveIdentifier a1
                |> Result.andThen (processOperator (MathModule Turns) (previousValue |> Maybe.withDefault (NumericValue 1)))

        (Url (Math "cos")) :: a1 :: [] ->
            requireValue resolveIdentifier a1
                |> Result.andThen (processOperator (MathModule Cosine) (previousValue |> Maybe.withDefault (NumericValue 1)))

        (Url (Math "sin")) :: a1 :: [] ->
            requireValue resolveIdentifier a1
                |> Result.andThen (processOperator (MathModule Sine) (previousValue |> Maybe.withDefault (NumericValue 1)))

        (Url (Math "tan")) :: a1 :: [] ->
            requireValue resolveIdentifier a1
                |> Result.andThen (processOperator (MathModule Tangent) (previousValue |> Maybe.withDefault (NumericValue 1)))

        (Operator operator) :: tl ->
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
            Result.map2 (\a b -> ( a, b )) start values
                |> Result.andThen ((\f ( a, b ) -> f a b) <| valueOperatorOnList operator)

        hd :: tl ->
            case requireValue resolveIdentifier hd of
                Ok value ->
                    processValueExpression value resolveIdentifier tl

                Err error ->
                    Err error

        [] ->
            Err NoInput


evaluateTokenLines : (String -> Result e JsonValue) -> List (List Token) -> Result Error JsonValue
evaluateTokenLines resolveIdentifier lines =
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
