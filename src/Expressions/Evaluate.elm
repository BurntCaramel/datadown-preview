module Expressions.Evaluate
    exposing
        ( processFloatExpression
        , evaulateTokenLines
        , Error(..)
        )

import Expressions.Tokenize exposing (Operator(..), Value(..), Token(..))


type Error
    = InvalidNumberExpression
    | NoInput
    | NotValue
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
                
                Just _ ->
                    Err NotFloat

                Nothing ->
                    Err NoValueForIdentifier

        _ ->
            Err NotValue


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


processFloatExpression : Float -> (String -> Maybe Value) -> List Token -> Result Error Float
processFloatExpression left resolveIdentifier tokens =
    case tokens of
        [] ->
            Ok left

        Operator operator :: right :: [] ->
            requireFloat resolveIdentifier right
                |> Result.map (floatOperator operator left)

        _ ->
            Err InvalidNumberExpression


evaluateTokens : (String -> Maybe Value) -> Maybe Value -> List Token -> Result Error Value
evaluateTokens resolveIdentifier previousValue tokens =
    case tokens of
        hd :: tl ->
            case requireValue resolveIdentifier hd |> Debug.log "require value" of
                Ok (Float f) ->
                    processFloatExpression f resolveIdentifier tl
                        |> Result.map Float
                
                Ok (Bool b) ->
                    Ok (Bool b) |> Debug.log "Had bool"

                Err error ->
                    case hd of
                        Operator operator ->
                            let
                                start : Result Error Float
                                start =
                                    case previousValue of
                                        Just (Float f) ->
                                            Ok f
                                        
                                        Just _ ->
                                            Err NotFloat
                                        
                                        Nothing ->
                                            Ok (identityForOperator operator)
                                
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
