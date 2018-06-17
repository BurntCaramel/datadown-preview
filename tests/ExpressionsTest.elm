module ExpressionsTest exposing (..)

import Expect exposing (Expectation)


-- import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)
import Datadown.Expressions exposing (Operator(..), Token(..), IntExpression(..), BoolExpression(..), Expression(..), ParseError(..), EvaluateError(..), tokenize, parseExpression, evaluateAsInt)


suite : Test
suite =
    describe "Expressions"
        [ describe "Tokenizing"
            [ describe "tokenize"
                [ test "1" <|
                    \_ ->
                        tokenize "1"
                            |> Expect.equal (Ok <| [ IntLiteral 1 ])
                , test "1 + 2" <|
                    \_ ->
                        tokenize "1 + 2"
                            |> Expect.equal (Ok <| [ IntLiteral 1, Operator Add, IntLiteral 2 ])
                , test "2 * 1" <|
                    \_ ->
                        tokenize "2 * 1"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Multiply, IntLiteral 1 ])
                , test "2 * 1 + 3" <|
                    \_ ->
                        tokenize "2 * 1 + 3"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Multiply, IntLiteral 1, Operator Add, IntLiteral 3 ])

                , test "2 + 1 * 3" <|
                    \_ ->
                        tokenize "2 + 1 * 3"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Add, IntLiteral 1, Operator Multiply, IntLiteral 3 ])
                ]
            ]
        , describe "Parsing"
            [ describe "parseExpression"
                [ test "1" <|
                    \_ ->
                        parseExpression "1"
                            |> Expect.equal (Ok <| Int <| UseInt 1)
                , test "1 + 2" <|
                    \_ ->
                        parseExpression "1 + 2"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 1) Add (UseInt 2))
                , test "2 * 1" <|
                    \_ ->
                        parseExpression "2 * 1"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 2) Multiply (UseInt 1))
                , test "2 * 1 + 3" <|
                    \_ ->
                        parseExpression "2 * 1 + 3"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator (UseInt 2) Multiply (UseInt 1))
                                    Add
                                    (UseInt 3)
                                    |> Int
                                    |> Ok
                                )
                , test "2 + 1 * 3" <|
                    \_ ->
                        parseExpression "2 + 1 * 3"
                            |> Expect.equal
                                (IntOperator
                                    (UseInt 2)
                                    Add
                                    (IntOperator (UseInt 1) Multiply (UseInt 3))
                                    |> Int
                                    |> Ok
                                )
                ]
            ]
        ]
