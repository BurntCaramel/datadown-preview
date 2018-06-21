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
                , test "1 - 2" <|
                    \_ ->
                        tokenize "1 - 2"
                            |> Expect.equal (Ok <| [ IntLiteral 1, Operator Subtract, IntLiteral 2 ])
                , test "2 * 1" <|
                    \_ ->
                        tokenize "2 * 1"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Multiply, IntLiteral 1 ])
                , test "2 / 3" <|
                    \_ ->
                        tokenize "2 / 3"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Divide, IntLiteral 3 ])
                , test "2 * 1 + 3" <|
                    \_ ->
                        tokenize "2 * 1 + 3"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Multiply, IntLiteral 1, Operator Add, IntLiteral 3 ])
                , test "2 + 1 * 3" <|
                    \_ ->
                        tokenize "2 + 1 * 3"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Add, IntLiteral 1, Operator Multiply, IntLiteral 3 ])
                , test "2 + 1 * 3 / 4" <|
                    \_ ->
                        tokenize "2 + 1 * 3 / 4"
                            |> Expect.equal (Ok <| [ IntLiteral 2, Operator Add, IntLiteral 1, Operator Multiply, IntLiteral 3, Operator Divide, IntLiteral 4 ])
                ]
            ]
        , describe "Parsing"
            [ describe "parseExpression Ok"
                [ test "1" <|
                    \_ ->
                        parseExpression "1"
                            |> Expect.equal (Ok <| Int <| UseInt 1)
                , test "1 + 2" <|
                    \_ ->
                        parseExpression "1 + 2"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 1) Add (UseInt 2))
                , test "1 - 2" <|
                    \_ ->
                        parseExpression "1 - 2"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 1) Subtract (UseInt 2))
                , test "2 * 1" <|
                    \_ ->
                        parseExpression "2 * 1"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 2) Multiply (UseInt 1))
                , test "2 / 5" <|
                    \_ ->
                        parseExpression "2 / 5"
                            |> Expect.equal (Ok <| Int <| IntOperator (UseInt 2) Divide (UseInt 5))
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
                , test "2 / 5 + 3" <|
                    \_ ->
                        parseExpression "2 / 5 + 3"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator (UseInt 2) Divide (UseInt 5))
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
                , test "2 / 5 + 1 * 3" <|
                    \_ ->
                        parseExpression "2 / 5 + 1 * 3"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator (UseInt 2) Divide (UseInt 5))
                                    Add
                                    (IntOperator (UseInt 1) Multiply (UseInt 3))
                                    |> Int
                                    |> Ok
                                )
                , test "2 / 5 / 3" <|
                    \_ ->
                        parseExpression "2 / 5 / 3"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator (UseInt 2) Divide (UseInt 5))
                                    Divide
                                    (UseInt 3)
                                    |> Int
                                    |> Ok
                                )
                , test "2 / 5 / 3 / 2" <|
                    \_ ->
                        parseExpression "2 / 5 / 3 / 2"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator
                                        (IntOperator (UseInt 2) Divide (UseInt 5))
                                        Divide
                                        (UseInt 3)
                                    )
                                    Divide
                                    (UseInt 2)
                                    |> Int
                                    |> Ok
                                )
                , test "2 / 5 / 3 + 1 * 3 * 2" <|
                    \_ ->
                        parseExpression "2 / 5 / 3 + 1 * 3 * 2"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator
                                        (IntOperator (UseInt 2) Divide (UseInt 5))
                                        Divide
                                        (UseInt 3)
                                    )
                                    Add
                                    (IntOperator
                                        (IntOperator (UseInt 1) Multiply (UseInt 3))
                                        Multiply
                                        (UseInt 2)
                                    )
                                    |> Int
                                    |> Ok
                                )
                , test "3 + 1 * 2 * 4 + 5" <|
                    \_ ->
                        parseExpression "3 + 1 * 2 * 4 + 5"
                            |> Expect.equal
                                (IntOperator
                                    (IntOperator
                                        (UseInt 3)
                                        Add
                                        (IntOperator
                                            (IntOperator
                                                (UseInt 1)
                                                Multiply
                                                (UseInt 2)
                                            )
                                            Multiply
                                            (UseInt 4)
                                        )
                                    )
                                    Add
                                    (UseInt 5)
                                    |> Int
                                    |> Ok
                                )
                ]
            , describe "parseExpression Err"
                [ test "+" <|
                    \_ ->
                        parseExpression "+"
                            |> Expect.equal (Err <| OperatorMissingRight Add)
                , test "1 +" <|
                    \_ ->
                        parseExpression "1 +"
                            |> Expect.equal (Err <| OperatorMissingRight Add)
                , test "+ 1" <|
                    \_ ->
                        parseExpression "+ 1"
                            |> Expect.equal (Err <| OperatorMissingLeft Add)
                , test "1 2" <|
                    \_ ->
                        parseExpression "1 2"
                            |> Expect.equal (Err <| Invalid (Int (UseInt 2)) [ IntLiteral 1 ])
                , test "1 + * 2" <|
                    \_ ->
                        parseExpression "1 + * 2"
                            |> Expect.equal (Err <| OperatorMissingRight Add)
                ]
            ]
        ]
