module ExpressionsTest exposing (..)

import Expect exposing (Expectation)


-- import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)
import Datadown.Expressions exposing (Operator(..), Token(..), IntExpression(..), BoolExpression(..), Expression(..), ParseError(..), EvaluateError(..), tokenize, parseExpression, evaluateAsInt, evaluateAsJson)
import Datadown.Url exposing (Url(..), MathFunction(..))
import Datadown.Procedures exposing (Procedure(..))
import JsonValue exposing (..)
import Dict


suite : Test
suite =
    describe "Expressions"
        [ describe "Tokenizing"
            [ describe "tokenize"
                [ test "1" <|
                    \_ ->
                        tokenize "1"
                            |> Expect.equal
                                ([ IntLiteral 1
                                 ]
                                    |> Ok
                                )
                , test "1 + 2" <|
                    \_ ->
                        tokenize "1 + 2"
                            |> Expect.equal
                                ([ IntLiteral 1
                                 , Operator Add
                                 , IntLiteral 2
                                 ]
                                    |> Ok
                                )
                , test "1 - 2" <|
                    \_ ->
                        tokenize "1 - 2"
                            |> Expect.equal
                                ([ IntLiteral 1
                                 , Operator Subtract
                                 , IntLiteral 2
                                 ]
                                    |> Ok
                                )
                , test "2 * 1" <|
                    \_ ->
                        tokenize "2 * 1"
                            |> Expect.equal
                                ([ IntLiteral 2
                                 , Operator Multiply
                                 , IntLiteral 1
                                 ]
                                    |> Ok
                                )
                , test "2 / 3" <|
                    \_ ->
                        tokenize "2 / 3"
                            |> Expect.equal
                                ([ IntLiteral 2
                                 , Operator Divide
                                 , IntLiteral 3
                                 ]
                                    |> Ok
                                )
                , test "2 * 1 + 3" <|
                    \_ ->
                        tokenize "2 * 1 + 3"
                            |> Expect.equal
                                ([ IntLiteral 2
                                 , Operator Multiply
                                 , IntLiteral 1
                                 , Operator Add
                                 , IntLiteral 3
                                 ]
                                    |> Ok
                                )
                , test "2 + 1 * 3" <|
                    \_ ->
                        tokenize "2 + 1 * 3"
                            |> Expect.equal
                                ([ IntLiteral 2
                                 , Operator Add
                                 , IntLiteral 1
                                 , Operator Multiply
                                 , IntLiteral 3
                                 ]
                                    |> Ok
                                )
                , test "2 + 1 * 3 / 4" <|
                    \_ ->
                        tokenize "2 + 1 * 3 / 4"
                            |> Expect.equal
                                ([ IntLiteral 2
                                 , Operator Add
                                 , IntLiteral 1
                                 , Operator Multiply
                                 , IntLiteral 3
                                 , Operator Divide
                                 , IntLiteral 4
                                 ]
                                    |> Ok
                                )
                , test "https://www.example.com/" <|
                    \_ ->
                        tokenize "https://www.example.com/"
                            |> Expect.equal
                                ([ Url (Https "//www.example.com/")
                                 ]
                                    |> Ok
                                )
                , test "math:pi" <|
                    \_ ->
                        tokenize "math:pi"
                            |> Expect.equal
                                ([ Url (Math Pi)
                                 ]
                                    |> Ok
                                )
                , test "math:e" <|
                    \_ ->
                        tokenize "math:e"
                            |> Expect.equal
                                ([ Url (Math E)
                                 ]
                                    |> Ok
                                )
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
                , test "https://api.example.com/" <|
                    \_ ->
                        parseExpression "https://api.example.com/"
                            |> Expect.equal
                                (HttpGetJson "https://api.example.com/"
                                    |> Procedure
                                    |> Ok
                                )
                , test "math:pi" <|
                    \_ ->
                        parseExpression "math:pi"
                            |> Expect.equal
                                (Math0 Pi
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
        , describe "Evaluating"
            [ describe "evaluateAsJson"
                [ test "5" <|
                    \_ ->
                        (Int
                            (UseInt 5)
                        )
                            |> evaluateAsJson (\_ -> Nothing)
                            |> Expect.equal (NumericValue 5.0 |> Ok)
                , test "5 + 3" <|
                    \_ ->
                        (Int
                            (IntOperator (UseInt 5) Add (UseInt 3))
                        )
                            |> evaluateAsJson (\_ -> Nothing)
                            |> Expect.equal (NumericValue 8.0 |> Ok)
                , test "5 + 3 * 4" <|
                    \_ ->
                        (Int
                            (IntOperator
                                (UseInt 5)
                                Add
                                (IntOperator
                                    (UseInt 3)
                                    Multiply
                                    (UseInt 4)
                                )
                            )
                        )
                            |> evaluateAsJson (\_ -> Nothing)
                            |> Expect.equal (NumericValue 17.0 |> Ok)
                , test "$a + 3 * $b" <|
                    \_ ->
                        (Int
                            (IntOperator
                                (ReadInt "a")
                                Add
                                (IntOperator
                                    (UseInt 3)
                                    Multiply
                                    (ReadInt "b")
                                )
                            )
                        )
                            |> evaluateAsJson
                                ([ ( "a", 5 ), ( "b", 4 ) ]
                                    |> Dict.fromList
                                    |> flip Dict.get
                                )
                            |> Expect.equal (NumericValue 17.0 |> Ok)
                , test "HttpGetJson" <|
                    \_ ->
                        (Procedure
                            (HttpGetJson "https://api.example.com/")
                        )
                            |> evaluateAsJson (\_ -> Nothing)
                            |> Expect.equal
                                (ObjectValue
                                    ([ ( "jsonrpc", StringValue "2.0" )
                                     , ( "method", StringValue "HTTP" )
                                     , ( "params"
                                       , ObjectValue
                                            ([ ( "method", StringValue "GET" )
                                             , ( "acceptMimeType", StringValue "application/json" )
                                             , ( "url", StringValue "https://api.example.com/" )
                                             ]
                                            )
                                       )
                                     , ( "id", StringValue "HTTP GET application/json https://api.example.com/" )
                                     ]
                                    )
                                    |> Ok
                                )
                , test "Math0 Pi" <|
                    \_ ->
                        (Int <| Math0 Pi)
                            |> evaluateAsJson (\_ -> Nothing)
                            |> Expect.equal
                                (NumericValue 3
                                    |> Ok
                                )
                ]
            ]
        ]
