module ExpressionsTest exposing (..)

import Expect exposing (Expectation)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Datadown.Expressions exposing (Operator(..), IntExpression(..), BoolExpression(..), Expression(..), ParseError(..), EvaluateError(..), parseExpression, evaluateAsInt)


suite : Test
suite =
    describe "Parsing"
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
            ]
        ]
