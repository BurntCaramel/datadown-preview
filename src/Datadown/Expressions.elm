module Datadown.Expressions exposing
    ( BoolExpression(..)
    , EvaluateError(..)
    , Expression(..)
    , IntExpression(..)
    , Operator(..)
    , ParseError(..)
    , Token(..)
    , evaluateAsInt
    , evaluateAsJson
    , parseExpression
    , tokenize
    )

import Char
import Datadown.Procedures exposing (Procedure(..), toRpcJson)
import Datadown.Url exposing (MathFunction(..), TimeFunction(..), Url(..), schemeAndStringToUrl)
import Json.Value exposing (..)
import Parser exposing (..)
import Set exposing (Set)


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide


precendenceOfOperator : Operator -> Int
precendenceOfOperator op =
    case op of
        Multiply ->
            2

        Divide ->
            2

        Add ->
            1

        Subtract ->
            1


type Token
    = Identifier String
    | IntLiteral Int
    | BoolLiteral Bool
    | Operator Operator
    | Url Url


isSpace : Char -> Bool
isSpace c =
    c == ' '


optionalSpaces : Parser ()
optionalSpaces =
    chompWhile isSpace


isIdentifierBodyChar : Char -> Bool
isIdentifierBodyChar c =
    Char.isLower c
        || Char.isUpper c
        || Char.isDigit c
        || c
        == '_'


identifier : Parser Token
identifier =
    map Identifier <|
        variable
            { start = (\c -> c == '$')
            , inner = isIdentifierBodyChar
            , reserved = Set.empty
            }


operator : Parser Operator
operator =
    oneOf
        [ succeed Add
            |. symbol "+"
        , succeed Subtract
            |. symbol "-"
        , succeed Multiply
            |. symbol "*"
        , succeed Divide
            |. symbol "/"
        ]


whitespaceChars : Set Char
whitespaceChars =
    [ ' ', '\n', '\u{000D}', '\t' ]
        |> Set.fromList


isNonWhitespace : Char -> Bool
isNonWhitespace c =
    Set.member c whitespaceChars
        |> not


url : Parser Token
url =
    backtrackable <|
    map Url <|
    succeed schemeAndStringToUrl
        |= (chompWhile Char.isLower |> getChompedString)
        |. symbol ":"
        |= (chompWhile isNonWhitespace |> getChompedString)


token : Parser Token
token =
    oneOf
        [ identifier
        , succeed Operator
            |= operator
        , succeed IntLiteral
            |= int
        , succeed (BoolLiteral True)
            |. symbol ".true"
        , succeed (BoolLiteral False)
            |. symbol ".false"
        , url
        ]


nextToken : Parser Token
nextToken =
    succeed identity
        |. optionalSpaces
        |= token


tokensHelp : List Token -> Parser (List Token)
tokensHelp revTokens =
    oneOf
        [ nextToken
            |> andThen (\t -> tokensHelp (t :: revTokens))
        , succeed (List.reverse revTokens)
        ]


tokens : Parser (List Token)
tokens =
    succeed identity
        |. optionalSpaces
        |= andThen (\t -> tokensHelp [ t ]) token
        |. optionalSpaces


tokenize : String -> Result (List Parser.DeadEnd) (List Token)
tokenize input =
    run tokens input


type IntExpression
    = ReadInt String
    | UseInt Int
    | IntOperator IntExpression Operator IntExpression
    | Math0 MathFunction
    | Time0 TimeFunction


type BoolExpression
    = UseBool Bool


type Expression
    = Empty
    | Int IntExpression
    | Bool BoolExpression
    | Procedure Procedure


type ParseError
    = CannotBeEmpty
    | Tokenization (List Parser.DeadEnd)
    | OperatorMissingLeft Operator
    | OperatorMissingRight Operator
    | OperatorMustHaveNumericLeft Expression Operator
    | OperatorMustHaveNumericRight Expression Operator Expression
    | UnsupportedUrl Url
    | Invalid Expression (List Token)


parseNext : Expression -> List Token -> Result ParseError Expression
parseNext right tokensList =
    case ( right, tokensList ) of
        ( Empty, [] ) ->
            Err CannotBeEmpty

        ( _, [] ) ->
            Ok right

        ( Empty, (Identifier id) :: rest ) ->
            parseNext (ReadInt id |> Int) rest

        ( Empty, (IntLiteral i) :: rest ) ->
            parseNext (UseInt i |> Int) rest

        ( Empty, (BoolLiteral b) :: rest ) ->
            parseNext (UseBool b |> Bool) rest

        ( Empty, (Url urlWithScheme) :: rest ) ->
            case urlWithScheme of
                Math (Ok f) ->
                    Math0 f
                        |> Int
                        |> Ok

                Https sansScheme ->
                    let
                        urlString =
                            "https:" ++ sansScheme
                    in
                    HttpGetJson urlString
                        |> Procedure
                        |> Ok

                _ ->
                    Err <| UnsupportedUrl urlWithScheme

        ( Empty, (Operator op) :: rest ) ->
            Err <| OperatorMissingRight op

        ( first, (Operator op) :: [] ) ->
            Err <| OperatorMissingLeft op

        ( Int first, (Operator op) :: rest ) ->
            case parseNext Empty rest of
                Ok (Int (IntOperator third nextOp second)) ->
                    if precendenceOfOperator nextOp >= precendenceOfOperator op then
                        Ok <| Int <| IntOperator (IntOperator third nextOp second) op first

                    else
                        Ok <| Int <| IntOperator third nextOp (IntOperator second op first)

                Ok (Int second) ->
                    Ok <| Int <| IntOperator second op first

                Ok second ->
                    Err <| OperatorMustHaveNumericRight (Int first) op second

                Err error ->
                    Err error

        ( _, (Operator op) :: rest ) ->
            Err <| OperatorMustHaveNumericLeft right op

        ( _, _ ) ->
            Err <| Invalid right tokensList


parseTokens : List Token -> Result ParseError Expression
parseTokens tokensList =
    tokensList
        |> List.reverse
        |> parseNext Empty


parseExpression : String -> Result ParseError Expression
parseExpression input =
    input
        |> tokenize
        |> Result.mapError Tokenization
        |> Result.andThen parseTokens


type EvaluateError
    = CannotEvaluateExpression Expression
    | ValueForIdentifierMustBeInt String


evaluateIntExpression : (String -> Maybe Int) -> IntExpression -> Result EvaluateError Int
evaluateIntExpression resolveIdentifier expression =
    case expression of
        ReadInt id ->
            case resolveIdentifier id of
                Just i ->
                    Ok i

                Nothing ->
                    Err (ValueForIdentifierMustBeInt id)

        UseInt i ->
            Ok i

        IntOperator left Add right ->
            Result.map2 (+)
                (evaluateIntExpression resolveIdentifier left)
                (evaluateIntExpression resolveIdentifier right)

        IntOperator left Subtract right ->
            Result.map2 (-)
                (evaluateIntExpression resolveIdentifier left)
                (evaluateIntExpression resolveIdentifier right)

        IntOperator left Multiply right ->
            Result.map2 (*)
                (evaluateIntExpression resolveIdentifier left)
                (evaluateIntExpression resolveIdentifier right)

        IntOperator left Divide right ->
            Result.map2 (//)
                (evaluateIntExpression resolveIdentifier left)
                (evaluateIntExpression resolveIdentifier right)

        Math0 f ->
            case f of
                Pi ->
                    pi |> round |> Ok

                E ->
                    e |> round |> Ok

        Time0 f ->
            case f of
                SecondsSinceUnixEpoch ->
                    0 |> Ok


evaluateAsInt : (String -> Maybe Int) -> Expression -> Result EvaluateError Int
evaluateAsInt resolveIdentifier expression =
    case expression of
        Int intExpression ->
            evaluateIntExpression resolveIdentifier intExpression

        _ ->
            Err <| CannotEvaluateExpression expression


evaluateAsJson : (String -> Maybe Int) -> Expression -> Result EvaluateError JsonValue
evaluateAsJson resolveIdentifier expression =
    case expression of
        Int intExpression ->
            evaluateIntExpression resolveIdentifier intExpression
                |> Result.map (toFloat >> Json.Value.NumericValue)

        Procedure procedure ->
            toRpcJson procedure
                |> Ok

        _ ->
            Err <| CannotEvaluateExpression expression
