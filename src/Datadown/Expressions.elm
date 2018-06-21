module Datadown.Expressions
    exposing
        ( Operator(..)
        , Token(..)
        , IntExpression(..)
        , BoolExpression(..)
        , Expression(..)
        , ParseError(..)
        , EvaluateError(..)
        , tokenize
        , parseExpression
        , evaluateAsInt
        )

import Char
import Set exposing (Set)
import Parser exposing (..)
import JsonValue
import Datadown.Url exposing (Url(..), schemeAndStringToUrl)
import Datadown.Procedures exposing (Procedure(..))
import Datadown.Rpc


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
    ignore zeroOrMore isSpace


isIdentifierHeadChar : Char -> Bool
isIdentifierHeadChar c =
    c == '$'


isIdentifierBodyChar : Char -> Bool
isIdentifierBodyChar c =
    Char.isLower c
        || Char.isUpper c
        || Char.isDigit c
        || c
        == '_'


identifier : Parser Token
identifier =
    succeed Identifier
        |. symbol "$"
        |= keep oneOrMore isIdentifierBodyChar


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
    [ ' ', '\n', '\x0D', '\t' ]
        |> Set.fromList


isNonWhitespace : Char -> Bool
isNonWhitespace c =
    Set.member c whitespaceChars
        |> not


url : Parser Token
url =
    delayedCommitMap schemeAndStringToUrl
        (keep oneOrMore Char.isLower |. symbol ":")
        (keep oneOrMore isNonWhitespace)
        |> map Url


token : Parser Token
token =
    inContext "token" <|
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
    delayedCommit optionalSpaces <|
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
    inContext "tokens" <|
        succeed identity
            |. optionalSpaces
            |= andThen (\t -> tokensHelp [ t ]) token
            |. optionalSpaces


tokenize : String -> Result Parser.Error (List Token)
tokenize input =
    run tokens input


type IntExpression
    = ReadInt String
    | UseInt Int
    | IntOperator IntExpression Operator IntExpression


type BoolExpression
    = UseBool Bool


type Expression
    = Empty
    | Int IntExpression
    | Bool BoolExpression
    | Procedure Procedure


type ParseError
    = CannotBeEmpty
    | Tokenization Parser.Error
    | OperatorMissingLeft Operator
    | OperatorMissingRight Operator
    | OperatorMustHaveNumericLeft Expression Operator
    | OperatorMustHaveNumericRight Expression Operator Expression
    | UnsupportedUrl Url
    | Invalid Expression (List Token)


parseNext : Expression -> List Token -> Result ParseError Expression
parseNext right tokens =
    case ( right, tokens ) of
        ( Empty, [] ) ->
            Err CannotBeEmpty

        ( right, [] ) ->
            Ok right

        ( Empty, (Identifier id) :: rest ) ->
            parseNext (ReadInt id |> Int) rest

        ( Empty, (IntLiteral i) :: rest ) ->
            parseNext (UseInt i |> Int) rest

        ( Empty, (BoolLiteral b) :: rest ) ->
            parseNext (UseBool b |> Bool) rest

        ( Empty, (Url url) :: rest ) ->
            case url of
                Https sansScheme ->
                    let
                        urlString =
                            "https:" ++ sansScheme
                    in
                        HttpGetJson urlString
                            |> Procedure
                            |> Ok

                url ->
                    Err <| UnsupportedUrl url

        ( Empty, (Operator op) :: rest ) ->
            Err <| OperatorMissingRight op

        ( first, (Operator op) :: [] ) ->
            Err <| OperatorMissingLeft op

        ( Int first, (Operator op) :: rest ) ->
            case parseNext Empty rest of
                Ok (Int (IntOperator third nextOp second)) ->
                    if (precendenceOfOperator nextOp) >= (precendenceOfOperator op) then
                        Ok <| Int <| IntOperator (IntOperator third nextOp second) op first
                    else
                        Ok <| Int <| IntOperator third nextOp (IntOperator second op first)

                Ok (Int second) ->
                    Ok <| Int <| IntOperator second op first

                Ok second ->
                    Err <| OperatorMustHaveNumericRight (Int first) op second

                Err error ->
                    Err error

        ( right, (Operator op) :: rest ) ->
            Err <| OperatorMustHaveNumericLeft right op

        ( right, tokens ) ->
            Err <| Invalid right tokens


parseTokens : List Token -> Result ParseError Expression
parseTokens tokens =
    tokens
        |> List.reverse
        |> parseNext Empty


parseExpression : String -> Result ParseError Expression
parseExpression input =
    input
        |> tokenize
        |> Result.mapError Tokenization
        |> Result.andThen parseTokens


type EvaluateError
    = MustBeIntExpression Expression
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


evaluateAsInt : (String -> Maybe Int) -> Expression -> Result EvaluateError Int
evaluateAsInt resolveIdentifier expression =
    case expression of
        Int expression ->
            evaluateIntExpression resolveIdentifier expression

        _ ->
            Err <| MustBeIntExpression expression
