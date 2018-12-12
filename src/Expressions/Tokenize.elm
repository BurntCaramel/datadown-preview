module Expressions.Tokenize exposing
    ( Token(..), Operator(..)
    , tokenize
    , HttpFunction(..), MathFunction(..), Url(..), identifier, lines, operator, token, tokens, urlToString
    )

{-| Tokenize


# Types

@docs Token, Value, Operator


# Functions

@docs tokenize

-}

import Char
import Json.Value exposing (JsonValue(..))
import Parser exposing (..)
import Set exposing (Set)


type MathFunction
    = Sine
    | Cosine
    | Tangent
    | Turns


type HttpFunction
    = GetJson


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate
    | EqualTo
    | LessThan Bool
    | GreaterThan Bool
    | MathModule MathFunction
    | HttpModule HttpFunction


type Url
    = Https String
    | Mailto String
    | Tel String
    | Math String
    | Time String
    | Other String String


type Token
    = Identifier String
    | Value JsonValue
    | Operator Operator
    | Url Url



-- type Type
--     = Float
--     | String
--     | Bool
--     | Unknown
-- type Expression t
--     = Get String t
--     | Use JsonValue
--     | Comparison Operator (Expression Unknown) (Expression Unknown)
--     | Math Float Operator (List (Expression Float))
--     | MathFunction String (Expression Float)
--     | If (Expression Bool) (Expression t) (Expression t)


isIdentifierHeadChar : Char -> Bool
isIdentifierHeadChar c =
    Char.isLower c
        || c
        == '.'
        || c
        == '_'


isIdentifierTailChar : Char -> Bool
isIdentifierTailChar c =
    Char.isLower c
        || Char.isUpper c
        || Char.isDigit c
        || c
        == '.'
        || c
        == '_'


notIdentifiers : Set String
notIdentifiers =
    [ "true", "false" ]
        |> Set.fromList


identifier : Parser Token
identifier =
    variable
        { start = isIdentifierHeadChar
        , inner = isIdentifierTailChar
        , reserved = notIdentifiers
        }
        |> map Identifier


schemeAndStringToUrl : String -> String -> Url
schemeAndStringToUrl scheme string =
    case scheme of
        "https" ->
            Https string

        "mailto" ->
            Mailto string

        "tel" ->
            Tel string

        "math" ->
            Math string

        "time" ->
            Time string

        _ ->
            Other scheme string


urlToString : Url -> String
urlToString aURL =
    case aURL of
        Https string ->
            "https:" ++ string

        Mailto email ->
            "mailto:" ++ email

        Tel phone ->
            "tel:" ++ phone

        Math string ->
            "math:" ++ string

        Time string ->
            "time:" ++ string

        Other scheme string ->
            scheme ++ ":" ++ string


whitespaceChars : Set Char
whitespaceChars =
    [ ' ', '\n', '\u{000D}', '\t' ]
        |> Set.fromList


notWhitespace : Char -> Bool
notWhitespace c =   
    Set.member c whitespaceChars |> not


url : Parser Token
url =
    backtrackable <|
    map Url <|
    succeed schemeAndStringToUrl
        |= (chompWhile Char.isLower |> getChompedString)
        |. symbol ":"
        |= (chompWhile notWhitespace |> getChompedString)


operator : Parser Operator
operator =
    oneOf
        [ succeed Add
            |. symbol "+"
        , succeed Subtract
            |. symbol "-"
        , succeed Exponentiate
            |. symbol "**"
        , succeed Multiply
            |. symbol "*"
        , succeed Divide
            |. symbol "/"
        , succeed EqualTo
            |. symbol "=="
        , succeed (LessThan True)
            |. symbol "<="
        , succeed (LessThan False)
            |. symbol "<"
        , succeed (GreaterThan True)
            |. symbol ">="
        , succeed (GreaterThan False)
            |. symbol ">"
        , succeed (HttpModule GetJson)
            |. keyword "HTTP.get_json"
        ]


value : Parser JsonValue
value =
    oneOf
        [ succeed (negate >> NumericValue)
            |. symbol "-"
            |= float
        , succeed NumericValue
            |= float
        , succeed (BoolValue True)
            |. keyword "true"
        , succeed (BoolValue False)
            |. keyword "false"
        ]


magicNumbers : Parser JsonValue
magicNumbers =
    oneOf
        [ succeed e
            |. keyword "Math.e"
        , succeed pi
            |. keyword "Math.pi"
        ]
        |> map NumericValue


token : Parser Token
token =
    oneOf
        [ succeed Value
            |= magicNumbers
        , url
        , identifier
        , succeed Value
            |= value
        , succeed Operator
            |= operator
        ]


isSpace : Char -> Bool
isSpace c =
    c == ' '


isNewline : Char -> Bool
isNewline c =
    c == '\n'


optionalSpaces : Parser ()
optionalSpaces =
    chompWhile isSpace


newlines : Parser ()
newlines =
    succeed ()
     |. Parser.token "\n"
     |. optionalNewlines


optionalNewlines : Parser ()
optionalNewlines =
    chompWhile isNewline


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


nextLine : Parser (List Token)
nextLine =
    succeed identity
        |. newlines
        |= tokens


linesHelp : List (List Token) -> Parser (List (List Token))
linesHelp revLines =
    oneOf
        [ nextLine
            |> andThen (\l -> linesHelp (l :: revLines))
        , lazy <|
            \_ -> succeed (List.reverse revLines)
        ]


lines : Parser (List (List Token))
lines =
    succeed identity
        |. optionalNewlines
        |= andThen (\l -> linesHelp [ l ]) tokens
        |. optionalNewlines
        |. end


tokenize : String -> Result (List DeadEnd) (List (List Token))
tokenize input =
    run lines input
