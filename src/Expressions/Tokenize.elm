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
        [ map (\_ -> Add) (symbol "+")
        , map (\_ -> Subtract) (symbol "-")
        , map (\_ -> Exponentiate) (symbol "**")
        , map (\_ -> Multiply) (symbol "*")
        , map (\_ -> Divide) (symbol "/")
        , map (\_ -> EqualTo) (symbol "==")
        , map (\_ -> LessThan True) (symbol "<=")
        , map (\_ -> LessThan False) (symbol "<")
        , map (\_ -> GreaterThan True) (symbol ">=")
        , map (\_ -> GreaterThan False) (symbol ">")
        , map (\_ -> HttpModule GetJson) (keyword "HTTP.get_json")
        ]


value : Parser JsonValue
value =
    oneOf
        [ succeed (negate >> NumericValue)
            |. symbol "-"
            |= float
        , map NumericValue float
        , map (\_ -> BoolValue True) (keyword "true")
        , map (\_ -> BoolValue False) (keyword "false")
        ]


magicNumbers : Parser JsonValue
magicNumbers =
    oneOf
        [ map (\_ -> e) (keyword "Math.e")
        , map (\_ -> pi) (keyword "Math.pi")
        ]
        |> map NumericValue
        |> backtrackable


token : Parser Token
token =
    oneOf
        [ map Value magicNumbers
        , url
        , identifier
        , map Value value
        , map Operator operator
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
    Parser.token "\n"
        |> andThen (\_ -> optionalNewlines)


optionalNewlines : Parser ()
optionalNewlines =
    chompWhile isNewline


nextToken : Parser Token
nextToken =
    succeed identity
        |. optionalSpaces
        |= token
        |. optionalSpaces


tokensHelp : List Token -> Parser (List Token)
tokensHelp revTokens =
    oneOf
        [ nextToken
            |> andThen (\t -> tokensHelp (t :: revTokens))
        , lazy <|
            \_ -> succeed (List.reverse revTokens)
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
        |. spaces
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
    let
        linesHelp2 revLines =
            succeed identity
                |. backtrackable optionalSpaces
                |= oneOf
                    [ succeed (Loop revLines)
                        |. Parser.token "\n"
                    , map (\l -> Loop (l :: revLines)) <|
                        nextLine
                        -- |. spaces
                        |. chompUntilEndOr "\n"
                    -- [ succeed (\l -> Loop (l :: revLines))
                    --     |= nextLine
                    --     |. spaces
                    --     -- |. chompUntilEndOr "\n"
                    , succeed ()
                        |> map (\_ -> Done (List.reverse revLines))
                    ]
    in
        loop [] linesHelp2


tokenize : String -> Result (List DeadEnd) (List (List Token))
tokenize input =
    case run lines input of
        Err deadEnds ->
            Err ({ col = -1, row = -1, problem = Problem input } :: deadEnds)

        Ok tokenLines ->
            Ok tokenLines
