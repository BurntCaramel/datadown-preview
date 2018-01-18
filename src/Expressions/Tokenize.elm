module Expressions.Tokenize
    exposing
        ( identifier
        , operator
        , token
        , tokens
        , lines
        , tokenize
        , Token(..)
        , Value(..)
        , Operator(..)
        )

{-| Tokenize


# Types

@docs Token, Value, Operator


# Functions

@docs tokenize

-}

import Parser exposing (..)
import Parser.LanguageKit exposing (variable)
import Char
import Set


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate


type Value
    = Float Float


type Token
    = Identifier String
    | Value Value
    | Operator Operator


type alias Piped =
    { initial : Value
    , steps : List (Operator Value)
    }


isIdentifierTailChar : Char -> Bool
isIdentifierTailChar c =
    Char.isLower c
        || Char.isUpper c
        || Char.isDigit c


identifier : Parser Token
identifier =
    variable Char.isLower isIdentifierTailChar Set.empty
        |> map Identifier


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
        ]


value : Parser Value
value =
    oneOf
        [ float
        , delayedCommit (symbol "-") <|
            succeed negate
                |= float
        ]
        |> map Float


magicNumbers : Parser Value
magicNumbers =
    oneOf
        [ succeed e
            |. keyword "Math.e"
        , succeed pi
            |. keyword "Math.pi"
        ]
        |> map Float


token : Parser Token
token =
    inContext "token" <|
        oneOf
            [ identifier
            , succeed Value
                |= magicNumbers
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


spaces : Parser ()
spaces =
    ignore oneOrMore isSpace


optionalSpaces : Parser ()
optionalSpaces =
    ignore zeroOrMore isSpace


newlines : Parser ()
newlines =
    ignore oneOrMore isNewline


optionalNewlines : Parser ()
optionalNewlines =
    ignore zeroOrMore isNewline


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


nextLine : Parser (List Token)
nextLine =
    delayedCommit newlines <|
        succeed identity
            |= tokens


linesHelp : List (List Token) -> Parser (List (List Token))
linesHelp revLines =
    oneOf
        [ nextLine
            |> andThen (\l -> linesHelp (l :: revLines))
        , succeed (List.reverse revLines)
        ]


lines : Parser (List (List Token))
lines =
    inContext "lines" <|
        succeed identity
            |. optionalNewlines
            |= andThen (\l -> linesHelp [ l ]) tokens


tokenize : String -> Result Error (List (List Token))
tokenize input =
    run lines input
