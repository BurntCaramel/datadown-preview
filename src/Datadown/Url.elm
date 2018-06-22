module Datadown.Url
    exposing
        ( Url(..)
        , MathFunction(..)
        , schemeAndStringToUrl
        , urlToString
        )


type MathFunction
    = Pi
    | E


type Url
    = Https String
    | Mailto String
    | Tel String
    | Math MathFunction
    | Time String
      -- | Data String String
    | Other String String


schemeAndStringToUrl : String -> String -> Maybe Url
schemeAndStringToUrl scheme string =
    case scheme of
        "https" ->
            Https string
                |> Just

        "mailto" ->
            Mailto string
                |> Just

        "tel" ->
            Tel string
                |> Just

        "math" ->
            case string of
                "pi" ->
                    Just <| Math Pi

                "e" ->
                    Just <| Math E

                _ ->
                    Nothing

        "time" ->
            Time string
                |> Just

        _ ->
            Other scheme string
                |> Just


urlToString : Url -> String
urlToString url =
    case url of
        Https string ->
            "https:" ++ string

        Mailto email ->
            "mailto:" ++ email

        Tel phone ->
            "tel:" ++ phone

        Math f ->
            case f of
                Pi ->
                    "math:pi"

                E ->
                    "math:e"

        Time string ->
            "time:" ++ string

        Other scheme string ->
            scheme ++ ":" ++ string
