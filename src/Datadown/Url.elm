module Datadown.Url exposing
    ( MathFunction(..)
    , TimeFunction(..)
    , Url(..)
    , schemeAndStringToUrl
    , urlToString
    )


type MathFunction
    = Pi
    | E


type TimeFunction
    = SecondsSinceUnixEpoch


type Url
    = Https String
    | Mailto String
    | Tel String
    | Math (Result String MathFunction)
    | Time (Result String TimeFunction)
      -- | Data String String
    | Other String String


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
            case string of
                "pi" ->
                    Pi |> Ok |> Math

                "e" ->
                    E |> Ok |> Math

                s ->
                    Err s |> Math

        "time" ->
            case string of
                "seconds" ->
                    SecondsSinceUnixEpoch |> Ok |> Time

                s ->
                    Err s |> Time

        _ ->
            Other scheme string


urlToString : Url -> String
urlToString url =
    case url of
        Https string ->
            "https:" ++ string

        Mailto email ->
            "mailto:" ++ email

        Tel phone ->
            "tel:" ++ phone

        Math either ->
            case either of
                Ok Pi ->
                    "math:pi"

                Ok E ->
                    "math:e"

                Err s ->
                    "math:" ++ s

        Time either ->
            case either of
                Ok SecondsSinceUnixEpoch ->
                    "time:seconds"

                Err s ->
                    "math:" ++ s

        Other scheme string ->
            scheme ++ ":" ++ string
