module Datadown.Url exposing (Url(..), schemeAndStringToUrl, urlToString)


type Url
    = Https String
    | Mailto String
    | Tel String
    | Math String
    | Time String
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
            Math string

        "time" ->
            Time string

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

        Math string ->
            "math:" ++ string

        Time string ->
            "time:" ++ string

        Other scheme string ->
            scheme ++ ":" ++ string
