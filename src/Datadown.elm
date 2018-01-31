module Datadown
    exposing
        ( Document
        , Section(..)
        , Content(..)
        )

{-| A library for Datadown parsing


# Types

@docs Document, Section, Content

-}

import Dict exposing (Dict)
import JsonValue exposing (JsonValue)


{-| Content, such as plain text, code, lists, etc
-}
type Content a
    = Text String
    | Code (Maybe String) String -- ```html
    | Expressions a -- ```
    | List (List (Content a)) -- -
    | Quote (Document a) -- >
    | Json JsonValue


{-| A section of data
-}
type Section a =
    Section
        { title : String
        , mainContent : List (Content a)
        , subsections : List (Section a)
        , inlineExpressions : Dict String a
        }


{-| A full document, with many sections
-}
type alias Document a =
    { title : String
    , introContent : List (Content a)
    , introInlineExpressions : Dict String a
    , sections : List (Section a)
    }
