module Datadown exposing
    ( Document, Section(..), Content(..)
    , ListItemQualifier(..), sectionHasTitle
    )

{-| A library for Datadown parsing


## Types

@docs Document, Section, Content

-}

import Dict exposing (Dict)
import Json.Value exposing (JsonValue)


type ListItemQualifier a
    = Always
    | Flag Bool
    | Expression a


{-| Content, such as plain text, code, lists, etc
-}
type Content a
    = Text String
    | Code (Maybe String) String -- ```html …
    | Expressions a -- ``` …
    | List (List ( Content a, ListItemQualifier a )) -- | - … | - [x] … | - [ ] …
    | Quote (Document a) -- > …
    | Json JsonValue
    | Reference String (List String) JsonValue


{-| A section of data
-}
type Section a
    = Section
        { title : String
        , mainContent : List (Content a)
        , subsections : List (Section a)
        , inlineExpressions : Dict String a
        , urls : List String
        }


sectionHasTitle : String -> Section a -> Bool
sectionHasTitle title sectionWrapper =
    case sectionWrapper of
        Section section ->
            section.title == title


{-| A full document, with many sections
-}
type alias Document a =
    { title : String
    , introContent : List (Content a)
    , introInlineExpressions : Dict String a
    , sections : List (Section a)
    }
