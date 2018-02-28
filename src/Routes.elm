module Routes
    exposing
        ( Route(..)
        , CollectionSource(..)
        , EditMode(..)
        , parseLocation
        , toPath
        )

import Dict exposing (Dict)
import Navigation exposing (Location)
import UrlParser exposing (..)


type EditMode
    = Off
    | WithPreview
    | Only


editModeDict : Dict String EditMode
editModeDict =
    Dict.fromList
        [ ( "0", Off )
        , ( "1", WithPreview )
        , ( "2", Only )
        ]


editModeFromString : Maybe String -> EditMode
editModeFromString maybeString =
    maybeString
        |> Maybe.andThen (\s -> Dict.get s editModeDict)
        |> Maybe.withDefault WithPreview


type CollectionSource
    = Example
    | GitHubRepo String String String


type Route
    = Landing
    | Collection CollectionSource
    | CollectionItem CollectionSource String EditMode
    | NotFound String


intFrom1 : UrlParser.Parser (Int -> c) c
intFrom1 =
    UrlParser.map (\i -> i - 1) int


locationParser : UrlParser.Parser (Route -> a) a
locationParser =
    oneOf
        [ map Landing top
        , map Collection <|
            map Example <|
                (s "example")
        , map CollectionItem <|
            (map Example <| s "example")
                </> string
                <?> customParam "editMode" editModeFromString
        , map Collection <|
            map GitHubRepo <|
                (s "github" </> string </> string </> string)
        , map CollectionItem <|
            (map GitHubRepo <|
                s "github"
                    </> string
                    -- owner
                    </> string
                    -- repo
                    </> string
             -- ref
            )
                </> string
                <?> customParam "editMode" editModeFromString
        ]


toPath : Route -> String
toPath route =
    case route of
        Landing ->
            "/"

        Collection collection ->
            case collection of
                Example ->
                    "/example"

                GitHubRepo owner repo ref ->
                    "/github/" ++ owner ++ "/" ++ repo ++ "/" ++ ref

        CollectionItem collection key editMode ->
            case collection of
                Example ->
                    "/example/" ++ key

                GitHubRepo owner repo ref ->
                    "/github/" ++ owner ++ "/" ++ repo ++ "/" ++ ref ++ "/" ++ key

        NotFound path ->
            path


parseLocation : Location -> Route
parseLocation location =
    parsePath locationParser location
        |> Maybe.withDefault (NotFound location.pathname)
