module Routes exposing
    ( CollectionSource(..)
    , CollectionSourceId
    , EditMode(..)
    , Route(..)
    , collectionSourceFor
    , collectionSourceToId
    , parseUrl
    , toPath
    )

import Dict exposing (Dict)
import Url exposing (Url)


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
    = Tour
    | GitHubRepo String String String


type Route
    = Landing
    | Collection CollectionSource
    | CollectionItem CollectionSource String EditMode
    | CollectionContentSources CollectionSource
    | NotFound String


fromPath : String -> Route
fromPath path =
    let
        segments =
            path
                |> String.split "/"
                |> List.filter ((/=) "")
    in
    case segments of
        [] ->
            Landing

        "tour" :: rest ->
            case rest of
                [] ->
                    Collection Tour

                [ "content" ] ->
                    CollectionContentSources Tour

                items ->
                    CollectionItem Tour (String.join "/" items) WithPreview

        "github" :: owner :: repo :: ref :: rest ->
            let
                collection =
                    GitHubRepo owner repo ref
            in
            case rest of
                [] ->
                    Collection collection

                [ "content" ] ->
                    CollectionContentSources collection

                items ->
                    CollectionItem collection (String.join "/" items) WithPreview

        components ->
            NotFound (String.join "/" components)


toPath : Route -> String
toPath route =
    case route of
        Landing ->
            "/"

        Collection collection ->
            case collection of
                Tour ->
                    "/tour"

                GitHubRepo owner repo ref ->
                    "/github/" ++ owner ++ "/" ++ repo ++ "/" ++ ref

        CollectionItem collection key editMode ->
            case collection of
                Tour ->
                    "/tour/" ++ key

                GitHubRepo owner repo ref ->
                    "/github/" ++ owner ++ "/" ++ repo ++ "/" ++ ref ++ "/" ++ key

        CollectionContentSources collection ->
            case collection of
                Tour ->
                    "/tour/content"

                GitHubRepo owner repo ref ->
                    "/github/" ++ owner ++ "/" ++ repo ++ "/" ++ ref ++ "/" ++ "content"

        NotFound path ->
            path


collectionSourceFor : Route -> Maybe CollectionSource
collectionSourceFor route =
    case route of
        Collection collection ->
            Just collection

        CollectionItem collection _ _ ->
            Just collection

        CollectionContentSources collection ->
            Just collection

        _ ->
            Nothing


type alias CollectionSourceId =
    String


collectionSourceToId : CollectionSource -> CollectionSourceId
collectionSourceToId collectionSource =
    Collection collectionSource
        |> toPath


parseUrl : Url -> Route
parseUrl url =
    fromPath url.path
