module Routes
    exposing
        ( Route(..)
        , CollectionSource(..)
        , EditMode(..)
        , parseLocation
        , toPath
        , collectionSourceFor
        , CollectionSourceId
        , collectionSourceToId
        )

import Dict exposing (Dict)
import Navigation exposing (Location)


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

            "example" :: rest ->
                case rest of
                    [] ->
                        Collection Example

                    [ "content" ] ->
                        CollectionContentSources Example

                    items ->
                        CollectionItem Example (String.join "/" items) WithPreview

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

        CollectionContentSources collection ->
            case collection of
                Example ->
                    "/example/content"

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


parseLocation : Location -> Route
parseLocation location =
    fromPath location.pathname
