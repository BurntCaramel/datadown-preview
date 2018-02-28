module Services.GitHub
    exposing
        ( ContentInfo
        , listDocuments
        )

import Json.Decode exposing (..)
import Http
import Task exposing (Task)
import Base64
import UrlBase64


type alias SearchResultItem =
    { name : String
    , path : String
    , url : String
    , git_url : String
    , score : Float
    }


type alias SearchResults =
    { total_count : Int
    , incomplete_results : Bool
    , items : List SearchResultItem
    }


type alias ContentInfo =
    { name : String
    , path : String
    , size : Int
    , content : Maybe String
    }


searchResultItemDecoder : Decoder SearchResultItem
searchResultItemDecoder =
    map5 SearchResultItem
        (field "name" string)
        (field "path" string)
        (field "url" string)
        (field "git_url" string)
        (field "score" float)


searchResultsDecoder : Decoder SearchResults
searchResultsDecoder =
    map3 SearchResults
        (field "total_count" int)
        (field "incomplete_results" bool)
        (field "items" <| list searchResultItemDecoder)


contentFieldsDecoder : Decoder (Maybe String)
contentFieldsDecoder =
    let
        fromFields content encoding =
            case encoding of
                "base64" ->
                    content
                        |> String.split "\n"
                        |> String.join ""
                        |> UrlBase64.decode Base64.decode
                        |> Result.toMaybe

                _ ->
                    Nothing
    in
        map2 fromFields
            (field "content" string)
            (field "encoding" string)


contentInfoDecoder : Decoder ContentInfo
contentInfoDecoder =
    map4 ContentInfo
        (field "name" string)
        (field "path" string)
        (field "size" int)
        contentFieldsDecoder


searchCode : List String -> Http.Request SearchResults
searchCode params =
    let
        q =
            params
                |> List.map Http.encodeUri
                |> String.join " "
    in
        Http.get ("https://api.github.com/search/code?q=" ++ q) <|
            searchResultsDecoder


loadContent : String -> Task Http.Error ContentInfo
loadContent url =
    Http.get url contentInfoDecoder
        |> Http.toTask


listDocuments : String -> String -> Task Http.Error (List ContentInfo)
listDocuments owner repo =
    let
        resultsToDownloadTasks results =
            results.items
                |> List.map (.url >> loadContent)
                |> Task.sequence
    in
        searchCode
            [ "components"
            , "in:path"
            , "language:md"
            , "repo:" ++ owner ++ "/" ++ repo
            ]
            |> Http.toTask
            |> Task.andThen resultsToDownloadTasks
