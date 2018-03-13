module Services.CollectedSource
    exposing
        ( ContentInfo
        , listDocuments
        )

import Json.Decode exposing (..)
import Http
import Task exposing (Task)


type alias ContentInfo =
    { path : String
    , content : Maybe String
    }


contentInfoDecoder : Decoder ContentInfo
contentInfoDecoder =
    map2 ContentInfo
        (field "path" string)
        (maybe <| field "content" string)


listDocuments : String -> String -> String -> Task Http.Error (List ContentInfo)
listDocuments owner repo branch =
    let
        url =
            [ "https://1.source.collected.design/github"
            , owner
            , repo
            , branch
            , "command:list?content"
            ]
                |> String.join "/"
    in
        Http.get url (list contentInfoDecoder)
            |> Http.toTask
