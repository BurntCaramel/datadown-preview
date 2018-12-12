module Datadown.Rpc exposing
    ( Rpc, Id, Response, Error
    , errorToJsonValue, fromJsonValue, graphQL, toCommand
    )

{-| Rpc


## Types

@docs Rpc, Id, Response, Error

-}

import Dict
import Http
import Json.Encode
import Json.Value exposing (JsonValue)
import Platform.Cmd exposing (Cmd)


{-| An identifier unique per Rpc
-}
type alias Id =
    String


{-| A command, modelled after the JSON remote procedure call.
See: <http://www.jsonrpc.org/specification>
-}
type alias Rpc a =
    { method : a
    , params : Maybe JsonValue
    , id : Id
    }


{-| An error, modelled after those at: <http://www.jsonrpc.org/specification>
-}
type alias Error =
    { code : Int
    , message : String
    , data : Maybe JsonValue
    }


{-| A response for a given Rpc
-}
type alias Response =
    { id : Id
    , result : Result Error JsonValue
    }


{-| Convert a JsonValue into a Rpc.
Requires fields "method" and "id", optionally "params".
-}
fromJsonValue : JsonValue -> Maybe (Rpc String)
fromJsonValue json =
    case json of
        Json.Value.ObjectValue pairs ->
            let
                dict =
                    Dict.fromList pairs

                hasVersion =
                    Dict.member "jsonrpc" dict

                maybeMethod =
                    case Dict.get "method" dict of
                        Just (Json.Value.StringValue value) ->
                            Just value

                        _ ->
                            Nothing

                maybeParams =
                    Dict.get "params" dict

                maybeId =
                    case Dict.get "id" dict of
                        Just (Json.Value.StringValue value) ->
                            Just value

                        Just (Json.Value.NumericValue value) ->
                            Just <| String.fromFloat value

                        _ ->
                            Nothing
            in
            case ( hasVersion, maybeMethod, maybeId ) of
                ( True, Just method, Just id ) ->
                    Just <| Rpc method maybeParams id

                _ ->
                    Nothing

        _ ->
            Nothing


graphQL : String -> Rpc String
graphQL queryString =
    let
        params =
            [ ( "query", Json.Value.StringValue queryString )
            ]
                |> Json.Value.ObjectValue
    in
    Rpc "graphql" (Just params) queryString


getParam : List String -> Rpc String -> Maybe JsonValue
getParam path rpc =
    Maybe.andThen (Json.Value.getIn path >> Result.toMaybe) rpc.params


requireString : JsonValue -> Maybe String
requireString json =
    case json of
        Json.Value.StringValue value ->
            Just value

        _ ->
            Nothing


convertError : Http.Error -> Error
convertError httpError =
    case httpError of
        Http.BadUrl url ->
            Error 0 ("Bad URL: " ++ url) Nothing

        Http.Timeout ->
            Error 0 "Timed out" Nothing

        Http.NetworkError ->
            Error 0 "Unable to connect" Nothing

        Http.BadStatus r ->
            Error r.status.code r.status.message (Just <| Json.Value.StringValue r.body)

        Http.BadPayload message r ->
            Error r.status.code r.status.message (Just <| Json.Value.StringValue r.body)


convertResult : Rpc String -> Result Http.Error JsonValue -> Response
convertResult rpc httpResult =
    let
        result =
            httpResult
                |> Result.mapError convertError
    in
    Response rpc.id result


toCommand : (Response -> msg) -> Rpc String -> Maybe (Cmd msg)
toCommand toMessage rpc =
    case rpc.method of
        "HTTP" ->
            let
                maybeUrl =
                    getParam [ "url" ] rpc
                        |> Maybe.andThen requireString
            in
            case maybeUrl of
                Just url ->
                    Http.get url Json.Value.decoder
                        |> Http.send (convertResult rpc >> toMessage)
                        |> Just

                _ ->
                    Nothing

        "graphql" ->
            let
                maybeQuery =
                    getParam [ "query" ] rpc

                url =
                    case getParam [ "url" ] rpc of
                        Just (Json.Value.StringValue s) ->
                            s

                        _ ->
                            "https://1.source.collected.design/graphql"
            in
            case maybeQuery of
                Just (Json.Value.StringValue queryString) ->
                    let
                        body =
                            [ ( "query", Json.Encode.string queryString ) ]
                                |> Json.Encode.object
                                |> Json.Encode.encode 0
                                |> Http.stringBody "application/json"
                    in
                    Http.post url body Json.Value.decoder
                        |> Http.send (convertResult rpc >> toMessage)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


errorToJsonValue : Error -> JsonValue
errorToJsonValue error =
    [ Just ( "code", Json.Value.NumericValue <| toFloat error.code )
    , Just ( "message", Json.Value.StringValue error.message )
    , Maybe.map (\b -> ( "data", b )) error.data
    ]
        |> List.filterMap identity
        |> Json.Value.ObjectValue
