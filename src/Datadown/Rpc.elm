module Datadown.Rpc
    exposing
        ( Rpc
        , Id
        , Response
        , Error
        , fromJsonValue
        , toCommand
        , errorToJsonValue
        )

{-| Commands


## Types

@docs Rpc, Id, Response, Error

-}

import Dict
import JsonValue exposing (JsonValue)
import Http
import Platform.Cmd exposing (Cmd)


{-| An identifier unique per Rpc
-}
type alias Id =
    String


{-| A command, modelled after the JSON remote procedure call.
See: <http://www.jsonrpc.org/specification>
-}
type alias Rpc =
    { method : String
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
fromJsonValue : JsonValue -> Maybe Rpc
fromJsonValue json =
    case json of
        JsonValue.ObjectValue pairs ->
            let
                dict =
                    Dict.fromList pairs
                
                hasVersion =
                    Dict.member "jsonrpc" dict

                maybeMethod =
                    case Dict.get "method" dict of
                        Just (JsonValue.StringValue value) ->
                            Just value

                        _ ->
                            Nothing

                maybeParams =
                    Dict.get "params" dict

                maybeId =
                    case Dict.get "id" dict of
                        Just (JsonValue.StringValue value) ->
                            Just value

                        Just (JsonValue.NumericValue value) ->
                            Just <| toString value

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


getParam : List String -> Rpc -> Maybe JsonValue
getParam path rpc =
    Maybe.andThen (JsonValue.getIn path >> Result.toMaybe) rpc.params


requireString : JsonValue -> Maybe String
requireString json =
    case json of
        JsonValue.StringValue value ->
            Just value

        _ ->
            Nothing


toCommand : (Response -> msg) -> Rpc -> Maybe (Cmd msg)
toCommand toMessage rpc =
    case rpc.method of
        "HTTP" ->
            let
                convertError : Http.Error -> Error
                convertError httpError =
                    case httpError of
                        Http.BadUrl url ->
                            Error 0 url Nothing
                        
                        Http.Timeout ->
                            Error 0 "Timed out" Nothing

                        Http.NetworkError ->
                            Error 0 (toString httpError) Nothing
                        
                        Http.BadStatus r ->
                            Error r.status.code r.status.message (Just <| JsonValue.StringValue r.body)
                        
                        Http.BadPayload message r ->
                            Error r.status.code r.status.message (Just <| JsonValue.StringValue r.body)

                convertResult : Result Http.Error JsonValue -> Response
                convertResult httpResult =
                    let
                        result =
                            httpResult
                                |> Result.mapError convertError
                    in
                        Response rpc.id result

                maybeUrl =
                    getParam [ "url" ] rpc
                        |> Maybe.andThen requireString
            in
                case maybeUrl of
                    Just url ->
                        Http.get url JsonValue.decoder
                            |> Http.send (convertResult >> toMessage)
                            |> Just

                    _ ->
                        Nothing

        _ ->
            Nothing


errorToJsonValue : Error -> JsonValue
errorToJsonValue error =
    [ Just ("code", JsonValue.NumericValue <| toFloat error.code)
    , Just ("message", JsonValue.StringValue error.message)
    , Maybe.map ((,) "data") error.data
    ]
        |> List.filterMap identity
        |> JsonValue.ObjectValue
