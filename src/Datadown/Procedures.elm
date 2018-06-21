module Datadown.Procedures
    exposing
        ( Procedure(..)
        , toRpcJson
        )

{-| Procedures


## Types

@docs ProcedureMethod

-}

import JsonValue exposing (..)


type Procedure
    = HttpGetJson String


rpcJson : String -> Maybe JsonValue -> String -> JsonValue
rpcJson method maybeParams id =
    [ Just ( "jsonrpc", StringValue "2.0" )
    , Just ( "method", StringValue method )
    , Maybe.map ((,) "params") maybeParams
    , Just ( "id", StringValue id )
    ]
        |> List.filterMap identity
        |> ObjectValue


toRpcJson : Procedure -> JsonValue
toRpcJson procedure =
    case procedure of
        HttpGetJson urlString ->
            rpcJson "HTTP"
                (Just <|
                    ObjectValue
                        [ ( "method", StringValue "GET" )
                        , ( "acceptMimeType", StringValue "application/json" )
                        , ( "url", StringValue urlString )
                        ]
                )
                ("HTTP GET application/json " ++ urlString)
