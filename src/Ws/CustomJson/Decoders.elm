module Ws.CustomJson.Decoders exposing (..)

import Json.Encode
import Json.Decode exposing (field)

type alias IncomingWsMessage =
    { name : String
    , data : WsData
    }

type alias Vector =
    { x : Float
    , y : Float
    }

type alias WsData =
    { vector : Vector
    , size : Int
    , humidity : Int
    }

decodeIncomingWsMessage : Json.Decode.Decoder IncomingWsMessage
decodeIncomingWsMessage =
    Json.Decode.map2 IncomingWsMessage
        (field "name" Json.Decode.string)
        (field "data" decodeWsData)

decodeVector : Json.Decode.Decoder Vector
decodeVector =
    Json.Decode.map2 Vector
        (field "x" Json.Decode.float)
        (field "y" Json.Decode.float)

decodeWsData : Json.Decode.Decoder WsData
decodeWsData =
    Json.Decode.map3 WsData
        (field "vector" decodeVector)
        (field "size" Json.Decode.int)
        (field "humidity" Json.Decode.int)

encodeIncomingWsMessage : IncomingWsMessage -> Json.Encode.Value
encodeIncomingWsMessage record =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| record.name)
        , ("data",  encodeWsData <| record.data)
        ]

encodeVector : Vector -> Json.Encode.Value
encodeVector record =
    Json.Encode.object
        [ ("x",  Json.Encode.float <| record.x)
        , ("y",  Json.Encode.float <| record.y)
        ]

encodeWsData : WsData -> Json.Encode.Value
encodeWsData record =
    Json.Encode.object
        [ ("vector",  encodeVector <| record.vector)
        , ("size",  Json.Encode.int <| record.size)
        ]