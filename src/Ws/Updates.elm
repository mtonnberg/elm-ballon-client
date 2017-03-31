module Ws.Updates exposing (updateModelWithWsData)


import Json.Decode exposing (..)

updateSize wsData model =
    case decodeString int wsData of
    Err _ -> model
    Ok size -> {model | size = size }

type alias WsData =
  { name : String
  , data  : String 
  }


wsDataDecoder = map2 WsData (field "name" string) (field "data" string)

updateModelWithWsData s model =
    case decodeString wsDataDecoder s  of  
    Err _ -> model                                                                                                                                                            
    Ok  ws ->
        case ws.name of
        "size" -> updateSize ws.data model
        _ -> model