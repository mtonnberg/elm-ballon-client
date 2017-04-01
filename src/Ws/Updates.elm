module Ws.Updates exposing (updateModelWithWsData)
import Ws.CustomJson.Decoders exposing (..)
import Model exposing (..)
import Json.Decode exposing (..)

updateModelWithWsData : String -> Model -> Model
updateModelWithWsData s model =
    case decodeString decodeIncomingWsMessage s  of  
    Err e -> { model | message = (toString e) ++ s }                                                                                                                                                
    Ok  ws -> { model |
          size = ws.data.size
        , pos = (Pos (floor ws.data.vector.x) (floor ws.data.vector.y))
        , humidity = ws.data.humidity
        }
