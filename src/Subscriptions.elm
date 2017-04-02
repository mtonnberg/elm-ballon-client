module Subscriptions exposing(subscriptions)

import Model exposing (..)
import Messages exposing (..)
import Keyboard exposing (..)
import WebSocket
import Time exposing (Time, second)

subscriptions : Model -> Sub Msg
subscriptions model =
  -- let 
  --   tickLength = if model % 10 == 0 then 100 else 1000
  -- in 
  Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , WebSocket.listen "ws://192.168.0.5:5999" WebsocketMessage
        , Time.every second (NewGate)
        , Time.every 16 (Tick)
        , Time.every 6000 (NewStar)
        -- , Time.every 1000 (LoosePoints)
        ]