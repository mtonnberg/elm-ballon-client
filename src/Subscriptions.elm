module Subscriptions exposing(subscriptions)

import Model exposing (..)
import Messages exposing (..)
import Keyboard exposing (..)
import WebSocket
import Time exposing (Time, second)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        -- , WebSocket.listen "ws://192.168.0.5:5999" WebsocketMessage
        , Time.every second (NewGate)
        -- , Time.every Time.millisecond (Tick)
        ]