module Subscriptions exposing(subscriptions)

import Model exposing (..)
import Messages exposing (..)
import Keyboard exposing (..)
import WebSocket

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
        [ Keyboard.presses KeyMsg
        , WebSocket.listen "ws://192.168.0.5:5999" WebsocketMessage
        ]