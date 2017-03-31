module Update exposing(update)

import Model exposing (..)
import Messages exposing (..)
import Ws.Updates exposing (updateModelWithWsData)
import WebSocket

flipColor: Model -> Model
flipColor model = 
    case model.color of
    Red -> { model | color = Blue }
    _ -> {model | color = Red }

increaseSize model step = 
    if model.size < (step*100) then
        { model | size = model.size + step}
    else model

decreaseSize model step =
    if model.size > (step*5) then
        { model | size = model.size - step}
    else model

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
    Foo -> (model, Cmd.none)
    KeyMsg code ->
        let step = 5
        in
        case code of
        38 ->  (increaseSize model step) ! []
        40 ->  (decreaseSize model step) ! []
        37 ->  ( { model | x = model.x - step}, Cmd.none )
        39 ->  ( { model | x = model.x + step}, Cmd.none )
        65 ->  (flipColor model) ! []
        _ ->   {model | code = code } ! [] -- WebSocket.send "ws://192.168.0.5:5999" "foo"
    WebsocketMessage s ->  updateModelWithWsData s model ! []