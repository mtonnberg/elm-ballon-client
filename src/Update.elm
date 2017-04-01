module Update exposing(update)

import Model exposing (..)
import Messages exposing (..)
import Ws.Updates exposing (updateModelWithWsData)
import WebSocket
import Set exposing (Set)
import Keyboard exposing (..)
import Json.Encode exposing (..)
import Char exposing (fromCode)
import Random

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

type alias KeyWsMessage =
    {
          name: String
        , keys: Set KeyCode
    }


toListOfValues: List Int -> List Value
toListOfValues ss = List.map (\s -> Json.Encode.int s) ss

keyCodesJson : Set KeyCode -> Value
keyCodesJson keycodes =
    let
      ints = Set.toList <| keycodes
      values = toListOfValues ints
    in
      
    list <| values

keyWsMessageToJson : KeyWsMessage -> Value
keyWsMessageToJson keymsg =
    object [ ("name", string "keycodes")
           , ("data", keyCodesJson keymsg.keys)
           ]

handleInput code model = 
    let step = 5
    in
    case code of
    -- 38 ->  increaseSize model step
    -- 40 ->  decreaseSize model step
    37 ->  { model | x = model.x - step}
    39 ->  { model | x = model.x + step}
    -- 65 ->  flipColor model
    _ ->   { model | code = code } -- WebSocket.send "ws://192.168.0.5:5999" "foo"

modifyWithInputs model = 
    if Set.isEmpty model.keysDown then
        model
    else
        Set.foldl handleInput model model.keysDown

updateGate : Gate -> Gate
updateGate gate = { gate | x = gate.x-1 }

updateGates : Model -> Model
updateGates model = 
    let
      updatedGates = List.map updateGate model.gates
      filteredGates = List.filter (\g -> g.x > 0) updatedGates
    in
    { model | gates = filteredGates }

generateANewGate : ContactType -> Int -> Int -> Gate
generateANewGate contactType start width= {
          x = 1000
        , contactType = contactType
        , openingStart = start
        , openingEnd = (start+width)
    }
update: Msg -> Model -> (Model, Cmd Msg)
update msg m1 =
    let
        model = updateGates m1
        -- model = modifyWithInputs m 
        cmd = WebSocket.send "ws://192.168.0.5:5999" (encode 0 (keyWsMessageToJson (KeyWsMessage  "keycodes" model.keysDown)))
    in
    case msg of
    KeyDown key ->
      ({ model | keysDown = Set.insert key model.keysDown }, cmd)
    KeyUp key ->
      ({ model | keysDown = Set.remove key model.keysDown }, cmd)
    WebsocketMessage s ->  updateModelWithWsData s model ! [cmd]
    NewGate time -> 
        let
            rand = Random.map4 
                (\a b c d -> (a,b,c,d))
                (Random.int 1 10)
                (Random.map (\b -> if b then Avoid else Hit) Random.bool)
                (Random.int 0 500)
                (Random.int 100 700)
        in
        (model, Random.generate GenerateNewGate rand )
    --     let (m, cmd) = Theme.update themeMsg model
    --   in
    --   (m, Cmd.map Theme cmd)
    GenerateNewGate (r, contactType, start, width) -> 
        case r of
        1 -> { model | gates = (generateANewGate contactType start width) :: model.gates } ! []
        _ -> model ! []