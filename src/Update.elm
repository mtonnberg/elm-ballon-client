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

handleInput : KeyCode -> Model -> Model
handleInput code model = 
    let 
        step = 5
        oldPos = model.pos
    in
    case code of
    84 ->  increaseSize model step
    71 ->  decreaseSize model step
    40 ->  { model | pos = { oldPos | y = oldPos.y + step } }
    38 ->  { model | pos = { oldPos | y = oldPos.y - step } }
    37 ->  { model | pos = { oldPos | x = oldPos.x - step } }
    39 ->  { model | pos = { oldPos | x = oldPos.x + step } }
    -- 65 ->  flipColor model
    _ ->   { model | code = code }

modifyWithInputs : Model -> Model
modifyWithInputs model = 
    if Set.isEmpty model.keysDown || not model.isAlive then
        model
    else
        Set.foldl handleInput model model.keysDown

updateGate gate = { gate | x = gate.x-1 }

updateGates : Model -> Model
updateGates model = 
    let
      updatedGates = List.map updateGate model.gates
      filteredGates = List.filter (\g -> g.x > 0) updatedGates
    in
    { model | gates = filteredGates }


updateStar : Star -> Star
updateStar star = 
    let
        oldPos = star.pos
        newPos = { oldPos | x = oldPos.x-star.speed }
    in
      { star | pos = newPos }

updateStars : Model -> Model
updateStars model =
    let
      updatedStars = List.map updateStar model.stars
      filteredStars = List.filter (\g -> g.pos.x > 0) updatedStars
    in
    { model | stars = filteredStars }

generateANewGate : ContactType -> Int -> Int -> Gate
generateANewGate contactType start width= {
          x = 1000
        , contactType = contactType
        , openingStart = start
        , openingEnd = (start+width)
    }

doesCollideWithGate : Int -> Pos -> Gate -> Bool
doesCollideWithGate s pos gate =
    let
        size = toFloat s 
        playerleftBoundery = pos.x-(floor (size/2.0))
        playerrightBoundery = pos.x+(floor (size/2.0))
        playerUpperBoundery = pos.y-(floor (size/2.0))
        playerLowerBoundery = pos.y+(floor (size/2.0))
    in
      
       gate.contactType == Avoid
    && (
            (playerleftBoundery < gate.x
            &&
            (playerrightBoundery) >= gate.x
            ) 
            ||
            (
                playerleftBoundery >= gate.x
                &&
                playerleftBoundery <= (gate.x+20)
            )
    )
    && (
        playerUpperBoundery <= gate.openingStart
        || playerLowerBoundery >= gate.openingEnd
    )

doesCollideWithAnyGate : Model -> Bool
doesCollideWithAnyGate model=
    List.any (doesCollideWithGate model.size model.pos) model.gates

doesCollideWithStar : Int -> Pos -> Star -> Bool
doesCollideWithStar size1 pos star =
    let
        size = toFloat size1 
        playerleftBoundery = pos.x-(floor (size/2.0))
        playerrightBoundery = pos.x+(floor (size/2.0))
        playerUpperBoundery = pos.y-(floor (size/2.0))
        playerLowerBoundery = pos.y+(floor (size/2.0))
    in
    (    
        (playerleftBoundery < star.pos.x
        &&
        (playerrightBoundery) >= star.pos.x
        ) 
        ||
        (
            playerleftBoundery >= star.pos.x
            &&
            playerleftBoundery <= (star.pos.x+star.size)
        )
    )
    && 
    (
        (
            playerUpperBoundery < star.pos.y
            &&
            (playerLowerBoundery) >= star.pos.y
        ) 
        ||
        (
            playerUpperBoundery >= star.pos.y
            &&
            playerUpperBoundery <= (star.pos.y+star.size)
        )
    )


doesCollideWithAnyStar : Model -> Model
doesCollideWithAnyStar model=
    let
      (capturedStars, starsToKeep) = List.partition (doesCollideWithStar model.size model.pos) model.stars
    in
    {model | stars = starsToKeep,  score = model.score + (List.length capturedStars)}

checkIfAlive : Model -> Model
checkIfAlive model =
    -- model
    if model.isAlive then
        { model | isAlive = not (doesCollideWithAnyGate model)}
    else
        model

addStar stars y speed =
    (Star (Pos 1000 y) 50 speed) :: stars

-- updateInputs msg model
update: Msg -> Model -> (Model, Cmd Msg)
update msg m1 =
    let
        m2 = updateGates m1
        m3 = updateStars m2
        m4 = checkIfAlive m3
        m5 = doesCollideWithAnyStar m4
        model = modifyWithInputs m5 
        cmd = WebSocket.send "ws://192.168.0.5:5999" (encode 0 (keyWsMessageToJson (KeyWsMessage  "keycodes" model.keysDown)))
    in
    case model.isAlive of
    True -> 
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
                    (Random.map (\b -> if b then Avoid else Avoid) Random.bool) --Always avoid
                    (Random.int 0 300)
                    (Random.int 200 700)
            in
            (model, Random.generate GenerateNewGate rand )
        GenerateNewGate (r, contactType, start, width) -> 
            case r of
            1 ->
                let
                  newGate = generateANewGate contactType start width
                  starsWithUpper = addStar model.stars (newGate.openingStart + 20) 1
                  starsWithGateStars = addStar starsWithUpper (newGate.openingEnd - 40) 1
                in
                  { model | gates = newGate :: model.gates, stars = starsWithGateStars } ! []
            _ -> model ! []
        Tick time -> model ! []
        NewStar time -> 
            let
                yPos = Random.map2 (\a b -> (a,b)) (Random.int 1 700) (Random.int 1 4)
            in
            (model, Random.generate GenerateNewStar yPos)
        GenerateNewStar (y, speed) -> ({model | stars = (addStar model.stars y speed)}, Cmd.none)
    False -> model ! []