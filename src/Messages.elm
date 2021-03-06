module Messages exposing (..)
import Keyboard exposing (..)
import Time exposing (Time)
import Model exposing (..)

type Msg
    = 
      KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | WebsocketMessage String
    | NewGate Time
    | NewStar Time
    | Tick Time
    | GenerateNewGate (Int, ContactType, Int, Int) 
    | GenerateNewStar (Int, Int)