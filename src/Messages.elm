module Messages exposing (..)
import Keyboard exposing (..)

type Msg
    = Foo
    | KeyMsg Keyboard.KeyCode
    | WebsocketMessage String