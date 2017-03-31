import Model exposing (..)
import Messages exposing (..)
import View exposing (..)
import Html exposing (..)
import Update exposing (..)
import Subscriptions exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


initialModel = Model 50 100 0 Blue ""

init : (Model, Cmd Msg)
init = initialModel ! [Cmd.none]