import Model exposing (..)
import Messages exposing (..)
import View exposing (..)
import Html exposing (..)
import Update exposing (..)
import Subscriptions exposing (..)
import Set exposing (Set)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


initialModel = {
          size = 50
        , humidity = 0
        , code = 0
        , color = Blue
        , message = ""
        , pos = Pos 120 150
        , keysDown = Set.empty
        , raindrops = []
        , gates =  [
        --   {
        --       x = 1000
        --     , contactType = Avoid
        --     , openingStart = 40
        --     , openingEnd = 150
        -- }
        ]
        , stars = []
        , isAlive = True
        , score = 0
    }

init : (Model, Cmd Msg)
init = initialModel ! [Cmd.none]