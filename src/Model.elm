module Model exposing (..)

type Color = 
    Blue
    | Red

type alias Model = 
    {
          size : Int
        , x : Int
        , code : Int
        , color: Color
        , message : String
    }