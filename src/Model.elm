module Model exposing (..)
import Set exposing (Set)
import Keyboard exposing (..)

type Color = 
    Blue
    | Red

type alias Raindrop =
    {
          x : Int
        , y : Int
    }
type alias Pos =
    {
          x : Int
        , y : Int
    }

type ContactType =
    Avoid
    | Hit
type alias Gate =
    {
          x : Int
        , contactType : ContactType
        , openingStart : Int
        , openingEnd : Int
    }

type alias Star = 
    {
          pos : Pos
        , size : Int
        , speed : Int
    }

type alias Model = 
    {
          size : Int
        , humidity : Int
        , code : Int
        , color: Color
        , message : String
        , pos : Pos
        , keysDown : Set KeyCode
        , raindrops : List Raindrop
        , gates : List Gate
        , stars : List Star
        , isAlive : Bool
        , score : Int
    }