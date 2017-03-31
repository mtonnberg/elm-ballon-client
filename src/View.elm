module View exposing (view)
import Model exposing (..)
import Messages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

getHalfSize: Int -> String
getHalfSize size =
    let 
        n = (toFloat size) /2
    in
    (toString (floor n))  ++ "px"

getColorHex: Color -> String
getColorHex c =
    case c of
    Red -> "#C55"
    Blue -> "#CC5"

getStyle: Model -> Attribute msg
getStyle model = 
    let size = (toString model.size) ++ "px"
        margin = "-" ++ (getHalfSize model.size)
        bgColor = getColorHex model.color
    in 
    style [
          ("height", size)
        , ("width", size)
        , ("margin-left", margin)
        , ("margin-top", margin)
        , ("left", (toString model.x) ++ "px")
        -- , ("background-color", bgColor)
    ]

view: Model -> Html Messages.Msg
view model = div []
    [
        img [getStyle model, class "box", src "/assets/red-balloon-hi.png"] []
        , div [] [text (model.message)]
    ]