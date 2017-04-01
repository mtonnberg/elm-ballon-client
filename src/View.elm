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
        , ("left", (toString model.pos.x) ++ "px")
        , ("top", (toString model.pos.y) ++ "px")
        -- , ("background-color", bgColor)
    ]

playAreaHeight = 700
playAreaWidth = 1000

raindrops humidity = div [] [text <| toString humidity]

renderGateInner gate cssClass =
    div [class "gate", style [("left", (toString gate.x) ++ "px")]] [
        div [
              class cssClass
            , style [
                  ("top", "0")
                , ("height", (toString (gate.openingStart)) ++ "px")
                ]
            ] []
        ,div [
              class cssClass
            , style [
                  ("top", (toString (gate.openingEnd)) ++ "px")
                , ("height", (toString (playAreaHeight - gate.openingEnd)) ++ "px")
                ]
            ] []
    ]


renderHitGate gate = renderGateInner gate "hit-gate"
renderAvoidGate gate = renderGateInner gate "avoid-gate"
-- renderGate : Gate -> Html Messages.Msg
renderGate gate =
    if gate.contactType == Hit then
        renderHitGate gate
    else 
        renderAvoidGate gate

renderGates gates =
    div [] <| List.map renderGate gates

bgStyle humidity =
    [("color", "red")]
    -- let
    --     percentage = (toFloat humidity)/100
    --     opacityValue = toString (percentage)
    -- in
    -- if percentage < 0.5 then
    --     [("color", "red"), ("background", "linear-gradient("
    --         ++ "rgba(0, 0, 0, " ++ opacityValue ++")," 
    --         ++ "rgba(0, 0, 0, " ++ opacityValue ++")"
    --         ++ "), url(/assets/bg.jpg) no-repeat center center fixed"
    --         )]
    -- else
    --     [("color", "red"), ("background", "linear-gradient("
    --         ++ "rgba(0, 0, 0, 0.5)," 
    --         ++ "rgba(0, 0, 0, 0.5)"
    --         ++ "), url(/assets/bg.jpg) no-repeat center center fixed"
    --         )]

view: Model -> Html Messages.Msg
view model = div [class "bg", style (bgStyle model.humidity)]
    [
        img [getStyle model, class "box", src "/assets/red-balloon-hi.png"] []
        , div [] [text (model.message)]
        , div [] [text (toString model.pos)]
        , div [] [text (toString model.code)]
        , renderGates model.gates
    ]