module ColorUtils exposing
    ( black
    , blue
    , blueHue
    , darkBlue
    , grey
    , hsl
    , lightGrey
    , roseHue
    , violetHue
    , white
    )

import Color
import Element


toEl : Color.Color -> Element.Color
toEl color =
    let
        c =
            Color.toRgba color
    in
    Element.rgba c.red c.green c.blue c.alpha


hsl : Int -> Float -> Float -> Element.Color
hsl h s l =
    let
        hf =
            toFloat h / 360
    in
    Color.hsl hf s l |> toEl


colorToCss : Element.Color -> String
colorToCss color =
    let
        c =
            Element.toRgb color

        c2 =
            Color.rgba c.red c.green c.blue c.alpha
    in
    Color.toCssString c2


grey : Float -> Element.Color
grey l =
    hsl 0 0 l


roseHue =
    326


violetHue =
    266


blueHue =
    205


blue =
    hsl blueHue 0.8 0.5


darkBlue =
    hsl blueHue 0.8 0.45


lightGrey =
    grey 0.9


white =
    Color.white |> toEl


black =
    Color.black |> toEl
