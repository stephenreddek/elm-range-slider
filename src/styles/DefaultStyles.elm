module DefaultStyles exposing (css)

{-| Some default styles for the range slider.
    This can be used as a base for new styles.

@docs css the default style definitions which can be compiled
-}

import Css exposing (..)
import Css.Namespace exposing (namespace)
import CssHooks exposing (..)


white : Color
white =
    rgb 256 256 256


handleDiameter : Float
handleDiameter =
    20


backgroundBarColor : Color
backgroundBarColor =
    rgb 238 238 238


primaryColor : Color
primaryColor =
    rgb 92 144 209


{-| The default style definitions which can be compiled
-}
css : Stylesheet
css =
    (stylesheet << namespace "rangeSlider")
        [ (#) Container
            [ textAlign center ]
        , (.) Handle
            [ backgroundColor white
            , boxShadow4 (px 0) (px 1) (px 5) (rgba 0 0 0 0.75)
            , marginLeft (px -7)
            , borderRadius <| pct 50
            , height <| px handleDiameter
            , width <| px handleDiameter
            ]
        , (.) BackgroundBar
            [ backgroundColor backgroundBarColor
            , height <| px 4
            , width <| pct 100
            ]
        , (.) BarHighlight
            [ backgroundColor primaryColor
            , height <| px 4
            ]
        , (.) Value
            [ position absolute
            , top <| px 0
            , backgroundColor primaryColor
            , color white
            , padding2 (px 1) (px 5)
            , borderRadius <| px 3
            , transform <| translateX <| pct -50
            , lineHeight <| em 1.3
            , fontSize <| px 13
            , fontFamilies [ "Open Sans", "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]
            ]
        , (.) Axis
            [ position absolute
            , bottom <| px 14
            , left <| px 0
            , height <| px 8
            , width <| pct 100
            ]
        , (.) AxisLabel
            [ position absolute
            , bottom <| px 0
            , fontSize <| px 9
            , transform <| translateX <| pct -50
            , color <| rgb 153 153 153
            ]
        , (.) Tick
            [ position absolute
            , backgroundColor <| rgb 153 153 153
            , width <| px 1
            ]
        , (.) MajorTick
            [ height <| px 8 ]
        , (.) MinorTick
            [ height <| px 4
            , marginBottom <| px 4
            ]
        ]
