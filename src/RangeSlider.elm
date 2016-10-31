module RangeSlider exposing (Model)

{-| A slider built natively in Elm

#The base model for the rnage slider
@docs Model
-}

import Html exposing (..)
import Html.Attributes as Attrs exposing (href, placeholder, tabindex, type', value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onWithOptions, targetValue)


{-| The base model for the slider
-}
type alias Model =
    { beginning : Float
    , end : Float
    , min : Float
    , max : Float
    }


initialModel : Model
initialModel =
    { beginning = 40.0
    , end = 60.0
    , min = 0.0
    , max = 100.0
    }
