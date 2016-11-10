module CssHooks exposing (..)

{-| Contains types for the view and the styles to share
    @docs CssClasses the classes that can be used

    @docs CssIds the ids that can be used
-}

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)


type CssClasses
    = Handle
    | BackgroundBar
    | BarHighlight
    | Value
    | Axis
    | AxisLabel
    | Tick
    | MajorTick
    | MinorTick


type CssIds
    = Container
