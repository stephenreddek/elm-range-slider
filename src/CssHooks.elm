module CssHooks exposing (..)

{-| Contains types for the view and the styles to share

@docs CssClasses the classes that can be used

@docs CssIds the ids that can be used
-}


{-| The classes that will be used on the range slider elements
-}
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


{-| The id for the range slider container
-}
type CssIds
    = Container
