module RangeSlider exposing
    ( RangeSlider, getValues, getSelectedValues
    , Msg(..), update, subscriptions
    , init, AxisTick, setDimensions, setExtents, setFormatter, setStepSize, setAxisTicks, setValues
    , view
    )

{-| A slider built natively in Elm


# Model

@docs RangeSlider, getValues, getSelectedValues


# Update

@docs Msg, update, subscriptions


# Configuring the slider

@docs init, AxisTick, setDimensions, setExtents, setFormatter, setStepSize, setAxisTicks, setValues


# View

@docs view

-}

import Browser
import Browser.Events
import Css
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode


{-| The base model for the slider
-}
type RangeSlider
    = RangeSlider Model


type alias Model =
    { dragPosition : RangeDrag
    , from : Float
    , to : Float
    , settings : Settings
    }


type alias Settings =
    { min : Float
    , max : Float
    , stepSize : Maybe Float
    , formatter : Float -> String
    , width : Float
    , height : Float
    , axisTicks : List AxisTick
    }


{-| Represents a tick that goes along the X axis.
The value determines where it should go,
isLabeled determines if the it should have a label below.
The label is formatted by the formatter.
-}
type alias AxisTick =
    { value : Float
    , isLabeled : Bool
    }


type RangeDrag
    = BeginDrag Drag
    | EndDrag Drag
    | None


type alias Position =
    { x : Int
    , y : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


{-| The basic type accepted by the update
-}
type Msg
    = DragStart (Drag -> RangeDrag) Position
    | DragAt Position
    | DragEnd Position


cssNamespace : String
cssNamespace =
    "range-slider-"


{-| Sets the width and height of the range slider when rendered
-}
setDimensions : Float -> Float -> RangeSlider -> RangeSlider
setDimensions width height (RangeSlider ({ settings } as model)) =
    RangeSlider { model | settings = { settings | width = width, height = height } }


{-| Sets the minimum and maximum values possible to select
-}
setExtents : Float -> Float -> RangeSlider -> RangeSlider
setExtents min max (RangeSlider ({ settings } as model)) =
    RangeSlider { model | settings = { settings | min = min, max = max } }


{-| Formats the value displayed above the handles and for axis ticks
-}
setFormatter : (Float -> String) -> RangeSlider -> RangeSlider
setFormatter formatter (RangeSlider ({ settings } as model)) =
    RangeSlider { model | settings = { settings | formatter = formatter } }


{-| Sets the step size which determines the interval for possible values
-}
setStepSize : Maybe Float -> RangeSlider -> RangeSlider
setStepSize stepSize (RangeSlider ({ settings } as model)) =
    RangeSlider { model | settings = { settings | stepSize = stepSize } }


{-| Sets the ticks that will appear in the x-axis.
-}
setAxisTicks : List AxisTick -> RangeSlider -> RangeSlider
setAxisTicks ticks (RangeSlider ({ settings } as model)) =
    RangeSlider { model | settings = { settings | axisTicks = ticks } }


{-| Sets the position of the 'from' handle and the 'to' handle.
Not intended to be used after the initial setup - it may not act as expected if the sliders are currently being moved.
-}
setValues : Float -> Float -> RangeSlider -> RangeSlider
setValues from to (RangeSlider model) =
    RangeSlider { model | from = from, to = to }


{-| Gets the current from and to values (from, to)
-}
getValues : RangeSlider -> ( Float, Float )
getValues (RangeSlider model) =
    ( getBeginValue model, getEndValue model )


{-| Gets the last selected from and to values (from, to)
-}
getSelectedValues : RangeSlider -> ( Float, Float )
getSelectedValues (RangeSlider model) =
    ( model.from, model.to )


{-| Returns a default range slider
-}
init : RangeSlider
init =
    let
        minValue =
            0.0

        maxValue =
            100.0

        defaultSettings =
            { min = minValue
            , max = maxValue
            , stepSize = Nothing
            , formatter = String.fromFloat
            , width = 200.0
            , height = 75.0
            , axisTicks = []
            }

        model =
            { dragPosition = None
            , from = 40.0
            , to = 0.0
            , settings = defaultSettings
            }
    in
    RangeSlider model


{-| Returns the subscriptions necessary to run
-}
subscriptions : RangeSlider -> Sub Msg
subscriptions (RangeSlider model) =
    case model.dragPosition of
        None ->
            Sub.none

        _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Json.Decode.map DragAt position)
                , Browser.Events.onMouseUp (Json.Decode.map DragEnd position)
                ]


{-| Takes a Msg and a RangeSlider and applies it to create an updated RangeSlider
-}
update : Msg -> RangeSlider -> RangeSlider
update msg (RangeSlider ({ settings } as model)) =
    case msg of
        DragStart createRangeDrag xy ->
            RangeSlider { model | dragPosition = createRangeDrag <| Drag xy xy }

        DragAt xy ->
            RangeSlider { model | dragPosition = updateDrag model.dragPosition xy }

        DragEnd _ ->
            RangeSlider { model | to = getEndValue model, from = getBeginValue model, dragPosition = None }


{-| Displays the range slider
-}
view : RangeSlider -> Html.Html Msg
view (RangeSlider model) =
    let
        barHeight =
            4

        handleDiameter =
            20

        valueRange =
            model.settings.max - model.settings.min

        rangeMidpoint =
            valueRange / 2

        toValue =
            getEndValue model

        fromValue =
            getBeginValue model

        scaleValue value =
            (value - model.settings.min) / valueRange * 100

        positionFromValue =
            scaleValue >> Css.pct >> Css.left

        barHighlightWidth =
            Css.width <| Css.pct <| (toValue - fromValue) / valueRange * 100

        handleTop =
            Css.top <| Css.px <| (model.settings.height - handleDiameter) / 2.0

        barTop =
            Css.top <| Css.px <| (model.settings.height - barHeight) / 2.0

        handle value dragCmd =
            span [ onMouseDown dragCmd, css [ Css.position Css.absolute, positionFromValue value, handleTop ], class (cssNamespace ++ "handle") ] []

        backgroundBar =
            span
                [ class (cssNamespace ++ "background-bar")
                , css
                    [ Css.position Css.absolute
                    , barTop
                    , Css.left <| Css.px 0
                    ]
                ]
                []

        highlightedBar =
            span [ css [ Css.position Css.absolute, positionFromValue fromValue, barTop, barHighlightWidth ], class (cssNamespace ++ "bar-highlight") ] []

        valueDisplay value =
            span [ css [ Css.position Css.absolute, positionFromValue value ], class (cssNamespace ++ "value") ] [ text <| model.settings.formatter value ]

        toTick : AxisTick -> Html a
        toTick tick =
            let
                percent =
                    scaleValue tick.value
            in
            span
                [ css [ Css.position Css.absolute, Css.left <| Css.pct percent ]
                , class (cssNamespace ++ "tick")
                , class
                    (if tick.isLabeled then
                        cssNamespace ++ "major-tick"

                     else
                        cssNamespace ++ "minor-tick"
                    )
                ]
                []

        axis =
            span [ class (cssNamespace ++ "axis"), css [ Css.position Css.absolute ] ] <|
                List.map toTick model.settings.axisTicks

        toLabel : Float -> Html a
        toLabel value =
            span
                [ css [ Css.position Css.absolute, Css.left <| Css.pct <| scaleValue value ], class (cssNamespace ++ "axis-label") ]
                [ text <| model.settings.formatter value ]

        axisLabels =
            span [ css <| [ Css.position Css.absolute, Css.left <| Css.px 0, Css.bottom <| Css.px 0, Css.width <| Css.px model.settings.width, Css.height <| Css.px 9 ] ] <|
                List.map (toLabel << .value) <|
                    List.filter .isLabeled model.settings.axisTicks

        {- Determine which handle is render at the top to prevent both handles being stuck at maximum and unable to move -}
        handles =
            if toValue < rangeMidpoint then
                [ handle fromValue BeginDrag
                , handle toValue EndDrag
                ]

            else
                [ handle toValue EndDrag
                , handle fromValue BeginDrag
                ]
    in
    div [ id (cssNamespace ++ "container") ]
        [ span [ css [ Css.display Css.inlineBlock, Css.position Css.relative, Css.width <| Css.px model.settings.width, Css.height <| Css.px model.settings.height ] ] <|
            [ backgroundBar
            , highlightedBar
            ]
                ++ handles
                ++ [ valueDisplay fromValue
                   , valueDisplay toValue
                   , axis
                   , axisLabels
                   ]
        ]
        |> toUnstyled


position : Json.Decode.Decoder Position
position =
    Json.Decode.map2 Position
        (Json.Decode.field "pageX" Json.Decode.int)
        (Json.Decode.field "pageY" Json.Decode.int)


onMouseDown : (Drag -> RangeDrag) -> Attribute Msg
onMouseDown createRangeDrag =
    Html.Styled.Events.on "mousedown" <| Json.Decode.map (DragStart createRangeDrag) position


updateDrag : RangeDrag -> Position -> RangeDrag
updateDrag rangeDrag newPosition =
    case rangeDrag of
        BeginDrag { start } ->
            BeginDrag <| Drag start newPosition

        EndDrag { start } ->
            EndDrag <| Drag start newPosition

        None ->
            None


getEndValue : Model -> Float
getEndValue { dragPosition, from, to, settings } =
    case dragPosition of
        None ->
            to

        BeginDrag _ ->
            to

        EndDrag { start, current } ->
            let
                difference =
                    toFloat current.x - toFloat start.x

                normalizedDifference =
                    difference * (settings.max - settings.min) / settings.width

                value =
                    valueBySteps settings to normalizedDifference
            in
            clamp from settings.max value


valueBySteps : Settings -> Float -> Float -> Float
valueBySteps settings baseValue normalizedDifference =
    case settings.stepSize of
        Just stepSize ->
            stepSize * (toFloat <| Basics.round <| (baseValue + normalizedDifference) / stepSize)

        Nothing ->
            baseValue + normalizedDifference


getBeginValue : Model -> Float
getBeginValue { dragPosition, from, to, settings } =
    case dragPosition of
        None ->
            from

        BeginDrag { start, current } ->
            let
                difference =
                    toFloat current.x - toFloat start.x

                normalizedDifference =
                    difference * (settings.max - settings.min) / settings.width

                value =
                    valueBySteps settings from normalizedDifference
            in
            clamp settings.min to value

        EndDrag _ ->
            from
