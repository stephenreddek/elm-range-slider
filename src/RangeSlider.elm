module RangeSlider exposing (Model, Settings, StepSize, Msg, activate, view, update, subscriptions)

{-| A slider built natively in Elm

#The base model for the range slider
@docs Model

@docs Settings the settings for the slider

@docs StepSize How big each step for the slider will be

@docs Msg is the type expected by update

@docs update takes a model and a message and applies it to create an updated model

@docs activate returns everything that is needed to create a range slider

@docs subscriptions the necessary subscriptions to make everything work

@docs view creates a basic html structure for the range slider
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse exposing (Position)
import Json.Decode as Json exposing ((:=))
import Css exposing (..)


{-| The base model for the slider
-}
type alias Model =
    { begin : Float
    , end : Float
    , min : Float
    , max : Float
    , dragPosition : RangeDrag
    , settings : Settings
    }


{-| The settings for the range slider
-}
type alias Settings =
    { stepSize : Maybe StepSize
    , formatter : Float -> String
    }


{-| How big each step for the slider will be
-}
type alias StepSize =
    Float


type RangeDrag
    = BeginDrag Drag
    | EndDrag Drag
    | None


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


defaultSettings : Settings
defaultSettings =
    { stepSize = Nothing
    , formatter = (\value -> (toString value))
    }


{-| Creates an initial model
-}
initialModel : Model
initialModel =
    { begin = 40.0
    , end = 60.0
    , min = 0.0
    , max = 100.0
    , dragPosition = None
    , settings = defaultSettings
    }


{-| Returns the necessities for initializing a range slider
-}
activate : ( Model, Cmd Msg )
activate =
    ( initialModel, Cmd.none )


{-| Returns the subscriptions necessary to run
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragPosition of
        None ->
            Sub.none

        _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


{-| takes a model and a message and applies it to create an updated model
-}
update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        DragStart createRangeDrag xy ->
            ( { model | dragPosition = createRangeDrag <| Drag xy xy }, Cmd.none )

        DragAt xy ->
            ( { model | dragPosition = updateDrag model.dragPosition xy }, Cmd.none )

        DragEnd _ ->
            ( { model | end = getEndValue model, begin = getBeginValue model, dragPosition = None }, Cmd.none )


{-| Displays the range slider
-}
view : Model -> Html Msg
view model =
    let
        backgroundBarColor =
            rgb 238 238 238

        primaryColor =
            rgb 92 144 209

        barHeight =
            4

        containerWidth =
            200

        containerHeight =
            75

        endValue =
            getEndValue model

        endPosition =
            left <| pct <| endValue / model.max * 100

        beginValue =
            getBeginValue model

        beginPosition =
            left <| pct <| beginValue / model.max * 100

        styles =
            Css.asPairs >> Html.Attributes.style

        handleDiameter =
            20

        handleTop =
            (containerHeight - handleDiameter) / 2

        handleStyles =
            [ position absolute, top <| px handleTop, backgroundColor (rgb 256 256 256), boxShadow4 (px 0) (px 1) (px 5) (rgba 0 0 0 0.75), marginLeft (px -7), borderRadius <| pct 50, Css.height <| px handleDiameter, Css.width (px handleDiameter) ]

        barTop =
            (containerHeight - barHeight) / 2

        backgroundBarStyles =
            [ position absolute, top (px barTop), left <| px 0, backgroundColor backgroundBarColor, Css.height <| px barHeight, Css.width <| pct 100 ]

        highlightedBarStyles =
            [ position absolute, top (px barTop), backgroundColor primaryColor, Css.height <| px barHeight, Css.width <| pct <| (endValue - beginValue) / model.max * 100 ]

        valueStyles =
            [ position absolute, top <| px 0, backgroundColor primaryColor, color <| rgb 256 256 256, padding2 (px 1) (px 5), borderRadius <| px 3, transform <| translateX <| pct -50, lineHeight <| Css.em 1.3, fontSize <| px 13, fontFamilies [ "Open Sans", "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ] ]

        axisStyles =
            [ position absolute, bottom <| px 0, left <| px 0, Css.height <| px 8, Css.width <| pct 100 ]

        tickStyles =
            [ position absolute, backgroundColor <| rgb 153 153 153, Css.width <| px 1 ]

        majorTickStyles =
            (Css.height <| px 8) :: tickStyles

        minorTickStyles =
            (Css.height <| px 4) :: (marginBottom <| px 4) :: tickStyles

        fromHandle =
            span [ onMouseDown BeginDrag, styles <| beginPosition :: handleStyles ] []

        toHandle =
            span [ onMouseDown EndDrag, styles <| endPosition :: handleStyles ] []

        backgroundBar =
            span [ styles backgroundBarStyles ] []

        highlightedBar =
            span [ styles <| beginPosition :: highlightedBarStyles ] []

        fromValue =
            span [ styles <| beginPosition :: valueStyles ] [ Html.text <| model.settings.formatter beginValue ]

        toValue =
            span [ styles <| endPosition :: valueStyles ] [ Html.text <| model.settings.formatter endValue ]

        toTick : Int -> Html a
        toTick percent =
            span
                [ styles <|
                    ((left <| pct <| (toFloat percent) * 10)
                        :: (if Basics.rem percent 5 == 0 then
                                majorTickStyles
                            else
                                minorTickStyles
                           )
                    )
                ]
                []

        axis =
            span [ styles axisStyles ] <|
                List.map toTick [0..10]
    in
        div [ style [ ( "text-align", "center" ) ] ]
            [ span [ style [ ( "display", "inline-block" ), ( "position", "relative" ), ( "border", "2px solid #eee" ), ( "width", (toString containerWidth) ++ "px" ), ( "height", (toString containerHeight) ++ "px" ) ] ]
                [ backgroundBar
                , highlightedBar
                , fromHandle
                , toHandle
                , fromValue
                , toValue
                , axis
                ]
            ]


onMouseDown : (Drag -> RangeDrag) -> Attribute Msg
onMouseDown createRangeDrag =
    on "mousedown" <| Json.map (DragStart createRangeDrag) Mouse.position


updateDrag : RangeDrag -> Position -> RangeDrag
updateDrag rangeDrag position =
    case rangeDrag of
        BeginDrag { start } ->
            BeginDrag <| Drag start position

        EndDrag { start } ->
            EndDrag <| Drag start position

        None ->
            None


getEndValue : Model -> Float
getEndValue model =
    case model.dragPosition of
        None ->
            model.end

        BeginDrag _ ->
            model.end

        EndDrag { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                normalizedDifference =
                    difference * 100.0 / 200.0

                value =
                    valueBySteps model.settings model.end normalizedDifference
            in
                clamp model.begin model.max value


valueBySteps : Settings -> Float -> Float -> Float
valueBySteps settings baseValue normalizedDifference =
    case settings.stepSize of
        Just stepSize ->
            stepSize * (toFloat <| round <| (baseValue + normalizedDifference) / stepSize)

        Nothing ->
            baseValue + normalizedDifference


getBeginValue : Model -> Float
getBeginValue model =
    case model.dragPosition of
        None ->
            model.begin

        BeginDrag { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                normalizedDifference =
                    difference * 100.0 / 200.0

                value =
                    valueBySteps model.settings model.begin normalizedDifference
            in
                clamp model.min model.end value

        EndDrag _ ->
            model.begin
