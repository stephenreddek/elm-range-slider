module RangeSlider exposing (Model, Msg, Msg(..), activate, view, update, subscriptions)

{-| A slider built natively in Elm

#The base model for the range slider
@docs Model

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
    }


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


{-| Creates an initial model
-}
initialModel : Model
initialModel =
    { begin = 40.0
    , end = 60.0
    , min = 0.0
    , max = 100.0
    , dragPosition = None
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
        height =
            20

        containerWidth =
            200

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

        handleStyles =
            [ position absolute, backgroundColor (rgb 0 0 256), marginLeft (px -7), top <| px -15, borderRadius <| pct 50, Css.height <| px 15, Css.width (px 15) ]

        lineStyles =
            [ position absolute, backgroundColor (rgb 0 0 256), Css.height <| px height, Css.width <| px 2 ]

        barStyles =
            [ position absolute, backgroundColor (rgb 0 0 256), Css.height <| px height, Css.width <| pct <| (endValue - beginValue) / model.max * 100 ]

        fromHandle =
            span [ onMouseDown BeginDrag, styles <| beginPosition :: handleStyles ] []

        fromLine =
            span [ onMouseDown BeginDrag, styles <| beginPosition :: lineStyles ] []

        toHandle =
            span [ onMouseDown EndDrag, styles <| endPosition :: handleStyles ] []

        toLine =
            span [ onMouseDown EndDrag, styles <| endPosition :: lineStyles ] []

        bar =
            span [ styles <| beginPosition :: barStyles ] []
    in
        div [ style [ ( "text-align", "center" ) ] ]
            [ span [ style [ ( "display", "inline-block" ), ( "position", "relative" ), ( "background-color", "red" ), ( "width", (toString containerWidth) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ]
                [ fromLine
                , toLine
                , bar
                , fromHandle
                , toHandle
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

                value =
                    model.end + (difference * 100.0 / 200.0)
            in
                clamp model.begin model.max value


getBeginValue : Model -> Float
getBeginValue model =
    case model.dragPosition of
        None ->
            model.begin

        BeginDrag { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                value =
                    model.begin + (difference * 100.0 / 200.0)
            in
                clamp model.min model.end value

        EndDrag _ ->
            model.begin
