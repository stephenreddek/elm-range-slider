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

        endPosition =
            getEndValue model

        beginPosition =
            getBeginValue model
    in
        div [ style [ ( "position", "relative" ), ( "background-color", "red" ), ( "width", (toString containerWidth) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ]
            [ span [ onMouseDown BeginDrag, style [ ( "position", "absolute" ), ( "background-color", "blue" ), ( "left", (toString <| beginPosition / model.max * containerWidth) ++ "px" ), ( "width", (toString height) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ] []
            , span [ onMouseDown EndDrag, style [ ( "position", "absolute" ), ( "background-color", "blue" ), ( "left", (toString <| endPosition / model.max * containerWidth) ++ "px" ), ( "width", (toString height) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ] []
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
getEndValue { dragPosition, end, min, max, begin } =
    case dragPosition of
        None ->
            end

        BeginDrag _ ->
            end

        EndDrag { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                value =
                    end + (difference * 100.0 / 200.0)
            in
                clamp begin max value


getBeginValue : Model -> Float
getBeginValue { dragPosition, end, min, max, begin } =
    case dragPosition of
        None ->
            begin

        BeginDrag { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                value =
                    begin + (difference * 100.0 / 200.0)
            in
                clamp min end value

        EndDrag _ ->
            begin
