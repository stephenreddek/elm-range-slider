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
    { beginning : Float
    , end : Float
    , min : Float
    , max : Float
    , dragPosition : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


{-| The basic type accepted by the update
-}
type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


{-| Creates an initial model
-}
initialModel : Model
initialModel =
    { beginning = 40.0
    , end = 60.0
    , min = 0.0
    , max = 100.0
    , dragPosition = Nothing
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
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


{-| takes a model and a message and applies it to create an updated model
-}
update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        DragStart xy ->
            ( { model | dragPosition = Just <| Drag xy xy }, Cmd.none )

        DragAt xy ->
            ( { model | dragPosition = (Maybe.map (\{ start } -> Drag start xy) model.dragPosition) }, Cmd.none )

        DragEnd _ ->
            ( { model | end = getEndValue model, dragPosition = Nothing }, Cmd.none )


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
    in
        div [ style [ ( "position", "relative" ), ( "background-color", "red" ), ( "width", (toString containerWidth) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ]
            [ span [ style [ ( "position", "absolute" ), ( "background-color", "blue" ), ( "left", (toString <| model.beginning / model.max * containerWidth) ++ "px" ), ( "width", (toString height) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ] []
            , span [ onMouseDown, style [ ( "position", "absolute" ), ( "background-color", "blue" ), ( "left", (toString <| endPosition / model.max * containerWidth) ++ "px" ), ( "width", (toString height) ++ "px" ), ( "height", (toString height) ++ "px" ) ] ] []
            ]


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.map DragStart Mouse.position)


getEndValue : Model -> Float
getEndValue { dragPosition, end } =
    case dragPosition of
        Nothing ->
            end

        Just { start, current } ->
            let
                difference =
                    (toFloat current.x) - (toFloat start.x)

                value =
                    end + (difference * 100.0 / 200.0)
            in
                clamp 0.0 100.0 value
