module Simple exposing (main)

import RangeSlider exposing (..)
import Html exposing (..)


type Msg
    = SliderMsg RangeSlider.Msg


type alias Model =
    { slider : RangeSlider.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        settings =
            { stepSize = Just 10.0
            , formatter = Just (\value -> (toString value) ++ "%")
            , from = Just -10.0
            , to = Just 10.0
            , min = Just -50.0
            , max = Just 50.0
            , height = Nothing
            , width = Nothing
            }

        ( initialModel, initialCmd ) =
            RangeSlider.activate settings
    in
        ( Model initialModel, Cmd.map SliderMsg initialCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ slider } as model) =
    case msg of
        SliderMsg msg ->
            let
                ( updatedModel, cmd ) =
                    RangeSlider.update slider msg
            in
                ( Model updatedModel, Cmd.map SliderMsg cmd )


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "A simple example of a range slider" ]
        , div []
            [ Html.map SliderMsg <| RangeSlider.view model.slider ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.map SliderMsg <| RangeSlider.subscriptions model.slider)
        }
