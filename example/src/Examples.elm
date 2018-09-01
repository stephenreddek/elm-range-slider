module Examples exposing (main)

import Browser
import Html exposing (..)
import RangeSlider exposing (..)


type Msg
    = PercentageSliderMsg RangeSlider.Msg
    | TimeSliderMsg RangeSlider.Msg


type alias Model =
    { percentageSlider : RangeSlider.RangeSlider
    , timeSlider : RangeSlider.RangeSlider
    }


init : ( Model, Cmd Msg )
init =
    let
        minPercentage =
            -25.0

        maxPercentage =
            25.0

        percentageTickStep =
            5

        maxPercentageTick =
            Basics.round (maxPercentage / percentageTickStep)

        minPercentageTick =
            Basics.round (minPercentage / percentageTickStep)

        percentageTicks =
            List.range minPercentageTick maxPercentageTick
                |> List.map (\v -> { value = toFloat v * percentageTickStep, isLabeled = False })

        timeFormatter value =
            let
                hours =
                    modBy 24 (floor value)

                minutes =
                    value
                        - toFloat (floor value)
                        |> (*) 60
                        |> round
            in
            String.fromInt hours ++ ":" ++ String.padLeft 2 '0' (String.fromInt minutes)

        timeAxisTicks =
            List.range 0 24
                |> List.map (\v -> AxisTick (toFloat v) (modBy 2 v == 0))

        percentageSlider =
            RangeSlider.init
                |> (setStepSize <| Just 5.0)
                |> setFormatter (\value -> String.fromFloat value ++ "%")
                |> setExtents minPercentage maxPercentage
                |> setValues -10.0 10.0
                |> setAxisTicks percentageTicks

        timeSlider =
            RangeSlider.init
                |> (setStepSize <| Just 0.5)
                |> setFormatter timeFormatter
                |> setExtents 0.0 24.0
                |> setValues 8.0 12.0
                |> setDimensions 400 75
                |> setAxisTicks timeAxisTicks
    in
    ( Model percentageSlider timeSlider
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ percentageSlider, timeSlider } as model) =
    case msg of
        PercentageSliderMsg m ->
            let
                updatedModel =
                    RangeSlider.update m percentageSlider
            in
            ( Model updatedModel timeSlider, Cmd.none )

        TimeSliderMsg m ->
            let
                updatedModel =
                    RangeSlider.update m timeSlider
            in
            ( Model percentageSlider updatedModel, Cmd.none )


view : Model -> Html Msg
view { percentageSlider, timeSlider } =
    div []
        [ div []
            [ h1 []
                [ text "A range slider with percentages" ]
            , div []
                [ Html.map PercentageSliderMsg <| RangeSlider.view percentageSlider ]
            ]
        , div []
            [ h1 []
                [ text "A range slider with times" ]
            , div []
                [ Html.map TimeSliderMsg <| RangeSlider.view timeSlider ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map PercentageSliderMsg <| RangeSlider.subscriptions model.percentageSlider
                    , Sub.map TimeSliderMsg <| RangeSlider.subscriptions model.timeSlider
                    ]
        }
