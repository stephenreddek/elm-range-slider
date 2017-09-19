module Examples exposing (main)

import RangeSlider exposing (..)
import Html exposing (..)
import Date.Extra.Create as Date
import Date.Extra.Format as Date
import Date.Extra.Config.Config_en_us as DateConfig


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

        percentageTicks =
            List.map ((flip AxisTick) False << ((*) percentageTickStep) << toFloat) <| List.range (Basics.round <| minPercentage / percentageTickStep) (Basics.round <| maxPercentage / percentageTickStep)

        timeFormatter value =
            let
                hours =
                    (floor value) % 24

                minutes =
                    value
                        - (toFloat <| floor value)
                        |> ((*) 60)
                        |> round
            in
                Date.format DateConfig.config "%l:%M" (Date.timeFromFields hours minutes 0 0)

        timeAxisTicks =
            List.map (\v -> AxisTick (toFloat v) (v % 2 == 0)) <| List.range 0 24

        percentageSlider =
            RangeSlider.init
                |> (setStepSize <| Just 5.0)
                |> setFormatter (\value -> (toString value) ++ "%")
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
        PercentageSliderMsg msg ->
            let
                updatedModel =
                    RangeSlider.update msg percentageSlider
            in
                ( Model updatedModel timeSlider, Cmd.none )

        TimeSliderMsg msg ->
            let
                updatedModel =
                    RangeSlider.update msg timeSlider
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            (\model ->
                Sub.batch
                    [ Sub.map PercentageSliderMsg <| RangeSlider.subscriptions model.percentageSlider
                    , Sub.map TimeSliderMsg <| RangeSlider.subscriptions model.timeSlider
                    ]
            )
        }
