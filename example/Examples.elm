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
    { percentageSlider : RangeSlider.Model
    , timeSlider : RangeSlider.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        percentageSettings =
            { stepSize = Just 10.0
            , formatter = Just (\value -> (toString value) ++ "%")
            , from = Just -10.0
            , to = Just 10.0
            , min = Just -50.0
            , max = Just 50.0
            , height = Nothing
            , width = Nothing
            }

        timeSettings =
            { stepSize = Just 2.0
            , formatter = Just (\value -> Date.format DateConfig.config "%H:%M" (Date.timeFromFields (round value) 0 0 0))
            , from = Just 8.0
            , to = Just 12.0
            , min = Just 0.0
            , max = Just 24.0
            , height = Nothing
            , width = Nothing
            }

        ( initialPercentageModel, initialPercentageCmd ) =
            RangeSlider.activate percentageSettings

        ( initialTimeModel, initialTimeCmd ) =
            RangeSlider.activate timeSettings
    in
        ( Model initialPercentageModel initialTimeModel
        , Cmd.batch [ Cmd.map PercentageSliderMsg initialPercentageCmd, Cmd.map TimeSliderMsg initialTimeCmd ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ percentageSlider, timeSlider } as model) =
    case msg of
        PercentageSliderMsg msg ->
            let
                ( updatedModel, cmd ) =
                    RangeSlider.update percentageSlider msg
            in
                ( Model updatedModel timeSlider, Cmd.map PercentageSliderMsg cmd )

        TimeSliderMsg msg ->
            let
                ( updatedModel, cmd ) =
                    RangeSlider.update timeSlider msg
            in
                ( Model percentageSlider updatedModel, Cmd.map TimeSliderMsg cmd )


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
