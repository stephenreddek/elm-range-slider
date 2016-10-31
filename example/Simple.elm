module Simple exposing (main)

import RangeSlider exposing (..)
import Html exposing (..)
import Html.App as Html


type Msg
    = NoOp


type alias Model =
    { slider : RangeSlider.Model
    }


init : ( Model, Cmd Msg )
init =
    ( RangeSlider.initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ slider } as model) =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "A simple example of a range slider" ]
        ]


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
