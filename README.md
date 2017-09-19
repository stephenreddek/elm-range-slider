# elm-range-slider

```shell
elm package install stephenreddek/elm-range-slider
```

## Usage

The `RangeSlider.init` function creates a default range slider which handles continuous values from 0 to 100. You can override any of the default settings by using the "setters" on the initialized model.
```elm
percentageSlider =
    RangeSlider.init
        |> (setStepSize <| Just 5.0)
        |> setFormatter (\value -> (toString value) ++ "%")
        |> setExtents -25.0 25.0
        |> setValues -10.0 10.0
```

Because it uses mouse movements, the range slider requires subscriptions. After initialization, handle the subscriptions.
```elm
subscriptions =
    (\model ->
        Sub.map SliderMsg <| RangeSlider.subscriptions model.slider
    )
```

Handle the updates from the subscription in your update function
```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg { slider } =
    case msg of
        SliderMsg msg ->
            let
                updatedModel =
                    RangeSlider.update msg slider
            in
                ( Model updatedModel, Cmd.none )
```

To view the slider, simply call the view function
```elm
Html.map SliderMsg <| RangeSlider.view slider
```

When they're finished inputting values, access the values with the `getValues` accessor
```elm
(from, to) =
    RangeSlider.getValues model.slider
```

## Example

Checkout the [example](https://github.com/stephenreddek/elm-range-slider/tree/master/example "elm-range-slider example") to test it or see how to wire it up.

## Css

The default styles for the range slider can be found [in the project repo](https://github.com/stephenreddek/elm-range-slider/tree/master/css "elm-range-slider Github"). You can use the CssHooks to generate your own CSS with elm-css.
