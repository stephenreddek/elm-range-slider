port module Stylesheets exposing (..)

import Css.File exposing (..)
import DefaultStyles


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "styles.css", compile [ DefaultStyles.css ] ) ]


main : Program Never () msg
main =
    Platform.program
        { init = ( (), files cssFiles )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
