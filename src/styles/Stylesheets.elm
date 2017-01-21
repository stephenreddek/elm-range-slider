port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram, toFileStructure, compile)
import DefaultStyles


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "styles.css", compile [ DefaultStyles.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
