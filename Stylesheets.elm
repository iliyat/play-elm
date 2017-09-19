port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import SliderCss


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "slider.css", Css.File.compile [ SliderCss.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
