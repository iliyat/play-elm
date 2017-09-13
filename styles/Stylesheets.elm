port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import MyCss
import Ui.ChipCss as Chip


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "auto.css", Css.File.compile [ MyCss.css, Chip.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
