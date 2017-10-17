module Ui.Typography exposing (headline, pad12)

import Ui.Options as Options exposing (Property, styled, cs, css)


headline : Property c m
headline =
    cs "header"


pad12 : Property c m
pad12 =
    css "padding-bottom" "12px"
