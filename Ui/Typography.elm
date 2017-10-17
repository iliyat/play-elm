module Ui.Typography exposing (headline, pad12, pad24)

import Ui.Options as Options exposing (Property, styled, cs, css)


headline : Property c m
headline =
    cs "header"


pad12 : Property c m
pad12 =
    css "padding-bottom" "12px"


pad24 : Property c m
pad24 =
    css "padding-bottom" "24px"
