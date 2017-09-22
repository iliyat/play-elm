module Typography exposing (headline)

import Options as Options exposing (Property, styled, cs, css)


headline : Property c m
headline =
    cs "header"
