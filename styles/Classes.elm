module Classes exposing (..)

import Html.CssHelpers exposing (withNamespace)


type CssClasses
    = Container
    | Search
    | SearchBlock
    | SearchIcon
    | TagsBlock
    | Block
    | Input
    | ListItem
    | ListItemDivider



-- type MenuClasses
--     = ListItem
--
--
-- menuNamespace =
--     withNamespace "menu"
--


searchBoxNamespace =
    withNamespace "searchBox"
