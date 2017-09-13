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
    | InputContainer
    | ListItem
    | ListItemDivider
    | Menu



-- type MenuClasses
--     = ListItem
--
--
-- menuNamespace =
--     withNamespace "menu"
--


searchBoxNamespace =
    withNamespace "searchBox"
