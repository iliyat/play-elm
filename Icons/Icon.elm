module Icons.Icon exposing (view)

import Html exposing (i, text, Html, Attribute)
import Html.Attributes exposing (class, style)


view : String -> Html msg
view name =
    i
        [ class "material-icons"
        , style
            [ ( "font-size", "24px" )
            , ( "cursor", "pointer" )
            , ( "color", "rgba(0,0,0,.54)" )
            , ( "box-sizing", "border-box" )
            ]
        ]
        [ text name ]
