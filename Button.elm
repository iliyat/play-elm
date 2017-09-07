port module Button exposing (view, renderButton)

import Html
    exposing
        ( Html
        , div
        , Attribute
        , span
        , button
        , text
        , i
        )
import Html.Attributes exposing (classList, class, style)
import Html.Events
import Icons.MoreVert as MoreVert


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { query : String, isOpen : Bool }


init : ( Model, Cmd Msg )
init =
    ( { query = "", isOpen = False }, Cmd.none )



-- UPDATE


type Msg
    = ToggleClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleClick ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )


{-| Represents a button.
-}



-- button : List (Attribute msg) -> List (Html msg) -> Html msg
-- button =
--   node "button"


renderButton : List (Attribute msg) -> Html msg
renderButton a =
    i
        ([ class "mdc-icon-toggle material-icons mdc-custom-icon"
         , style
            [ ( "width", "24px" )
            , ( "height", "24px" )
            , ( "padding", "10px 0px" )
            , ( "font-size", "24px" )
            , ( "line-height", "3px" )
            ]
         ]
            ++ a
        )
        [ text "more_vert" ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [] []
            , renderButton []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "style.css?v1"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            ]
        ]
