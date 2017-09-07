port module Button exposing (view)

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



-- VIEW
-- but : Html Msg
-- but =
--     button [ class "mdc-button" ] [ MoreVert.view "" ]


but : Html Msg
but =
    i
        [ class "mdc-icon-toggle material-icons mdc-custom-icon"
        , style
            [ ( "width", "24px" )
            , ( "height", "24px" )
            , ( "padding", "10px 0px" )
            , ( "font-size", "24px" )
            , ( "line-height", "3px" )
            ]
        ]
        [ text "more_vert" ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [] []
            , but
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
