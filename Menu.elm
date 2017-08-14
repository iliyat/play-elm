port module Menu exposing (view)

import Html exposing (Html, div, h1, input, text, li, ul, Attribute, span)
import Html.Attributes exposing (classList, class)
import Html.Events exposing (on, onInput, onClick, onWithOptions, onMouseUp)
import Json.Decode exposing (Decoder, succeed, at, string, map)
import Debug


main =
    Html.program
        { init = init ""
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { query : String }


init : String -> ( Model, Cmd Msg )
init st =
    ( { query = "" }, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        MouseUp ->
            ( model, Cmd.none )



-- UPDATE


view_ : Bool -> Html.Html msg
view_ isOpen =
    div
        [ classList
            [ ( "mdc-simple-menu", True )
            , ( "mdc-simple-menu--open", isOpen )
            ]
        ]
        [ ul [ class "mdc-simple-menu__items mdc-list" ]
            [ li [ class "mdc-list-item" ] [ text "Back" ]
            , li [ class "mdc-list-item" ] [ text "Back" ]
            ]
        ]


targetValue : Decoder String
targetValue =
    at [ "target", "value" ] string


handleMouseUp message =
    on "mouseup" (succeed message) |> Debug.log "test"


view : Model -> Html Msg
view model =
    div [ handleMouseUp MouseUp ]
        [ view_ True
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css"
            ]
            []
        ]
