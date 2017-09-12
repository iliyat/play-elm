module Autocomplete exposing (..)

import Html exposing (Html, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, checked, type_, style)
import Views
import Debug


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { query : String
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "" }, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String
    | OnMenuClick String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMenuClick itemValue ->
            ( model, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )



-- VIEW


entries =
    [ Views.ValueItem "1" "Vasya"
    , Views.Divider
    , Views.ValueItem "2" "Petya"
    ]


view : Model -> Html Msg
view model =
    div []
        [ Views.search
        , Views.menu entries OnMenuClick
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "auto.css"
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



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
