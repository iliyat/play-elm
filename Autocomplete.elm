module Autocomplete exposing (..)

import Html exposing (Html, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, class, checked, type_, style)
import Html.Events exposing (onInput, onClick, onWithOptions)


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
    let
        model =
            { query = ""
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "700px" ) ] ]
        [ div []
            [ input [ placeholder "Search by Name", onInput SetQuery ] []
            ]
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "autocomplete.css"
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
