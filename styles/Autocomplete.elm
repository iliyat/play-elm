module Autocomplete exposing (..)

import Html exposing (Html, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, checked, type_, style)
import Views
import Debug
import Menu exposing (Common)
import Mouse


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { searchString : String
    , tags : List { label : String, value : String }
    , menuModel : Menu.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { searchString = ""
      , menuModel = Menu.init
      , tags =
            [ { label = "ololo", value = "test" }
            , { label = "12", value = "18" }
            , { label = "13", value = "19" }
            , { label = "13", value = "20" }
            , { label = "14", value = "23" }
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetSearchString Common
    | OnMenuClick String
    | OnTagDelete String
    | Click Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMenuClick itemValue ->
            ( model, Cmd.none )

        OnTagDelete value ->
            let
                filterFunc v tag =
                    tag.value /= v
            in
                ( { model
                    | tags = List.filter (filterFunc value) model.tags
                  }
                , Cmd.none
                )

        SetSearchString { geometry, value } ->
            let
                newMenuModel mdl =
                    ({ mdl
                        | top = geometry.button.bounds.top
                        , left = geometry.button.bounds.left - 170 + geometry.button.bounds.width
                        , geometry = Just geometry
                        , open =
                            if String.length value > 0 then
                                True
                            else
                                False
                     }
                    )
            in
                ( { model
                    | menuModel = newMenuModel model.menuModel
                    , searchString = value
                  }
                , Cmd.none
                )

        Click pos ->
            case model.menuModel.geometry of
                Just geom ->
                    let
                        menuModel =
                            Menu.mouseClick pos model.menuModel geom

                        closed =
                            not menuModel.open
                    in
                        if closed then
                            ( { model | menuModel = menuModel }, Cmd.none )
                        else
                            ( { model
                                | menuModel = menuModel
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


entries =
    [ Views.ValueItem "1" "Vasya"
    , Views.ValueItem "2" "Petya"
    ]


config : Model -> Views.Config Msg
config model =
    { onSearchChange = SetSearchString
    , onTagDelete = OnTagDelete
    , onMenuClick = OnMenuClick
    , tags = model.tags
    , open = model.menuModel.open
    }


view : Model -> Html Msg
view model =
    div []
        [ Views.search (config model) entries
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
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500" ]
            []
        ]



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Menu.subscriptions model.menuModel Click
        ]
