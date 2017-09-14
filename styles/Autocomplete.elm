module Autocomplete exposing (..)

import Html exposing (Html, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, checked, type_, style)
import Views
import Dom
import Debug
import Menu exposing (Common)
import Mouse
import Task


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
    , dataSource : List { label : String, value : String }
    , selectedTags : List { label : String, value : String }
    , menuTags : List { label : String, value : String }
    , menuModel : Menu.Model
    , searchAvailable : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { searchString = ""
      , menuModel = Menu.init
      , searchAvailable = False
      , selectedTags =
            []
      , menuTags =
            [ { label = "Boria", value = "1" }
            , { label = "Petia", value = "2" }
            , { label = "Vanya", value = "3" }
            ]
      , dataSource =
            [ { label = "Boria", value = "1" }
            , { label = "Petia", value = "2" }
            , { label = "Vanya", value = "3" }
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetSearchString Common
    | OnMenuClick String
    | OnTagDelete { value : String, label : String }
    | OnCloseClick
    | OnOpenSearchClick
    | Click Mouse.Position
    | FocusResult (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCloseClick ->
            ( { model
                | searchAvailable = False
                , menuTags = model.dataSource
                , selectedTags = []
              }
            , Cmd.none
            )

        OnOpenSearchClick ->
            ( { model | searchAvailable = True }
            , Dom.focus "input-search"
                |> Task.attempt FocusResult
            )

        FocusResult result ->
            ( model, Cmd.none )

        OnMenuClick value ->
            let
                found =
                    List.filter (\tag -> value == tag.value) model.dataSource

                selectedTags =
                    List.append model.selectedTags found

                newMenuModel mdl =
                    ({ mdl | open = False })
            in
                ( { model
                    | selectedTags = selectedTags
                    , menuTags = List.filter (not << flip List.member selectedTags) model.dataSource
                    , menuModel = newMenuModel model.menuModel
                  }
                , Dom.focus "input-search" |> Task.attempt FocusResult
                )

        OnTagDelete tag ->
            let
                filterFunc v tag =
                    tag.value /= v

                selectedTags =
                    List.filter (filterFunc tag.value) model.selectedTags
            in
                ( { model
                    | selectedTags = selectedTags
                    , menuTags = List.filter (not << flip List.member selectedTags) model.dataSource
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


config : Model -> Views.Config Msg
config model =
    { onSearchChange = SetSearchString
    , onTagDelete = OnTagDelete
    , onMenuClick = OnMenuClick
    , onCloseClick = OnCloseClick
    , onOpenSearchClick = OnOpenSearchClick
    , selectedTags = model.selectedTags
    , open = model.menuModel.open
    , searchAvailable = model.searchAvailable
    }


view : Model -> Html Msg
view model =
    div []
        [ Views.search (config model)
            (List.map
                (\e ->
                    Views.ValueItem e.value
                        e.label
                )
                model.menuTags
            )
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
