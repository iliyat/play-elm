module Autocomplete exposing (..)

import Html exposing (Html, br, div, h1, input, text, Attribute, span, button)
import Html.Attributes exposing (placeholder, checked, type_, style)
import Views
import Dom
import Debug
import AutocompleteMenu as Menu exposing (Common)
import Mouse
import Task
import Data.Loan exposing (Loan)
import Table


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
    , loans : List Loan
    , tableState : Table.State
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
      , loans =
            [ Loan "1" "" "23.08.1992" "4012 273265" "+999 (312) 322-22-21" "333-872262" "Одобрено" "Comment text"
            , Loan "2" "" "26.08.1992" "4013 273266" "+999 (313) 322-22-21" "336-872262" "Одобрено" "Comment text"
            ]
      , tableState = Table.initialSort "name"
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
    | SetTableState Table.State


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
                newMenuModel oldModel =
                    ({ oldModel
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

        SetTableState newState ->
            ( { model
                | tableState = newState
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


searchConfig : Model -> Views.Config Msg
searchConfig model =
    { onSearchChange = SetSearchString
    , onTagDelete = OnTagDelete
    , onMenuClick = OnMenuClick
    , onCloseClick = OnCloseClick
    , onOpenSearchClick = OnOpenSearchClick
    , selectedTags = model.selectedTags
    , open = model.menuModel.open
    , searchAvailable = model.searchAvailable
    }


tableConfig : Table.Config Loan Msg
tableConfig =
    Table.config
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "" .avatar
            , Table.stringColumn "ФИО" .dateOfBirth
            , Table.stringColumn "Паспорт" .passport
            , Table.stringColumn "Моб. телефон" .phone
            , Table.stringColumn "Номер заявки" .loanNumber
            , Table.stringColumn "status" .status
            , Table.stringColumn "Комментарий" .comment
            ]
        }


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "1216px" ) ] ]
        [ Views.search (searchConfig model)
            (List.map (\e -> Views.ValueItem e.value e.label) model.menuTags)
        , br [] []
        , br [] []
        , Views.table tableConfig model.tableState model.loans 20
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "auto.css"
            ]
            []
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "style.css"
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
