module Step.PrintDocuments
    exposing
        ( view
        , update
        , Msg
        , Msg(..)
        , Model
        , defaultModel
        )

import Html exposing (Html, div, h1, text, p, span, label)
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography
import Step.Conditions.Info as ConditionsInfo
import Ui.Elevation as Elevation
import Ui.Checkbox as Checkbox
import Html.Events as Events


type Msg
    = ToggleCheckbox1
    | ToggleCheckbox2
    | Checkbox1Msg Checkbox.Msg
    | Checkbox2Msg Checkbox.Msg


type alias Model =
    { checkbox1 : Checkbox.Model
    , checkbox2 : Checkbox.Model
    , checkbox1Clicked : Bool
    , checkbox2Clicked : Bool
    }


defaultModel : Model
defaultModel =
    { checkbox1 = Checkbox.defaultModel
    , checkbox2 = Checkbox.defaultModel
    , checkbox1Clicked = False
    , checkbox2Clicked = True
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Checkbox1Msg msg_ ->
            let
                ( new, effects ) =
                    Checkbox.update Checkbox1Msg msg_ model.checkbox1

                r =
                    case new of
                        Nothing ->
                            model.checkbox1

                        Just a ->
                            a
            in
                ( { model | checkbox1 = r }, effects )

        Checkbox2Msg msg_ ->
            let
                ( new, effects ) =
                    Checkbox.update Checkbox2Msg msg_ model.checkbox2

                r =
                    case new of
                        Nothing ->
                            model.checkbox2

                        Just a ->
                            a
            in
                ( { model | checkbox2 = r }, effects )

        ToggleCheckbox1 ->
            ({ model | checkbox1Clicked = not model.checkbox1Clicked }) ! []

        ToggleCheckbox2 ->
            ({ model | checkbox2Clicked = not model.checkbox2Clicked }) ! []


view : Model -> Html Msg
view model =
    div []
        [ styled div
            [ Elevation.z1, cs "block" ]
            [ styled div [ Typography.headline ] [ text "Печать документов" ]
            , ConditionsInfo.view |> Html.map never
            ]
        , styled div
            [ Elevation.z1, cs "block", css "margin-top" "24px" ]
            [ styled div [ Typography.headline ] [ text "Проверка подписей" ]
            , div []
                [ Checkbox.view Checkbox1Msg
                    model.checkbox1
                    [ Options.onClick ToggleCheckbox1
                    , Checkbox.checked |> Options.when model.checkbox1Clicked
                    ]
                    [ label
                        [ Events.onClick ToggleCheckbox1
                        ]
                        [ text "Договор" ]
                    ]
                , Checkbox.view Checkbox2Msg
                    model.checkbox2
                    [ Options.onClick ToggleCheckbox2
                    , Checkbox.checked |> Options.when model.checkbox2Clicked
                    , Checkbox.invalid
                        |> Options.when
                            (not
                                model.checkbox2Clicked
                            )
                    ]
                    [ label
                        [ Events.onClick ToggleCheckbox2
                        ]
                        [ text "Заявление-согласие на обработку данных" ]
                    ]
                ]
            ]
        ]
