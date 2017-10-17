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
import Html.Attributes as Attrs exposing (style)
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography
import Step.Conditions.Info as ConditionsInfo
import Ui.Elevation as Elevation
import Ui.Checkbox as Checkbox
import Ui.Button as Button
import Icons.Icon as Icon


type Msg
    = ToggleCheckbox1
    | ToggleCheckbox2
    | Checkbox1Msg Checkbox.Msg
    | Checkbox2Msg Checkbox.Msg
    | ButtonMsg Button.Msg


type alias Model =
    { checkbox1 : Checkbox.Model
    , checkbox2 : Checkbox.Model
    , checkbox1Clicked : Bool
    , checkbox2Clicked : Bool
    , buttonModel : Button.Model
    }


defaultModel : Model
defaultModel =
    { checkbox1 = Checkbox.defaultModel
    , checkbox2 = Checkbox.defaultModel
    , checkbox1Clicked = False
    , checkbox2Clicked = True
    , buttonModel = Button.defaultModel
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

        ButtonMsg msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map ButtonMsg )


view : Model -> Html Msg
view model =
    let
        iconStyle =
            [ ( "color", "#fff" )
            , ( "position", "relative" )
            , ( "top", "6px" )
            , ( "right", "8px" )
            ]
    in
        div []
            [ styled div
                [ Elevation.z1, cs "block" ]
                [ styled div
                    [ css "display" "flex"
                    , css "justify-content" "space-between"
                    ]
                    [ styled div
                        [ Typography.headline, Typography.pad12 ]
                        [ text "Печать документов"
                        ]
                    , Button.view ButtonMsg
                        model.buttonModel
                        [ Button.ripple
                        , Button.raised
                        , Button.primary
                        , css "width" "120px"
                        ]
                        [ Icon.view "print" [ style iconStyle ], text "Печать" ]
                    ]
                , ConditionsInfo.view |> Html.map never
                ]
            , styled div
                [ Elevation.z1, cs "block", css "margin-top" "24px" ]
                [ styled div [ Typography.headline, Typography.pad12 ] [ text "Проверка подписей" ]
                , styled div
                    [ css "position" "relative", css "left" "-8px" ]
                    [ Checkbox.view Checkbox1Msg
                        model.checkbox1
                        [ Options.onClick ToggleCheckbox1
                        , Checkbox.checked |> Options.when model.checkbox1Clicked
                        ]
                        [ styled label
                            [ Options.onClick ToggleCheckbox1
                            , css "user-select" "none"
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
                        [ styled label
                            [ Options.onClick ToggleCheckbox2
                            , css "user-select" "none"
                            ]
                            [ text "Заявление-согласие на обработку данных" ]
                        ]
                    ]
                ]
            ]
