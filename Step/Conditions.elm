module Step.Conditions exposing (view, Model, Msg, init, update)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs exposing (style)
import Ui.Options as Options exposing (styled, cs, css)
import Ui.Typography as Typography
import Ui.Textfield as Textfield
import Ui.Button as Button
import Ui.Elevation as Elevation
import Ui.SliderWithTextfield as SliderWithTextfield


-- ID                              string `gorm:"type:uuid;primary_key;"`
-- ExternalId                      string `gorm:"type:uuid;index;"`
-- NumberPrefix                    uint32 `gorm:"index"`
-- Number                          uint32 `gorm:"index"`
-- RequestAt                       time.Time
-- RequestBy                       *string
-- RequestAmount                   uint64
-- RequestDaysCount                uint32
-- RequestPurpose                  string
-- RequestModelCalculatingDailyId  string `gorm:"type:uuid;index;"`
-- RequestPromoCode                string
-- IsActual                        bool
-- Status                          int32
-- ApprovedAt                      *time.Time
-- ApprovedBy                      *string `gorm:"type:uuid;index;"`
-- ApprovedAmount                  *uint64
-- ApprovedDaysCount               *uint32
-- ApprovedModelCalculatingDailyId *string `gorm:"type:uuid;index;"`
-- TimeZone                        *string
-- RejectReason                    *string
-- RiskClientType                  bool
-- RiskCountry                     bool
-- RiskClientOperations            bool
-- RiskUpdatedAt                   *time.Time
-- CreatedAt                       time.Time
-- UpdatedAt                       time.Time
-- DeletedAt                       *time.Time
-- MODEL --


type alias Model =
    { errors : List String
    , buttonModel : Button.Model
    , showConditionsBlock : Bool
    , sumSliderModel : SliderWithTextfield.Model
    , periodSliderModel : SliderWithTextfield.Model
    }


init : Model
init =
    { errors = []
    , buttonModel = Button.defaultModel
    , showConditionsBlock = False
    , sumSliderModel = SliderWithTextfield.defaultModel
    , periodSliderModel = SliderWithTextfield.defaultModel
    }


type Msg
    = ChangeConditionsClick
    | Ripple Button.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeConditionsClick ->
            ({ model | showConditionsBlock = True }) ! []

        Ripple msg_ ->
            let
                ( new, effects ) =
                    Button.update msg_ model.buttonModel
            in
                ( { model | buttonModel = new }, effects |> Cmd.map Ripple )


view : Model -> Html Msg
view model =
    let
        tfConfig =
            Textfield.defaultConfig

        tfModel =
            Textfield.defaultModel

        amountConfig =
            { tfConfig
                | labelText = Just "Одобрено"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        daysConfig =
            { tfConfig
                | labelText = Just "Период"
                , asTitle = True
                , readonly = True
            }

        payAt =
            { tfConfig
                | labelText = Just "К возврату"
                , asTitle = True
                , readonly = True
                , fullWidth = True
            }

        returnConfig =
            { tfConfig
                | labelText = Just "К возврату"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        percentConfig =
            { tfConfig
                | labelText = Just "В т.ч. процент по займу"
                , extraInside = Just "₽"
                , asTitle = True
                , readonly = True
            }

        promoConfig =
            { tfConfig
                | labelText = Just "Акция"
                , asTitle = True
                , readonly = True
            }

        field value config =
            Textfield.viewReadonly (Just value) tfModel config |> Html.map never
    in
        div []
            [ styled div
                [ Elevation.z1, cs "block" ]
                [ styled div
                    [ css "display" "flex"
                    , css "justify-content"
                        "space-between"
                    ]
                    [ styled div [ Typography.headline, css "padding-bottom" "24px" ] [ text "Заявка №435-84313" ]
                    , Button.view Ripple
                        model.buttonModel
                        [ Button.ripple
                        , Button.primary
                        , Options.onClick ChangeConditionsClick
                        ]
                        [ text "Изменить условия" ]
                    ]
                , div [ Attrs.class "ui-flex" ]
                    [ field "10000" amountConfig
                    , field "14 дней" daysConfig
                    , styled div
                        [ css "width" "300px" ]
                        [ field "12 сентября 2017" payAt
                        ]
                    , field "13 212" returnConfig
                    , field "13 212" percentConfig
                    , Textfield.viewReadonly (Just "lastchance") tfModel promoConfig
                        |> Html.map never
                    ]
                ]
            , styled div
                [ Elevation.z1
                , cs "block"
                , css "margin-top" "24px"
                , css "display" "none"
                    |> Options.when
                        (not
                            model.showConditionsBlock
                        )
                ]
                [ styled div
                    [ Typography.headline
                    ]
                    [ text "Изменение условий по заявке" ]
                ]
            ]
