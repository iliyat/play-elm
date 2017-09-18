module Textfield exposing (view, Model, defaultModel, Msg, update)

import Html exposing (Html, span, input, label, text, div, button, Attribute)
import Html.Attributes as Attr exposing (class, classList, style)
import Html.Events as Events
import Json.Decode as Json exposing (Decoder)
import Internal.Textfield exposing (Msg(..))


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    , value : Maybe String
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    , value = Nothing
    }


type alias Msg =
    Internal.Textfield.Msg


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        Input str ->
            let
                dirty =
                    str /= ""
            in
                ( { model | value = Just str, isDirty = dirty }, Cmd.none )

        Blur ->
            ( { model | isFocused = False }, Cmd.none )

        Focus ->
            ( { model | isFocused = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type alias Config =
    { labelText : Maybe String
    , labelFloat : Bool
    , value : Maybe String
    , defaultValue : Maybe String
    , disabled : Bool
    , asTitle : Bool
    , required : Bool
    , type_ : Maybe String
    , fullWidth : Bool
    , invalid : Bool
    , extra : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { labelText = Just "Сумма"
    , labelFloat = False
    , value = Nothing
    , defaultValue = Nothing
    , disabled = False
    , asTitle = True
    , required = False
    , type_ = Just "text"
    , fullWidth = False
    , invalid = False
    , extra = Just "₽"
    }


view : (Msg -> m) -> Model -> Html m
view lift model =
    let
        config =
            defaultConfig

        isFocused =
            model.isFocused && not config.disabled

        isDirty =
            model.isDirty

        labelBottom =
            if config.asTitle then
                "24px"
            else
                "8px"

        height =
            if config.asTitle then
                "68px"
            else
                "48px"

        fontSize =
            if config.asTitle then
                "34px"
            else
                "18px"
    in
        div
            [ classList
                [ ( "mdc-textfield mdc-textfield--upgraded", True )
                , ( "mdc-textfield--focused", isFocused )
                , ( "mdc-textfield--disabled", config.disabled )
                , ( "mdc-textfield--fullwidth", config.fullWidth )
                , ( "mdc-textfield--invalid", False )
                ]
            , Events.onFocus <| lift Focus
            , Events.onBlur <| lift Blur
            , style
                [ ( "height", height )
                ]
            ]
            [ input
                [ Attr.type_ "text"
                , style
                    [ ( "font-size", fontSize )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                , Events.onFocus <| lift Focus
                , Events.onBlur <| lift Blur
                , Events.onInput (Input >> lift)
                , Attr.value (Maybe.withDefault "" model.value)
                ]
                []
            , label
                [ classList
                    [ ( "mdc-textfield__label mdc-typography", True )
                    , ( "mdc-textfield__label--float-above", isFocused || isDirty )
                    ]
                , style
                    [ ( "bottom", labelBottom )
                    ]
                ]
                (case config.labelText of
                    Just str ->
                        [ text str ]

                    Nothing ->
                        []
                )
            , span
                [ style
                    [ ( "float", "right" )
                    , ( "position", "absolute" )
                    , ( "right", "0" )
                    , ( "bottom", "10px" )
                    , ( "height", "24px" )
                    , ( "font-family", "Roboto" )
                    , ( "font-size", "34px" )
                    , ( "line-height", "15px" )
                    , ( "color", "rgba(0, 0, 0, 0.38)" )
                    ]
                ]
                [ text "дней" ]
            , div [ class "mdc-textfield__bottom-line" ] []
            ]
