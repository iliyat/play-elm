module Textfield
    exposing
        ( view
        , Model
        , defaultModel
        , Msg
        , update
        , Config
        , defaultConfig
        , TextfieldEvent
        , TextfieldEvent(..)
        , externalUpdate
        )

import Html exposing (Html, span, input, label, text, div, button, Attribute)
import Html.Attributes as Attr exposing (class, classList, style)
import Html.Events as Events
import Internal.Textfield exposing (Msg(..))
import Char
import Json.Decode as Json
import Utils exposing (..)


type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    }


defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    }


type alias Msg =
    Internal.Textfield.Msg


type TextfieldEvent
    = NoChange
    | Changed (Maybe String)


externalUpdate : Msg -> Model -> Config -> Maybe String -> ( Model, Maybe String )
externalUpdate msg model textfieldConfig previousText =
    let
        ( newTextfieldModel, _, textfieldEvent ) =
            update
                msg
                model
                textfieldConfig

        newText =
            case textfieldEvent of
                Changed newString ->
                    newString

                _ ->
                    previousText
    in
        ( newTextfieldModel, newText )


update : Msg -> Model -> Config -> ( Model, Cmd m, TextfieldEvent )
update msg model config =
    case msg of
        Input str ->
            let
                dirty =
                    str /= (config.defaultValue |> Maybe.withDefault "")

                allDigits =
                    String.all Char.isDigit str

                numberedValue =
                    if allDigits then
                        Just str
                    else
                        Nothing

                newValue =
                    if config.numbered then
                        numberedValue
                    else
                        Just str
            in
                ( { model | isDirty = dirty }, Cmd.none, Changed newValue )

        SetValue str ->
            let
                dirty =
                    case config.defaultValue of
                        Just a ->
                            a /= str

                        _ ->
                            True
            in
                { model | isDirty = dirty } ! []

        Blur ->
            { model | isFocused = False } ! []

        Focus ->
            { model | isFocused = True } ! []

        SubmitText ->
            model ! []

        NoOp ->
            model ! []


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
    , numbered : Bool
    , readonly : Bool
    , plural : Maybe Plural
    }


defaultConfig : Config
defaultConfig =
    { labelText = Nothing
    , labelFloat = False
    , value = Nothing
    , defaultValue = Nothing
    , disabled = False
    , asTitle = False
    , required = False
    , type_ = Just "text"
    , fullWidth = False
    , invalid = False
    , extra = Nothing
    , numbered = False
    , readonly = False
    , plural = Nothing
    }


view : Maybe String -> Model -> Config -> Html Msg
view value_ model config =
    let
        isFocused =
            model.isFocused && not config.disabled

        isDirty =
            model.isDirty || (config.defaultValue /= Nothing)

        labelBottom =
            if config.asTitle then
                "24px"
            else
                "8px"

        height =
            if config.asTitle then
                "56px"
            else
                "48px"

        fontSize =
            if config.asTitle then
                "34px"
            else
                "18px"

        value =
            value_
                |> Maybe.withDefault
                    (config.defaultValue
                        |> Maybe.withDefault ""
                    )

        extra =
            config.extra |> Maybe.withDefault ""

        intValue =
            String.toInt value |> Result.withDefault 0

        pl =
            Maybe.map (flip Utils.pluralize intValue) config.plural
                |> Maybe.withDefault ""

        inputHtml =
            input
                [ Attr.type_ "text"
                , style
                    [ ( "font-size", fontSize )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                , Events.onFocus <| Focus
                , Events.onBlur <| Blur
                , Events.onInput Input
                , Events.on "change" (Json.succeed SubmitText)
                , Attr.value value
                ]
                []

        divHtml =
            div
                [ style
                    [ ( "font-size", fontSize )
                    , ( "width", "168px" )
                    ]
                , classList
                    [ ( "mdc-textfield__input", True )
                    ]
                ]
                [ text value ]

        contentHtml =
            if config.readonly then
                divHtml
            else
                inputHtml
    in
        div
            [ classList
                [ ( "mdc-textfield mdc-textfield--upgraded", True )
                , ( "mdc-textfield--focused", isFocused )
                , ( "mdc-textfield--disabled", config.disabled )
                , ( "mdc-textfield--fullwidth", False )
                , ( "mdc-textfield--invalid", False )
                ]
            , Events.onFocus <| Focus
            , Events.onBlur <| Blur
            , style
                [ ( "height", height )
                , ( "width"
                  , if config.fullWidth then
                        "100%"
                    else
                        "initial"
                  )
                ]
            ]
            [ contentHtml
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
                [ text <| extra ++ pl ]
            , div [ class "mdc-textfield__bottom-line" ] []
            ]


(!) : Model -> List (Cmd m) -> ( Model, Cmd m, TextfieldEvent )
(!) m cs =
    ( m, Cmd.batch cs, NoChange )
