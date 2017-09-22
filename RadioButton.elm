module RadioButton
    exposing
        ( -- VIEWW
          view
        , Property
        , disabled
        , selected
        , name
        , Msg
          -- TEA
        , Model
        , defaultModel
        , update
          -- RENDER
        )

{-| The MDC Radio Button component provides a radio button adhering to the Material
Design Specification. It requires no Javascript out of the box, but can be
enhanced with Javascript to provide better interaction UX as well as a
component-level API for state modification.


## Design & API Documentation

  - [Material Design guidelines: Selection Controls – Radio buttons](https://material.io/guidelines/components/selection-controls.html#selection-controls-radio-button)
  - [Demo](https://aforemny.github.io/elm-mdc/#radio-buttons)


## View

@docs view


## Properties

@docs Property, disabled, selected


## TEA architecture

@docs Model, defaultModel, Msg, update


## Featured render

@docs render
@docs Store, react

-}

import Html.Attributes as Html
import Html exposing (Html, text, div)
import Json.Decode as Json
import Internal.Helpers as Helpers exposing (map1st, map2nd, blurOn, filter, noAttr)
import Internal.Options as Internal
import Internal.RadioButton exposing (Msg(..))


-- import Msg exposing (Index)

import Options as Options exposing (Style, cs, styled, many, when, maybe)
import Ripple as Ripple


-- MODEL


type alias Model =
    { ripple : Ripple.Model
    , isFocused : Bool
    }


defaultModel : Model
defaultModel =
    { ripple = Ripple.defaultModel
    , isFocused = False
    }



-- ACTION, UPDATE


type alias Msg =
    Internal.RadioButton.Msg


update : (Msg -> m) -> Msg -> Model -> ( Maybe Model, Cmd m )
update lift msg model =
    case msg of
        RippleMsg msg_ ->
            let
                ( ripple, effects ) =
                    Ripple.update msg_ model.ripple
            in
                ( Just { model | ripple = ripple }
                , Cmd.map (RippleMsg >> lift) effects
                )

        SetFocus focus ->
            ( Just { model | isFocused = focus }, Cmd.none )

        NoOp ->
            ( Nothing, Cmd.none )



-- OPTIONS


type alias Config m =
    { input : List (Options.Style m)
    , container : List (Options.Style m)
    , value : Bool
    }


defaultConfig : Config m
defaultConfig =
    { input = []
    , container = []
    , value = False
    }


{-| TODO
-}
type alias Property m =
    Options.Property (Config m) m


{-| TODO
-}
disabled : Property m
disabled =
    Options.many
        [ cs "mdc-radio--disabled"
        , Internal.input
            [ Internal.attribute <| Html.disabled True
            ]
        ]


{-| TODO
-}
selected : Property m
selected =
    Internal.option (\config -> { config | value = True })


{-| TODO
-}
name : String -> Property m
name value =
    Internal.attribute (Html.name value)



-- VIEW


view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options ch =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig options

        ( rippleOptions, rippleStyle ) =
            Ripple.view True (RippleMsg >> lift) model.ripple [] []
    in
        styled div
            [ cs "mdc-form-field" ]
            ([ Internal.applyContainer summary
                div
                [ cs "mdc-radio"
                , Internal.attribute <| blurOn "mouseup"
                , rippleOptions
                ]
                [ Internal.applyInput summary
                    Html.input
                    [ cs "mdc-radio__native-control"
                    , Internal.attribute <| Html.type_ "radio"
                    , Internal.attribute <| Html.checked config.value
                    , Internal.on1 "focus" lift (SetFocus True)
                    , Internal.on1 "blur" lift (SetFocus False)
                    , Options.onWithOptions "click"
                        { preventDefault = True
                        , stopPropagation = False
                        }
                        (Json.succeed (lift NoOp))
                    ]
                    []
                , styled Html.div
                    [ cs "mdc-radio__background"
                    ]
                    [ styled Html.div [ cs "mdc-radio__inner-circle" ] []
                    , styled Html.div [ cs "mdc-radio__outer-circle" ] []
                    ]
                , rippleStyle
                ]
             ]
                ++ ch
            )
