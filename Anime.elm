module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px, turn)
import Animation.Messenger
import Mouse
import Time


type alias Model =
    { style : Animation.Messenger.State Msg
    , opened : Bool
    }


type Msg
    = Open
    | Close
    | Click Mouse.Position
    | Animate Animation.Msg


type alias Styles =
    { open : List Animation.Property
    , closed : List Animation.Property
    }


styles : Styles
styles =
    { open =
        [ Animation.scale3d 0.1 0.3 1
        ]
    , closed =
        [ Animation.scale3d 1 1 1
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Open ->
            ( { model
                | opened = True
                , style =
                    Animation.interrupt
                        [ Animation.to styles.open ]
                        model.style
              }
            , Cmd.none
            )

        Close ->
            ( { model
                | opened = False
                , style =
                    Animation.interrupt
                        [ Animation.to styles.closed
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model
                | style = Animation.update animMsg model.style
              }
            , Cmd.none
            )

        Click pos ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Open ] [ text "Open" ]
        , button [ onClick Close ] [ text "close" ]
        , div
            [ class "container"
            , style [ ( "background-color", "red" ), ( "width", "20px" ) ]
            ]
            [ div
                (Animation.render model.style
                    ++ [ style
                            [ ( "background-color", "blue" )
                            , ( "width", "50px" )
                            , ( "height", "50px" )
                            ]
                       , class "menu"
                       ]
                )
                [ div [] []
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.opened == True then
            Mouse.clicks Click
          else
            Sub.none
        , Animation.subscription Animate [ model.style ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style
                [ Animation.scale3d 1 1 1
                ]
      , opened = False
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
