module Views.Stepper exposing (step, stepLine)

import Html exposing (Html, div, h1, text, p, span)
import Html.Attributes as Attrs
import Svg
import Svg.Attributes as Svg


step : String -> Int -> Bool -> Html msg
step title num enabled =
    let
        class =
            mkClass "stepper--"

        classList =
            mkClassList "stepper--"

        circleColor =
            if enabled then
                "#009ce1"
            else
                "rgb(158, 158, 158)"
    in
        div
            [ classList
                [ ( "step", True ), ( "step--disabled", enabled ) ]
            ]
            [ span [ class "circle-wrap" ]
                [ span [ class "circle-pad" ]
                    [ Svg.svg
                        [ Svg.class "ci"
                        , Svg.width "21"
                        , Svg.height "21"
                        ]
                        [ Svg.circle
                            [ Svg.cx "12"
                            , Svg.cy "12"
                            , Svg.r "10"
                            , Svg.fill circleColor
                            ]
                            []
                        , Svg.text_
                            [ Svg.x "12"
                            , Svg.y "16"
                            , Svg.fontSize "12"
                            , Svg.fill "#fff"
                            ]
                            [ Svg.text <| toString num ]
                        ]
                    ]
                ]
            , div [ class "text" ]
                [ text title
                ]
            ]


stepLine : Html msg
stepLine =
    let
        class =
            mkClass "stepper--"
    in
        div [ class "step-line" ]
            [ span [] []
            ]


mkClass : String -> String -> Html.Attribute msg
mkClass classNamespace c =
    Attrs.class (classNamespace ++ c)


mkClassList : String -> List ( String, Bool ) -> Html.Attribute msg
mkClassList classNamespace cs =
    List.map (\( c, b ) -> ( classNamespace ++ c, b )) cs
        |> Attrs.classList
