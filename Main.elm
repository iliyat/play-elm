port module Main exposing (..)

import Html exposing (p, div, text, span)
import String
import List
import Dict


type Rule
    = Create
    | Read
    | Update
    | Delete
    | Nope


type Entity
    = Legal
    | Brand
    | PointOfSale
    | Workplace
    | Employee
    | CalculationDaily


type Role
    = Admin
    | Accountant


type alias U =
    { entity : Entity
    , rules : List String
    }


ruleDict : Dict.Dict String Rule
ruleDict =
    Dict.fromList
        [ ( "r", Read )
        , ( "c", Create )
        , ( "u", Update )
        , ( "d", Delete )
        ]


entities : Dict.Dict String Entity
entities =
    Dict.fromList
        [ ( "legal", Legal )
        , ( "brand", Brand )
        , ( "pointOfSale", PointOfSale )
        , ( "workplace", Workplace )
        , ( "employee", Employee )
        , ( "rule", CalculationDaily )
        ]


roleDict : Dict.Dict String (Dict.Dict String (List Rule))
roleDict =
    Dict.fromList
        [ ( "ADMINISTRATOR"
          , Dict.fromList
                [ ( "legal", [ Read, Create, Update, Delete ] )
                , ( "brand", [ Read, Create, Update, Delete ] )
                , ( "pointOfSale", [ Read, Create, Update, Delete ] )
                , ( "workplace", [ Read, Create, Update, Delete ] )
                , ( "employee", [ Read, Create, Update, Delete ] )
                , ( "rule", [ Read, Create, Update, Delete ] )
                ]
          )
        , ( "ACCOUNTANT"
          , Dict.fromList
                [ ( "legal", [ Read, Create, Update ] )
                , ( "brand", [ Read, Create, Update ] )
                , ( "pointOfSale", [ Read, Create, Update ] )
                , ( "workplace", [ Read, Create, Update ] )
                , ( "employee", [] )
                , ( "rule", [] )
                ]
          )
        , ( "HUMAN_RESOURCES"
          , Dict.fromList
                [ ( "legal", [ Read, Create, Update, Delete ] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read, Create, Update ] )
                , ( "workplace", [ Read, Create, Update ] )
                , ( "employee", [] )
                , ( "rule", [] )
                ]
          )
        , ( "SENIOR_MANAGER"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read, Update ] )
                , ( "workplace", [ Read ] )
                , ( "employee", [] )
                , ( "rule", [] )
                ]
          )
        , ( "LOANS_MANAGER"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read ] )
                , ( "workplace", [] )
                , ( "employee", [] )
                , ( "rule", [] )
                ]
          )
        , ( "TECHNICAL_SUPPORT"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read, Update ] )
                , ( "workplace", [] )
                , ( "employee", [ Read ] )
                , ( "rule", [] )
                ]
          )
        ]


canUser : List String -> String -> String -> Bool
canUser roles rule entity =
    let
        fold role acc =
            let
                rulesForRole =
                    Dict.get role roleDict |> Maybe.andThen (Dict.get entity) |> Maybe.withDefault []
            in
                List.append acc rulesForRole

        getRules =
            List.foldl fold [] roles
    in
        List.member (Dict.get rule ruleDict |> Maybe.withDefault Nope) getRules


main : Html.Html msg
main =
    Html.text <|
        toString <|
            canUser [ "HUMAN_RESOURCES", "ACCOUNTANT" ]
                "u"
                "brand"
