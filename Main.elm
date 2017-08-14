module Roles exposing (main)

import Html
import Dict
import Set


type Rule
    = Create
    | Read
    | Update
    | Delete
    | Nope


ruleDict : Dict.Dict String Rule
ruleDict =
    Dict.fromList
        [ ( "r", Read )
        , ( "c", Create )
        , ( "u", Update )
        , ( "d", Delete )
        ]


indexRoutes : Dict.Dict String String
indexRoutes =
    Dict.fromList
        [ ( "ADMINISTRATOR", "/directory/brands" )
        , ( "ACCOUNTANT", "/directory/brands" )
        , ( "HUMAN_RESOURCES", "/directory/offices" )
        , ( "SENIOR_MANAGER", "/directory/offices" )
        , ( "LOANS_MANAGER", "/directory/offices" )
        , ( "TECHNICAL_SUPPORT", "/directory/offices" )
        , ( "CALL_CENTER", "/directory/employees" )
        , ( "VIDEO_AUDITOR", "/directory/employees" )
        ]


getIndexRoute : List String -> String
getIndexRoute roles =
    List.head roles
        |> Maybe.map
            (\v -> Dict.get v indexRoutes |> Maybe.withDefault "/undefined-role")
        |> Maybe.withDefault "/no-access"


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
                , ( "workplace", [] )
                , ( "employee", [] )
                , ( "rule", [] )
                ]
          )
        , ( "HUMAN_RESOURCES"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read, Create, Update ] )
                , ( "workplace", [] )
                , ( "employee", [ Read, Create, Update, Delete ] )
                , ( "rule", [] )
                ]
          )
        , ( "SENIOR_MANAGER"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read ] )
                , ( "workplace", [] )
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
                , ( "workplace", [ Read, Create, Update ] )
                , ( "employee", [ Read ] )
                , ( "rule", [] )
                ]
          )
        , ( "CALL_CENTER"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read ] )
                , ( "workplace", [] )
                , ( "employee", [ Read ] )
                , ( "rule", [] )
                ]
          )
        , ( "VIDEO_AUDITOR"
          , Dict.fromList
                [ ( "legal", [] )
                , ( "brand", [] )
                , ( "pointOfSale", [ Read ] )
                , ( "workplace", [] )
                , ( "employee", [ Read ] )
                , ( "rule", [] )
                ]
          )
        ]


availableEntites : List String -> List String
availableEntites =
    let
        entities role =
            Dict.get role roleDict
                |> Maybe.withDefault Dict.empty
                |> Dict.filter (\k v -> List.member Read v)
                |> Dict.keys

        foldFunc role acc =
            List.append acc (entities role)
    in
        Set.toList << Set.fromList << List.foldl foldFunc []


ruleMapper : Rule -> String
ruleMapper rule =
    case rule of
        Read ->
            "r"

        Create ->
            "c"

        Update ->
            "u"

        Delete ->
            "d"

        _ ->
            ""


main : Html.Html msg
main =
    Html.text <| toString <| availableEntites [ "ADMINISTRATOR" ]
