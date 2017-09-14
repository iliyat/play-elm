module Data.Loan exposing (Loan)


type alias Loan =
    { id : String
    , avatar : String
    , dateOfBirth : String
    , passport : String
    , phone : String
    , loanNumber : String
    , status : String
    , comment : String
    }
