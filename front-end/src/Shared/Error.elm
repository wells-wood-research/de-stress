module Shared.Error exposing (Error, Severity(..))


type alias Error =
    { title : String
    , details : String
    , severity : Severity
    }


type Severity
    = High
    | Medium
    | Low
