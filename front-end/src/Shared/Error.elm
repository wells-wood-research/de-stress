module Shared.Error exposing
    ( Error
    , Severity(..)
    , updateSharedModelErrors
    , updateWithError
    )

import Task


type alias Error =
    { title : String
    , details : String
    , severity : Severity
    }


type Severity
    = High
    | Medium
    | Low


updateWithError :
    msg
    -> { a | pageErrors : List Error }
    -> Error
    -> ( { a | pageErrors : List Error }, Cmd msg )
updateWithError clearMsg model error =
    ( { model
        | pageErrors =
            error :: model.pageErrors
      }
    , Task.succeed clearMsg
        |> Task.perform identity
    )


updateSharedModelErrors :
    { a | pageErrors : List Error }
    -> { b | errors : List Error }
    -> { b | errors : List Error }
updateSharedModelErrors pageModel sharedModel =
    { sharedModel
        | errors =
            pageModel.pageErrors ++ sharedModel.errors
    }
