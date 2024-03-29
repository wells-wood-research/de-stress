-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module BigStructure.Object.DFIRE2Results exposing (..)

import BigStructure.InputObject
import BigStructure.Interface
import BigStructure.Object
import BigStructure.Scalar
import BigStructure.ScalarCodecs
import BigStructure.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


id : SelectionSet BigStructure.ScalarCodecs.Id BigStructure.Object.DFIRE2Results
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (BigStructure.ScalarCodecs.codecs |> BigStructure.Scalar.unwrapCodecs |> .codecId |> .decoder)


logInfo : SelectionSet String BigStructure.Object.DFIRE2Results
logInfo =
    Object.selectionForField "String" "logInfo" [] Decode.string


errorInfo : SelectionSet String BigStructure.Object.DFIRE2Results
errorInfo =
    Object.selectionForField "String" "errorInfo" [] Decode.string


returnCode : SelectionSet Int BigStructure.Object.DFIRE2Results
returnCode =
    Object.selectionForField "Int" "returnCode" [] Decode.int


total : SelectionSet (Maybe Float) BigStructure.Object.DFIRE2Results
total =
    Object.selectionForField "(Maybe Float)" "total" [] (Decode.float |> Decode.nullable)


stateId : SelectionSet (Maybe Int) BigStructure.Object.DFIRE2Results
stateId =
    Object.selectionForField "(Maybe Int)" "stateId" [] (Decode.int |> Decode.nullable)


state :
    SelectionSet decodesTo BigStructure.Object.State
    -> SelectionSet (Maybe decodesTo) BigStructure.Object.DFIRE2Results
state object____ =
    Object.selectionForCompositeField "state" [] object____ (identity >> Decode.nullable)
