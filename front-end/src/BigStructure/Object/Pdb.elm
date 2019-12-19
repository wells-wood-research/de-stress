-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module BigStructure.Object.Pdb exposing (..)

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


id : SelectionSet BigStructure.ScalarCodecs.Id BigStructure.Object.Pdb
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (BigStructure.ScalarCodecs.codecs |> BigStructure.Scalar.unwrapCodecs |> .codecId |> .decoder)


pdbCode : SelectionSet String BigStructure.Object.Pdb
pdbCode =
    Object.selectionForField "String" "pdbCode" [] Decode.string


depositionDate : SelectionSet String BigStructure.Object.Pdb
depositionDate =
    Object.selectionForField "String" "depositionDate" [] Decode.string


method : SelectionSet String BigStructure.Object.Pdb
method =
    Object.selectionForField "String" "method" [] Decode.string


biolUnits : SelectionSet decodesTo BigStructure.Object.BiolUnit -> SelectionSet (Maybe (List (Maybe decodesTo))) BigStructure.Object.Pdb
biolUnits object_ =
    Object.selectionForCompositeField "biolUnits" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)