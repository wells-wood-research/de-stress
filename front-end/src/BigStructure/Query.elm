-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module BigStructure.Query exposing (..)

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
import Json.Decode as Decode exposing (Decoder)


type alias AllPdbsOptionalArguments =
    { first : OptionalArgument Int }


{-| Gets all PDB records. Accepts the argument `first`, which allows you to limit the number of results.
-}
allPdbs : (AllPdbsOptionalArguments -> AllPdbsOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.Pdb -> SelectionSet (List (Maybe decodesTo)) RootQuery
allPdbs fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "allPdbs" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


{-| Returns a count of the PDB records.
-}
pdbCount : SelectionSet Int RootQuery
pdbCount =
    Object.selectionForField "Int" "pdbCount" [] Decode.int


type alias AllBiolUnitsOptionalArguments =
    { first : OptionalArgument Int }


allBiolUnits : (AllBiolUnitsOptionalArguments -> AllBiolUnitsOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.BiolUnit -> SelectionSet (List (Maybe decodesTo)) RootQuery
allBiolUnits fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "allBiolUnits" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


type alias PreferredBiolUnitsOptionalArguments =
    { first : OptionalArgument Int }


{-| Gets preferred biological unit records. Accepts the argument `first`, which allows you to limit the number of results.
-}
preferredBiolUnits : (PreferredBiolUnitsOptionalArguments -> PreferredBiolUnitsOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.BiolUnit -> SelectionSet (List (Maybe decodesTo)) RootQuery
preferredBiolUnits fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "preferredBiolUnits" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


{-| Returns a count of the biological unit records.
-}
biolUnitCount : SelectionSet Int RootQuery
biolUnitCount =
    Object.selectionForField "Int" "biolUnitCount" [] Decode.int


type alias AllStatesOptionalArguments =
    { first : OptionalArgument Int }


{-| Gets all states. Accepts the argument `first`, which allows you to limit the number of results.
-}
allStates : (AllStatesOptionalArguments -> AllStatesOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.State -> SelectionSet (List (Maybe decodesTo)) RootQuery
allStates fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "allStates" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


type alias PreferredStatesOptionalArguments =
    { first : OptionalArgument Int
    , stateNumber : OptionalArgument Int
    }


{-| Gets the preferred state for all preferred biological units. Accepts the arguments:
`state_number`, which allows you specify the preferred state number.
`first`, which allows you to limit the number of results.
-}
preferredStates : (PreferredStatesOptionalArguments -> PreferredStatesOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.State -> SelectionSet (List (Maybe decodesTo)) RootQuery
preferredStates fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent, stateNumber = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "stateNumber" filledInOptionals.stateNumber Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "preferredStates" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


{-| Returns a count of the state records.
-}
stateCount : SelectionSet Int RootQuery
stateCount =
    Object.selectionForField "Int" "stateCount" [] Decode.int


type alias AllChainsOptionalArguments =
    { first : OptionalArgument Int }


{-| Gets all chains. Accepts the argument `first`, which allows you to limit the number of results.
-}
allChains : (AllChainsOptionalArguments -> AllChainsOptionalArguments) -> SelectionSet decodesTo BigStructure.Object.Chain -> SelectionSet (List (Maybe decodesTo)) RootQuery
allChains fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "allChains" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


{-| Returns a count of the chain records.
-}
chainCount : SelectionSet Int RootQuery
chainCount =
    Object.selectionForField "Int" "chainCount" [] Decode.int