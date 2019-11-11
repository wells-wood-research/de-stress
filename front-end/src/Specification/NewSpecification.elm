module Specification.NewSpecification exposing
    ( NewRequirement(..)
    , NewRequirementData(..)
    , newRequirementToRequirement
    )

import Codec exposing (Codec)
import Specification as Spec exposing (Specification)


type alias NewSpecification =
    { name : Maybe String
    , description : Maybe String
    , requirements : List (NewRequirement NewRequirementData)
    }


buildSpecification : NewSpecification -> Result String Specification
buildSpecification newSpecification =
    Result.map3 Specification
        (Result.fromMaybe "No name provided." newSpecification.name)
        (Result.fromMaybe "No description provided." newSpecification.description)
        (appendRequirementResult newSpecification.requirements (Ok [])
            |> Result.map Spec.All
        )


appendRequirementResult :
    List (NewRequirement NewRequirementData)
    -> Result String (List (Spec.Requirement Spec.RequirementData))
    -> Result String (List (Spec.Requirement Spec.RequirementData))
appendRequirementResult newRequirements rRequirements =
    case newRequirements of
        [] ->
            Ok []

        req :: rest ->
            Result.map2 (\r rs -> r :: rs)
                (newRequirementToRequirement req)
                rRequirements
                |> appendRequirementResult rest


type NewRequirement a
    = Data a
    | Not (Maybe (NewRequirement a))
    | Or (Maybe (NewRequirement a)) (Maybe (NewRequirement a))
    | And (Maybe (NewRequirement a)) (Maybe (NewRequirement a))


type NewRequirementData
    = Constant (Maybe ConstantType)
    | Value (Maybe ValueType)


type ConstantType
    = Method (Maybe MethodType)


stringFromConstantType : ConstantType -> String
stringFromConstantType constantType =
    case constantType of
        Method _ ->
            "Method"


type MethodType
    = SPPS
    | MolecularBiology


stringFromMethodType : MethodType -> String
stringFromMethodType methodType =
    case methodType of
        SPPS ->
            "SPPS"

        MolecularBiology ->
            "Molecular Biology"


type ValueType
    = IsoelectricPoint (Maybe Order) (Maybe String)
    | HydrophobicFitness (Maybe Order) (Maybe String)
    | MeanPackingDensity (Maybe Order) (Maybe String)
    | SequenceContains (Maybe String)


stringFromValueType : ValueType -> String
stringFromValueType valueType =
    case valueType of
        IsoelectricPoint _ _ ->
            "Isoelectric Point"

        HydrophobicFitness _ _ ->
            "Hydrophobic Fitness"

        MeanPackingDensity _ _ ->
            "Mean Packing Density"

        SequenceContains _ ->
            "Sequence Contains"


stringFromNewRequirement : NewRequirement NewRequirementData -> String
stringFromNewRequirement newRequirement =
    case newRequirement of
        Data data ->
            case data of
                Constant _ ->
                    "Constant"

                Value _ ->
                    "Value"

        Not _ ->
            "Not"

        Or _ _ ->
            "Or"

        And _ _ ->
            "And"


newRequirementToRequirement :
    NewRequirement NewRequirementData
    -> Result String (Spec.Requirement Spec.RequirementData)
newRequirementToRequirement newRequirement =
    case newRequirement of
        Data data ->
            case data of
                Constant mConstantType ->
                    let
                        constantConstructor =
                            Spec.Constant
                                >> Spec.Data
                    in
                    case mConstantType of
                        Nothing ->
                            Err "Constant was not fully defined."

                        Just constantType ->
                            case constantType of
                                Method Nothing ->
                                    Err "Constant was not fully defined."

                                Method (Just methodType) ->
                                    let
                                        methodConstructor =
                                            constantConstructor << Spec.Method
                                    in
                                    (case methodType of
                                        SPPS ->
                                            methodConstructor Spec.SPPS

                                        MolecularBiology ->
                                            methodConstructor Spec.MolecularBiology
                                    )
                                        |> Ok

                Value mValueType ->
                    let
                        valueConstructor =
                            Spec.Value
                                >> Spec.Data
                    in
                    case mValueType of
                        Nothing ->
                            Err "Value was not fully defined."

                        Just valueType ->
                            case valueType of
                                IsoelectricPoint (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Spec.IsoelectricPoint order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for isoelectric "
                                                    ++ "point is not a number."

                                IsoelectricPoint _ _ ->
                                    Err "Isoelectric point value was not fully defined."

                                HydrophobicFitness (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Spec.HydrophobicFitness order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for hydrophobic "
                                                    ++ "fitness is not a number."

                                HydrophobicFitness _ _ ->
                                    Err "Hydrophobic fitness value was not fully defined."

                                MeanPackingDensity (Just order) (Just value) ->
                                    case String.toFloat value of
                                        Just floatValue ->
                                            Spec.MeanPackingDensity order floatValue
                                                |> valueConstructor
                                                |> Ok

                                        Nothing ->
                                            Err <|
                                                "The value you've provided for mean "
                                                    ++ "packing density is not a number."

                                MeanPackingDensity _ _ ->
                                    Err "Mean packing density value was not fully defined."

                                SequenceContains (Just string) ->
                                    Spec.SequenceContains string
                                        |> valueConstructor
                                        |> Ok

                                SequenceContains _ ->
                                    Err "Sequence contains value was not fully defined."

        Not mNewSubRequirement ->
            case mNewSubRequirement of
                Nothing ->
                    Err "Not statement is not fully defined."

                Just newSubRequirement ->
                    newRequirementToRequirement newSubRequirement
                        |> Result.map Spec.Not

        Or mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Spec.Or
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "Or statement is not fully defined."

        And mNewSubRequirement1 mNewSubRequirement2 ->
            case ( mNewSubRequirement1, mNewSubRequirement2 ) of
                ( Just newSubRequirement1, Just newSubRequirement2 ) ->
                    Result.map2 Spec.And
                        (newRequirementToRequirement newSubRequirement1)
                        (newRequirementToRequirement newSubRequirement2)

                _ ->
                    Err "And statement is not fully defined."
