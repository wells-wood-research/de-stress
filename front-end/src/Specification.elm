module Specification exposing
    ( ConstantType(..)
    , MethodType(..)
    , Requirement(..)
    , RequirementData(..)
    , Specification
    , SpecificationStub
    , UnitType(..)
    , ValueType(..)
    , applySpecification
    , codec
    , createSpecificationStub
    , resolveRequirement
    , specificationStubCodec
    , stringFromOrder
    , stringFromUnitType
    )

import Codec exposing (Codec, Value)
import Dict
import Metrics exposing (DesignMetrics)
import Style


type alias Specification =
    { name : String
    , description : String
    , requirements : Requirement RequirementData
    , deleteStatus : Style.DangerStatus
    }


codec : Codec Specification
codec =
    Codec.object Specification
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "requirements" .requirements (requirementCodec requirementDataTypeCodec)
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


type Requirement a
    = Data a
    | Not (Requirement a)
    | Or (Requirement a) (Requirement a)
    | And (Requirement a) (Requirement a)
    | Any (List (Requirement a))
    | All (List (Requirement a))


requirementCodec : Codec a -> Codec (Requirement a)
requirementCodec meta =
    Codec.recursive
        (\rmeta ->
            Codec.custom
                (\fdata fnot for fand fany fall value ->
                    case value of
                        Data data ->
                            fdata data

                        Not requirement ->
                            fnot requirement

                        Or requirement1 requirement2 ->
                            for requirement1 requirement2

                        And requirement1 requirement2 ->
                            fand requirement1 requirement2

                        Any requirements ->
                            fany requirements

                        All requirements ->
                            fall requirements
                )
                |> Codec.variant1 "Data" Data meta
                |> Codec.variant1 "Not" Not rmeta
                |> Codec.variant2 "Or" Or rmeta rmeta
                |> Codec.variant2 "And" And rmeta rmeta
                |> Codec.variant1 "Any" Any (Codec.list rmeta)
                |> Codec.variant1 "All" All (Codec.list rmeta)
                |> Codec.buildCustom
        )


type RequirementData
    = Constant ConstantType
    | Value ValueType


requirementDataTypeCodec : Codec RequirementData
requirementDataTypeCodec =
    Codec.custom
        (\fconstant fvalue value ->
            case value of
                Constant constantType ->
                    fconstant constantType

                Value valueType ->
                    fvalue valueType
        )
        |> Codec.variant1 "Constant" Constant constantTypeCodec
        |> Codec.variant1 "Value" Value valueTypeCodec
        |> Codec.buildCustom


type ConstantType
    = Method MethodType


constantTypeCodec : Codec ConstantType
constantTypeCodec =
    Codec.custom
        (\fconstantType value ->
            case value of
                Method methodType ->
                    fconstantType methodType
        )
        |> Codec.variant1 "Method" Method methodTypeCodec
        |> Codec.buildCustom


type MethodType
    = SPPS
    | MolecularBiology


methodTypeCodec : Codec MethodType
methodTypeCodec =
    Codec.custom
        (\fspps fmolbio value ->
            case value of
                SPPS ->
                    fspps

                MolecularBiology ->
                    fmolbio
        )
        |> Codec.variant0 "SPPS" SPPS
        |> Codec.variant0 "MolecularBiology" MolecularBiology
        |> Codec.buildCustom


type ValueType
    = IsoelectricPoint Order Float
    | HydrophobicFitness Order Float
    | MeanPackingDensity Order Float
    | SequenceContains String
    | CompositionDeviation UnitType Float


valueTypeCodec : Codec ValueType
valueTypeCodec =
    Codec.custom
        (\fisoelectric fhfitness fmpdensity fseqcontains fcompdeviation value ->
            case value of
                IsoelectricPoint order float ->
                    fisoelectric order float

                HydrophobicFitness order float ->
                    fhfitness order float

                MeanPackingDensity order float ->
                    fmpdensity order float

                SequenceContains sequence ->
                    fseqcontains sequence

                CompositionDeviation unitType float ->
                    fcompdeviation unitType float
        )
        |> Codec.variant2 "IsoelectricPoint" IsoelectricPoint orderCodec Codec.float
        |> Codec.variant2 "HydrophobicFitness" HydrophobicFitness orderCodec Codec.float
        |> Codec.variant2 "MeanPackingDensity" MeanPackingDensity orderCodec Codec.float
        |> Codec.variant1 "SequenceContains" SequenceContains Codec.string
        |> Codec.variant2 "CompositionDeviation" CompositionDeviation unitTypeCodec Codec.float
        |> Codec.buildCustom


orderCodec : Codec Order
orderCodec =
    Codec.custom
        (\flt feq fgt value ->
            case value of
                LT ->
                    flt

                EQ ->
                    feq

                GT ->
                    fgt
        )
        |> Codec.variant0 "LT" LT
        |> Codec.variant0 "EQ" EQ
        |> Codec.variant0 "GT" GT
        |> Codec.buildCustom


stringFromOrder : Order -> String
stringFromOrder order =
    case order of
        LT ->
            "LessThan"

        EQ ->
            "EqualTo"

        GT ->
            "GreaterThan"


type UnitType
    = StdDevs
    | Percent


stringFromUnitType : UnitType -> String
stringFromUnitType unitType =
    case unitType of
        StdDevs ->
            "Standard Deviations"

        Percent ->
            "Percent"


unitTypeCodec : Codec UnitType
unitTypeCodec =
    Codec.custom
        (\fsd fpc value ->
            case value of
                StdDevs ->
                    fsd

                Percent ->
                    fpc
        )
        |> Codec.variant0 "StdDevs" StdDevs
        |> Codec.variant0 "Percent" Percent
        |> Codec.buildCustom


type alias SpecificationStub =
    { name : String
    , description : String
    , deleteStatus : Style.DangerStatus
    }


specificationStubCodec : Codec SpecificationStub
specificationStubCodec =
    Codec.object SpecificationStub
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "deleteStatus" .deleteStatus (Codec.constant Style.Unclicked)
        |> Codec.buildObject


createSpecificationStub : Specification -> SpecificationStub
createSpecificationStub specification =
    { name = specification.name
    , description = specification.description
    , deleteStatus = specification.deleteStatus
    }


applySpecification : Maybe Metrics.AggregateData -> DesignMetrics -> Specification -> Bool
applySpecification mAggregateData designMetrics specification =
    resolveRequirement mAggregateData designMetrics specification.requirements


resolveRequirement :
    Maybe Metrics.AggregateData
    -> DesignMetrics
    -> Requirement RequirementData
    -> Bool
resolveRequirement mAggregateData metrics requirement =
    case requirement of
        All requirements ->
            List.all (resolveRequirement mAggregateData metrics) requirements

        Any requirements ->
            List.any (resolveRequirement mAggregateData metrics) requirements

        And subRequirement1 subRequirement2 ->
            resolveRequirement mAggregateData metrics subRequirement1
                && resolveRequirement mAggregateData metrics subRequirement2

        Or subRequirement1 subRequirement2 ->
            resolveRequirement mAggregateData metrics subRequirement1
                || resolveRequirement mAggregateData metrics subRequirement2

        Not subRequirement ->
            not (resolveRequirement mAggregateData metrics subRequirement)

        Data data ->
            case data of
                Value valueType ->
                    case valueType of
                        IsoelectricPoint order value ->
                            compare metrics.isoelectricPoint value == order

                        HydrophobicFitness order value ->
                            case metrics.hydrophobicFitness of
                                Nothing ->
                                    False

                                Just hf ->
                                    compare hf value == order

                        MeanPackingDensity order value ->
                            compare metrics.packingDensity value == order

                        SequenceContains seqString ->
                            List.any (String.contains seqString)
                                (Dict.values
                                    metrics.sequenceInfo
                                    |> List.map .sequence
                                )

                        CompositionDeviation unitType value ->
                            Debug.log "This should do something" True

                Constant constantType ->
                    case constantType of
                        Method _ ->
                            Debug.log "This should do something" True
