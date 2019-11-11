module Specification exposing
    ( ConstantType(..)
    , MethodType(..)
    , Requirement(..)
    , RequirementData(..)
    , Specification
    , ValueType(..)
    , specificationCodec
    )

import Codec exposing (Codec)


type alias Specification =
    { name : String
    , description : String
    , requirements : Requirement RequirementData
    }


specificationCodec : Codec Specification
specificationCodec =
    Codec.object Specification
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "requirements" .requirements (requirementCodec requirementDataTypeCodec)
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


valueTypeCodec : Codec ValueType
valueTypeCodec =
    Codec.custom
        (\fisoelectric fhfitness fmpdensity fseqcontains value ->
            case value of
                IsoelectricPoint order float ->
                    fisoelectric order float

                HydrophobicFitness order float ->
                    fhfitness order float

                MeanPackingDensity order float ->
                    fmpdensity order float

                SequenceContains sequence ->
                    fseqcontains sequence
        )
        |> Codec.variant2 "IsoelectricPoint" IsoelectricPoint orderCodec Codec.float
        |> Codec.variant2 "HydrophobicFitness" HydrophobicFitness orderCodec Codec.float
        |> Codec.variant2 "MeanPackingDensity" MeanPackingDensity orderCodec Codec.float
        |> Codec.variant1 "SequenceContains" SequenceContains Codec.string
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
