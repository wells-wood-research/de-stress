module Shared.Requirement exposing
    ( ConstantType(..)
    , MethodType(..)
    , Requirement(..)
    , RequirementData(..)
    , UnitType(..)
    , ValueType(..)
    , dataToString
    , requirementView
    , requirementWithDataCodec
    , resolveRequirement
    , stringFromOrder
    , stringFromUnitType
    )

import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Shared.Metrics as Metrics exposing (DesignMetrics)
import Shared.Style as Style
import Shared.Tooltips exposing (HoverInfoOption(..))



-- {{{ Requirement


type Requirement a
    = Data a
    | Not (Requirement a)
    | Or (Requirement a) (Requirement a)
    | And (Requirement a) (Requirement a)
    | Any (List (Requirement a))
    | All (List (Requirement a))


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

                        CompositionDeviation unitType threshold ->
                            case mAggregateData of
                                Just aggregateData ->
                                    resolveCompositionDeviation
                                        unitType
                                        threshold
                                        aggregateData.composition
                                        metrics.composition

                                Nothing ->
                                    True

                        BUDEFFTotal order value ->
                            case metrics.budeFFResults.totalEnergy of
                                Nothing ->
                                    False

                                Just budetot ->
                                    compare budetot value == order

                        EvoEF2Total order value ->
                            case metrics.evoEF2Results.total of
                                Nothing ->
                                    False

                                Just evoef2tot ->
                                    compare evoef2tot value == order

                        DFIRE2Total order value ->
                            case metrics.dfire2Results.total of
                                Nothing ->
                                    False

                                Just dfire2tot ->
                                    compare dfire2tot value == order

                        RosettaTotal order value ->
                            case metrics.rosettaResults.total_score of
                                Nothing ->
                                    False

                                Just rostot ->
                                    compare rostot value == order

                        Agg3DTotal order value ->
                            case metrics.aggrescan3dResults.total_value of
                                Nothing ->
                                    False

                                Just agg3dtot ->
                                    compare agg3dtot value == order

                Constant constantType ->
                    case constantType of
                        Method _ ->
                            True


resolveCompositionDeviation :
    UnitType
    -> Float
    -> Dict String (Maybe Metrics.MeanMedAndStdDev)
    -> Dict String Float
    -> Bool
resolveCompositionDeviation unitType threshold meanComposition designComposition =
    case unitType of
        Percent ->
            {- This looks horrible, but it's not too
               bad, it basically takes the aggregate
               composition dict and the design composition
               dict and makes a new dict where the key is
               the amino acid label and the value is a
               Bool that is True if the difference
               between the reference set composition and
               the designs composition is less than the
               allowed composition deviation from the
               specification
            -}
            Dict.merge
                (\k _ -> Dict.insert k True)
                (\k a b ->
                    Dict.insert k
                        (((Maybe.map .mean a
                            |> Maybe.withDefault 0
                          )
                            - b
                         )
                            |> abs
                            |> (\d ->
                                    d
                                        < (threshold
                                            * 0.01
                                          )
                               )
                        )
                )
                (\k _ -> Dict.insert k True)
                meanComposition
                designComposition
                Dict.empty
                |> Dict.values
                |> List.all identity

        StdDevs ->
            Dict.merge
                (\k _ -> Dict.insert k True)
                (\k a b ->
                    Dict.insert k
                        (case a of
                            Just { mean, stdDev } ->
                                mean
                                    - b
                                    |> abs
                                    |> (\d -> d < (threshold * stdDev))

                            Nothing ->
                                True
                        )
                )
                (\k _ -> Dict.insert k True)
                meanComposition
                designComposition
                Dict.empty
                |> Dict.values
                |> List.all identity


requirementView : Requirement RequirementData -> Element msg
requirementView requirement =
    let
        arrowRow r =
            row [ padding 5, spacing 15, width fill ]
                [ el []
                    (FeatherIcons.chevronRight
                        |> FeatherIcons.toHtml []
                        |> html
                    )
                , requirementView r
                ]
    in
    case requirement of
        Data data ->
            paragraph
                (Style.defaultBorder ++ [ padding 10, width fill ])
                [ text <| dataToString data ]

        Not subRequirement ->
            row (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                [ el [] <| Style.h3 <| el [ Font.bold ] (text <| "NOT")
                , requirementView subRequirement
                ]

        Or subRequirement1 subRequirement2 ->
            column
                (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                [ requirementView subRequirement1
                , Style.h3 <| el [ Font.bold ] (text "---- OR ----")
                , requirementView subRequirement2
                ]

        And requirement1 requirement2 ->
            column
                (Style.defaultBorder ++ [ padding 10, spacing 10, width fill ])
                [ requirementView requirement1
                , el [ Font.bold ]
                    (text "---- AND ----")
                , requirementView requirement2
                ]

        Any requirements ->
            column
                (Style.defaultBorder
                    ++ [ padding 10
                       , spacing 10
                       , width fill
                       ]
                )
                [ el [ Font.bold ] (text "ANY")
                , column [ padding 10, spacing 10, width fill ] <|
                    List.map arrowRow requirements
                ]

        All requirements ->
            column
                (Style.defaultBorder
                    ++ [ padding 10
                       , spacing 10
                       , width fill
                       , Background.color
                            Style.colorPalette.white
                       ]
                )
                [ el [ Font.bold ] (text "ALL")
                , column [ padding 10, spacing 5, width fill ] <|
                    List.map arrowRow requirements
                ]


requirementWithDataCodec : Codec (Requirement RequirementData)
requirementWithDataCodec =
    requirementCodec requirementDataTypeCodec


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



-- }}}
-- {{{ RequirementData


type RequirementData
    = Constant ConstantType
    | Value ValueType


dataToString : RequirementData -> String
dataToString requirementData =
    case requirementData of
        Constant constantType ->
            let
                typeString =
                    "Constant: "
            in
            case constantType of
                Method methodType ->
                    let
                        constantTypeString =
                            typeString ++ "Method: "
                    in
                    case methodType of
                        SPPS ->
                            constantTypeString ++ "SPPS"

                        MolecularBiology ->
                            constantTypeString ++ "MolBio"

        Value valueType ->
            let
                typeString =
                    "Value: "
            in
            case valueType of
                IsoelectricPoint order value ->
                    typeString
                        ++ "IsoelectricPoint: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                HydrophobicFitness order value ->
                    typeString
                        ++ "HydrophobicFitness: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                MeanPackingDensity order value ->
                    typeString
                        ++ "MeanPackingDensity: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                SequenceContains string ->
                    typeString
                        ++ "SequenceContains: "
                        ++ string

                CompositionDeviation unitType value ->
                    typeString
                        ++ "CompositionDeviation: "
                        ++ stringFromUnitType unitType
                        ++ ": "
                        ++ String.fromFloat value

                BUDEFFTotal order value ->
                    typeString
                        ++ "BUDEFF Total Energy: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                EvoEF2Total order value ->
                    typeString
                        ++ "EvoEF2 Total Energy: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                DFIRE2Total order value ->
                    typeString
                        ++ "DFIRE2 Total Energy: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                RosettaTotal order value ->
                    typeString
                        ++ "Rosetta Total Energy: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value

                Agg3DTotal order value ->
                    typeString
                        ++ "Aggrescan3D Total Score: "
                        ++ stringFromOrder
                            order
                        ++ ": "
                        ++ String.fromFloat value


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
    | BUDEFFTotal Order Float
    | EvoEF2Total Order Float
    | DFIRE2Total Order Float
    | RosettaTotal Order Float
    | Agg3DTotal Order Float


valueTypeCodec : Codec ValueType
valueTypeCodec =
    Codec.custom
        (\fisoelectric fhfitness fmpdensity fseqcontains fcompdeviation fbudefftotal fevoef2total fdfire2total frosettatotal fagg3dtotal value ->
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

                BUDEFFTotal order float ->
                    fbudefftotal order float

                EvoEF2Total order float ->
                    fevoef2total order float

                DFIRE2Total order float ->
                    fdfire2total order float

                RosettaTotal order float ->
                    frosettatotal order float

                Agg3DTotal order float ->
                    fagg3dtotal order float
        )
        |> Codec.variant2 "IsoelectricPoint" IsoelectricPoint orderCodec Codec.float
        |> Codec.variant2 "HydrophobicFitness" HydrophobicFitness orderCodec Codec.float
        |> Codec.variant2 "MeanPackingDensity" MeanPackingDensity orderCodec Codec.float
        |> Codec.variant1 "SequenceContains" SequenceContains Codec.string
        |> Codec.variant2 "CompositionDeviation" CompositionDeviation unitTypeCodec Codec.float
        |> Codec.variant2 "BUDEFFTotal" BUDEFFTotal orderCodec Codec.float
        |> Codec.variant2 "EvoEF2Total" EvoEF2Total orderCodec Codec.float
        |> Codec.variant2 "DFIRE2Total" DFIRE2Total orderCodec Codec.float
        |> Codec.variant2 "RosettaTotal" RosettaTotal orderCodec Codec.float
        |> Codec.variant2 "Agg3DTotal" Agg3DTotal orderCodec Codec.float
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



-- }}}
