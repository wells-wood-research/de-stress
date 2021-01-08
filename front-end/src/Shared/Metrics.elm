module Shared.Metrics exposing
    ( AggregateData
    , DesignMetrics
    , EvoEF2Results
    , MeanAndStdDev
    , RefSetMetrics
    , SequenceInfo
    , aggregateDataCodec
    , calculateMeanComposition
    , compositionStringToDict
    , createAggregateData
    , createAllHistogramsSpec
    , createCompositionSpec
    , createTorsionAngleSpec
    , desMetricsCodec
    , overviewSpec
    , refSetMetricsCodec
    , testEvoEF2Results
    , torsionAngleStringToDict
    )

import Codec exposing (Codec)
import Dict exposing (Dict)
import Element exposing (..)
import Parser exposing ((|.), (|=))
import Utils.ListExtra as ListExtra
import Utils.Stats as Stats
import VegaLite as VL



-- {{{ DesignMetrics


type alias DesignMetrics =
    { sequenceInfo : Dict String SequenceInfo
    , composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numOfResidues : Int
    , packingDensity : Float
    }


desMetricsCodec : Codec DesignMetrics
desMetricsCodec =
    Codec.object DesignMetrics
        |> Codec.field "sequenceInfo" .sequenceInfo (Codec.dict sequenceInfoCodec)
        |> Codec.field "composition" .composition (Codec.dict Codec.float)
        |> Codec.field "torsionAngles"
            .torsionAngles
            (Codec.dict
                (Codec.triple
                    Codec.float
                    Codec.float
                    Codec.float
                )
            )
        |> Codec.field "hydrophobicFitness"
            .hydrophobicFitness
            (Codec.maybe Codec.float)
        |> Codec.field "isoelectricPoint" .isoelectricPoint Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "numOfResidues" .numOfResidues Codec.int
        |> Codec.field "packingDensity" .packingDensity Codec.float
        |> Codec.buildObject


type alias SequenceInfo =
    { sequence : String
    , dsspAssignment : String
    }


sequenceInfoCodec : Codec SequenceInfo
sequenceInfoCodec =
    Codec.object SequenceInfo
        |> Codec.field "sequence" .sequence Codec.string
        |> Codec.field "dsspAssignment" .dsspAssignment Codec.string
        |> Codec.buildObject



-- }}}
-- {{{ EvoEF2Results


type alias EvoEF2Results =
    { log_info : String
    , reference_ALA : Float
    , reference_CYS : Float
    , reference_ASP : Float
    , reference_GLU : Float
    , reference_PHE : Float
    , reference_GLY : Float
    , reference_HIS : Float
    , reference_ILE : Float
    , reference_LYS : Float
    , reference_LEU : Float
    , reference_MET : Float
    , reference_ASN : Float
    , reference_PRO : Float
    , reference_GLN : Float
    , reference_ARG : Float
    , reference_SER : Float
    , reference_THR : Float
    , reference_VAL : Float
    , reference_TRP : Float
    , reference_TYR : Float
    , intraR_vdwatt : Float
    , intraR_vdwrep : Float
    , intraR_electr : Float
    , intraR_deslvP : Float
    , intraR_deslvH : Float
    , intraR_hbscbb_dis : Float
    , intraR_hbscbb_the : Float
    , intraR_hbscbb_phi : Float
    , aapropensity : Float
    , ramachandran : Float
    , dunbrack : Float
    , interS_vdwatt : Float
    , interS_vdwrep : Float
    , interS_electr : Float
    , interS_deslvP : Float
    , interS_deslvH : Float
    , interS_ssbond : Float
    , interS_hbbbbb_dis : Float
    , interS_hbbbbb_the : Float
    , interS_hbbbbb_phi : Float
    , interS_hbscbb_dis : Float
    , interS_hbscbb_the : Float
    , interS_hbscbb_phi : Float
    , interS_hbscsc_dis : Float
    , interS_hbscsc_the : Float
    , interS_hbscsc_phi : Float
    , interD_vdwatt : Float
    , interD_vdwrep : Float
    , interD_electr : Float
    , interD_deslvP : Float
    , interD_deslvH : Float
    , interD_ssbond : Float
    , interD_hbbbbb_dis : Float
    , interD_hbbbbb_the : Float
    , interD_hbbbbb_phi : Float
    , interD_hbscbb_dis : Float
    , interD_hbscbb_the : Float
    , interD_hbscbb_phi : Float
    , interD_hbscsc_dis : Float
    , interD_hbscsc_the : Float
    , interD_hbscsc_phi : Float
    , total : Float
    , ref_total : Float
    , intraR_total : Float
    , interS_total : Float
    , interD_total : Float
    }


testEvoEF2Results : EvoEF2Results
testEvoEF2Results =
    { log_info = "\n                                    EvoEF2                                                  \n  A framework for macromolecular modeling, e.g.,protein design, protein side-chain packing, \nprotein structure energy minimization, add and optimize hydrogen bonds, build mutant model, \ncalculate protein folding stability, calculate protein-protein binding free energy, etc     \n\n\n  Copyright (c) Xiaoqiang Huang (xiaoqiah@umich.edu; tommyhuangthu@foxmail.com)\n  Dept. of Computational Medicine & Bioinformatics\n  Medical School\n  University of Michigan\n############################################################################################\ncommand ComputeStability works\n\n"
    , reference_ALA = -5.3
    , reference_CYS = -0.11
    , reference_ASP = -4.01
    , reference_GLU = -11.03
    , reference_PHE = 2.72
    , reference_GLY = -14.65
    , reference_HIS = -1.47
    , reference_ILE = 9.32
    , reference_LYS = -10.0
    , reference_LEU = 6.45
    , reference_MET = 3.79
    , reference_ASN = -4.31
    , reference_PRO = -4.53
    , reference_GLN = -1.94
    , reference_ARG = -2.64
    , reference_SER = -5.93
    , reference_THR = -3.33
    , reference_VAL = 20.4
    , reference_TRP = 2.0
    , reference_TYR = 2.8
    , intraR_vdwatt = -19.25
    , intraR_vdwrep = 3.66
    , intraR_electr = -0.26
    , intraR_deslvP = 0.0
    , intraR_deslvH = -1.92
    , intraR_hbscbb_dis = -0.27
    , intraR_hbscbb_the = -0.0
    , intraR_hbscbb_phi = -0.0
    , aapropensity = -16.89
    , ramachandran = 224.95
    , dunbrack = 37.88
    , interS_vdwatt = -610.82
    , interS_vdwrep = 40.41
    , interS_electr = -30.09
    , interS_deslvP = 351.86
    , interS_deslvH = -255.43
    , interS_ssbond = 0.0
    , interS_hbbbbb_dis = -48.9
    , interS_hbbbbb_the = -34.68
    , interS_hbbbbb_phi = -51.45
    , interS_hbscbb_dis = -9.61
    , interS_hbscbb_the = -7.4
    , interS_hbscbb_phi = -1.51
    , interS_hbscsc_dis = -8.93
    , interS_hbscsc_the = -3.47
    , interS_hbscsc_phi = -0.0
    , interD_vdwatt = 0.0
    , interD_vdwrep = 0.0
    , interD_electr = 0.0
    , interD_deslvP = 0.0
    , interD_deslvH = 0.0
    , interD_ssbond = 0.0
    , interD_hbbbbb_dis = 0.0
    , interD_hbbbbb_the = 0.0
    , interD_hbbbbb_phi = 0.0
    , interD_hbscbb_dis = 0.0
    , interD_hbscbb_the = 0.0
    , interD_hbscbb_phi = 0.0
    , interD_hbscsc_dis = 0.0
    , interD_hbscsc_the = 0.0
    , interD_hbscsc_phi = 0.0
    , total = -463.9
    , ref_total = -21.77
    , intraR_total = -18.04
    , interS_total = -670.02
    , interD_total = 0.0
    }



-- }}}
-- {{{ RefSetMetrics


type alias RefSetMetrics =
    { composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numOfResidues : Int
    , packingDensity : Float
    }


refSetMetricsCodec : Codec RefSetMetrics
refSetMetricsCodec =
    Codec.object RefSetMetrics
        |> Codec.field "composition" .composition (Codec.dict Codec.float)
        |> Codec.field "torsionAngles"
            .torsionAngles
            (Codec.dict
                (Codec.triple
                    Codec.float
                    Codec.float
                    Codec.float
                )
            )
        |> Codec.field "hydrophobicFitness" .hydrophobicFitness (Codec.maybe Codec.float)
        |> Codec.field "isoelectricPoint" .isoelectricPoint Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "numOfResidues" .numOfResidues Codec.int
        |> Codec.field "packingDensity" .packingDensity Codec.float
        |> Codec.buildObject


type alias AggregateData =
    { meanComposition : Dict String (Maybe MeanAndStdDev) }


type alias MeanAndStdDev =
    { mean : Float
    , stdDev : Float
    }


createAggregateData : List RefSetMetrics -> AggregateData
createAggregateData refSetMetricsList =
    { meanComposition =
        refSetMetricsList
            |> List.map .composition
            |> calculateMeanComposition
    }


aggregateDataCodec : Codec AggregateData
aggregateDataCodec =
    Codec.object AggregateData
        |> Codec.field "meanComposition"
            .meanComposition
            (Codec.dict
                (Codec.maybe
                    meanAndStdDevCodec
                )
            )
        |> Codec.buildObject


meanAndStdDevCodec : Codec MeanAndStdDev
meanAndStdDevCodec =
    Codec.object MeanAndStdDev
        |> Codec.field "mean" .mean Codec.float
        |> Codec.field "stdDev" .stdDev Codec.float
        |> Codec.buildObject



-- }}}
-- {{{ Helper Functions


compositionStringToDict : String -> Dict String Float
compositionStringToDict compString =
    Parser.run compositionParser compString
        |> Result.map Dict.fromList
        |> Result.withDefault Dict.empty


compositionParser : Parser.Parser (List ( String, Float ))
compositionParser =
    let
        residueHelp residues =
            Parser.oneOf
                [ Parser.succeed (\residue -> Parser.Loop (residue :: residues))
                    |= residueFrequencyParser
                    |. Parser.oneOf
                        [ Parser.symbol ";"
                        , Parser.end
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done residues)
                ]
    in
    Parser.loop [] residueHelp


residueFrequencyParser : Parser.Parser ( String, Float )
residueFrequencyParser =
    Parser.succeed Tuple.pair
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> Char.isAlphaNum c)
           )
        |. Parser.symbol ":"
        |= Parser.float


torsionAngleStringToDict : String -> Dict String ( Float, Float, Float )
torsionAngleStringToDict torsionAnglesString =
    torsionAnglesString
        |> Parser.run torsionAnglesParser
        |> Result.map Dict.fromList
        |> Result.withDefault Dict.empty


torsionAnglesParser : Parser.Parser (List ( String, ( Float, Float, Float ) ))
torsionAnglesParser =
    let
        torsionAnglesHelp tas =
            Parser.oneOf
                [ Parser.succeed (\ta -> Parser.Loop (ta :: tas))
                    |= torsionAngleParser
                    |. Parser.oneOf
                        [ Parser.symbol ")"
                        , Parser.end
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done tas)
                ]
    in
    Parser.loop [] torsionAnglesHelp


torsionAngleParser : Parser.Parser ( String, ( Float, Float, Float ) )
torsionAngleParser =
    let
        negativeFloatParser =
            Parser.oneOf
                [ Parser.succeed negate
                    |. Parser.symbol "-"
                    |= Parser.float
                , Parser.float
                ]
    in
    Parser.succeed (\id om ph ps -> ( id, ( om, ph, ps ) ))
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
           )
        |. Parser.symbol "("
        |= negativeFloatParser
        |. Parser.symbol ","
        |= negativeFloatParser
        |. Parser.symbol ","
        |= negativeFloatParser



-- }}}
-- {{{ Plotting


overviewSpec : String -> Dict String Float -> VL.Spec
overviewSpec metricName overviewMetricDict =
    let
        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Designs"
                    (VL.strs <|
                        Dict.keys overviewMetricDict
                    )
                << VL.dataColumn
                    metricName
                    (VL.nums <|
                        Dict.values overviewMetricDict
                    )

        config =
            (VL.configure
                << VL.configuration (VL.coView [ VL.vicoStroke <| Just "transparent" ])
                << VL.configuration (VL.coAxis [ VL.axcoDomainWidth 1 ])
            )
                []
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 2
        , VL.bar
            []
        , (VL.encoding
            << VL.column
                [ VL.fName "Designs"
                , VL.fMType VL.Nominal
                ]
            << VL.position VL.Y
                [ VL.pName metricName
                , VL.pMType VL.Quantitative
                , VL.pAxis [ VL.axTitle metricName, VL.axGrid True ]
                ]
          )
            []
        , config
        ]


createAllHistogramsSpec : DesignMetrics -> List RefSetMetrics -> VL.Spec
createAllHistogramsSpec designMetrics pdbMetricsList =
    case List.filter (\a -> a.hydrophobicFitness /= Nothing) pdbMetricsList of
        [] ->
            VL.toVegaLite []

        firstPdb :: remainingPdb ->
            let
                pdbData =
                    (List.map metricDataRow remainingPdb
                        |> List.foldl (>>) (metricDataRow firstPdb)
                    )
                        >> VL.dataFromRows []

                designData =
                    metricDataRow designMetrics
                        >> VL.dataFromRows []
            in
            [ VL.asSpec
                [ List.map
                    (histogramSpec (pdbData []) (designData []))
                    [ "Hydrophobic Fitness"
                    , "Isoelectric Point"
                    ]
                    |> VL.hConcat
                ]
            , VL.asSpec
                [ List.map
                    (histogramSpec (pdbData []) (designData []))
                    [ "Mean Packing Density"
                    , "# Residues"
                    ]
                    |> VL.hConcat
                ]
            ]
                |> VL.vConcat
                |> List.singleton
                |> VL.toVegaLite


metricDataRow :
    { a
        | hydrophobicFitness : Maybe Float
        , isoelectricPoint : Float
        , numOfResidues : Int
        , packingDensity : Float
    }
    -> (List VL.DataRow -> List VL.DataRow)
metricDataRow { hydrophobicFitness, isoelectricPoint, numOfResidues, packingDensity } =
    VL.dataRow
        [ ( "Hydrophobic Fitness", VL.num <| Maybe.withDefault 666 hydrophobicFitness )
        , ( "Isoelectric Point", VL.num isoelectricPoint )
        , ( "# Residues", VL.num <| toFloat numOfResidues )
        , ( "Mean Packing Density", VL.num packingDensity )
        ]


histogramSpec : VL.Data -> VL.Data -> String -> VL.Spec
histogramSpec pdbData designData fieldName =
    VL.asSpec
        [ VL.height 225
        , VL.width 225
        , VL.layer
            [ VL.asSpec
                [ pdbData
                , VL.bar []
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName fieldName
                        , VL.pBin [ VL.biBase 10, VL.biDivide [ 4, 2 ] ]
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle fieldName ]
                        ]
                    << VL.position VL.Y
                        [ VL.pAggregate VL.opCount
                        , VL.pMType VL.Quantitative
                        ]
                  )
                    []
                ]
            , VL.asSpec
                [ designData
                , VL.rule [ VL.maSize 3 ]
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName fieldName
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "" ]
                        ]
                  )
                    []
                ]
            ]
        ]


compositionLabels : List String
compositionLabels =
    [ "A"
    , "C"
    , "D"
    , "E"
    , "F"
    , "G"
    , "H"
    , "I"
    , "K"
    , "L"
    , "M"
    , "N"
    , "P"
    , "Q"
    , "R"
    , "S"
    , "T"
    , "V"
    , "W"
    , "X"
    , "Y"
    ]


compositionDictWithDefaultValues : Dict String Float -> Dict String Float
compositionDictWithDefaultValues inputDict =
    let
        getWithDefault dict k =
            Dict.get k dict
                |> Maybe.withDefault 0
                |> Tuple.pair k
    in
    List.map (getWithDefault inputDict) compositionLabels
        |> Dict.fromList


calculateMeanComposition : List (Dict String Float) -> Dict String (Maybe MeanAndStdDev)
calculateMeanComposition compositionList =
    compositionList
        |> List.foldl
            (\dictB dictA ->
                Dict.merge
                    (\key a -> Dict.insert key a)
                    (\key a b -> Dict.insert key (b :: a))
                    (\key b -> Dict.insert key [ b ])
                    dictA
                    dictB
                    Dict.empty
            )
            Dict.empty
        |> Dict.toList
        |> List.map
            (Tuple.mapSecond
                (\v ->
                    case ( Stats.mean v, Stats.stdDeviation v ) of
                        ( Just mean, Just stdDev ) ->
                            Just
                                { mean = mean
                                , stdDev = stdDev
                                }

                        _ ->
                            Nothing
                )
            )
        |> Dict.fromList


createCompositionSpec :
    AggregateData
    -> DesignMetrics
    -> VL.Spec
createCompositionSpec aggregateData designMetrics =
    let
        designComposition =
            designMetrics.composition
                |> compositionDictWithDefaultValues
    in
    compositionSpec designComposition
        (Dict.toList aggregateData.meanComposition
            |> List.map
                (\( k, v ) -> ( k, Maybe.map .mean v |> Maybe.withDefault 0 ))
            |> Dict.fromList
        )


compositionSpec : Dict String Float -> Dict String Float -> VL.Spec
compositionSpec designCompositionDict pdbCompositionDict =
    let
        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Amino Acids"
                    (VL.strs <|
                        List.concat
                            [ Dict.keys designCompositionDict
                            , Dict.keys pdbCompositionDict
                            ]
                    )
                << VL.dataColumn
                    "Proportion"
                    (VL.nums <|
                        List.concat
                            [ Dict.values designCompositionDict
                            , Dict.values pdbCompositionDict
                            ]
                    )
                << VL.dataColumn
                    "Set"
                    (VL.strs <|
                        List.concat
                            [ List.repeat (List.length <| Dict.keys designCompositionDict) "Design"
                            , List.repeat (List.length <| Dict.keys pdbCompositionDict)
                                "Reference"
                            ]
                    )

        config =
            (VL.configure
                << VL.configuration (VL.coView [ VL.vicoStroke <| Just "transparent" ])
                << VL.configuration (VL.coAxis [ VL.axcoDomainWidth 1 ])
            )
                []
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 2
        , VL.bar
            []
        , (VL.encoding
            << VL.column
                [ VL.fName "Amino Acids"
                , VL.fMType VL.Nominal
                ]
            << VL.position VL.Y
                [ VL.pName "Proportion"
                , VL.pAggregate VL.opSum
                , VL.pMType VL.Quantitative
                , VL.pAxis [ VL.axTitle "Amino Acid Proportion", VL.axGrid True ]
                ]
            << VL.position VL.X
                [ VL.pName "Set"
                , VL.pMType VL.Nominal
                , VL.pAxis [ VL.axTitle "" ]
                , VL.pScale [ VL.scRangeStep <| Just 12 ]
                ]
            << VL.color [ VL.mName "Set", VL.mMType VL.Nominal, VL.mLegend [] ]
          )
            []
        , config
        ]


type alias TorsionAnglesDict =
    Dict String ( Float, Float, Float )


createTorsionAngleSpec : DesignMetrics -> List RefSetMetrics -> VL.Spec
createTorsionAngleSpec designMetrics referenceSetMetricsList =
    torsionAnglesSpec
        designMetrics.torsionAngles
        (List.map .torsionAngles referenceSetMetricsList)


torsionAnglesSpec : TorsionAnglesDict -> List TorsionAnglesDict -> VL.Spec
torsionAnglesSpec designTorsionAngles pdbTorsionAnglesDicts =
    let
        designIds =
            Dict.keys designTorsionAngles

        ( designOms, designPhis, designPsis ) =
            Dict.values designTorsionAngles
                |> ListExtra.unzipTriples

        designData =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Omega"
                    (VL.nums designOms)
                << VL.dataColumn
                    "Phi"
                    (VL.nums designPhis)
                << VL.dataColumn
                    "Psi"
                    (VL.nums designPsis)
                << VL.dataColumn
                    "Residue ID"
                    (VL.strs designIds)

        ( pdbOms, pdbPhis, pdbPsis ) =
            List.map Dict.values pdbTorsionAnglesDicts
                |> List.concat
                |> ListExtra.unzipTriples

        pdbData =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Omega"
                    (VL.nums pdbOms)
                << VL.dataColumn
                    "Phi"
                    (VL.nums pdbPhis)
                << VL.dataColumn
                    "Psi"
                    (VL.nums pdbPsis)

        sel =
            VL.selection
                << VL.select "grid"
                    VL.seInterval
                    [ VL.seBindScales
                    , VL.seZoom
                        "wheel![event.shiftKey]"
                    ]
    in
    VL.toVegaLite
        [ VL.vConcat
            [ VL.asSpec
                [ VL.height 400
                , VL.width 400
                , VL.layer
                    [ VL.asSpec
                        [ pdbData []
                        , VL.rect []
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pBin [ VL.biStep 4 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Phi" ]
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pBin [ VL.biStep 4 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Psi" ]
                                ]
                            << VL.color
                                [ VL.mAggregate VL.opCount
                                , VL.mMType VL.Quantitative
                                , VL.mLegend [ VL.leTitle "PDB Counts" ]
                                ]
                          )
                            []
                        ]
                    , VL.asSpec
                        [ designData []
                        , sel []
                        , VL.point
                            [ VL.maTooltip VL.ttData ]
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pMType VL.Quantitative
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pMType VL.Quantitative
                                ]
                            << VL.color [ VL.mStr "#666" ]
                          )
                            []
                        ]
                    ]
                ]
            , VL.asSpec
                [ VL.width 400
                , designData []
                , sel []
                , VL.tick
                    [ VL.maTooltip VL.ttData ]
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName "Omega"
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "Design Omega Values" ]
                        ]
                  )
                    []
                ]
            , VL.asSpec
                [ VL.width 400
                , pdbData []
                , sel []
                , VL.tick []
                , (VL.encoding
                    << VL.position VL.X
                        [ VL.pName "Omega"
                        , VL.pMType VL.Quantitative
                        , VL.pAxis [ VL.axTitle "PDB Omega Values" ]
                        ]
                  )
                    []
                ]
            ]
        ]



-- }}}
