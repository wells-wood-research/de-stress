module Shared.Metrics exposing
    ( AggregateData
    , Aggrescan3DResults
    , BudeFFResults
    , DFIRE2Results
    , DesignMetrics
    , EvoEF2Results
    , MeanMedAndStdDev
    , RefSetMetrics
    , RosettaResults
    , SequenceInfo
    , aggregateDataCodec
    , calculateMeanComposition
    , compositionStringToDict
    , createAggregateData
    , createAllHistogramsSpec
    , createCompositionSpec
    , createTorsionAngleSpec
    , desMetricsCodec
    , makeHistPlotData
    , refSetMetricsCodec
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
    , budeFFResults : BudeFFResults
    , evoEF2Results : EvoEF2Results
    , dfire2Results : DFIRE2Results
    , rosettaResults : RosettaResults
    , aggrescan3dResults : Aggrescan3DResults
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
        |> Codec.field "budeFFResults" .budeFFResults budeFFResultsCodec
        |> Codec.field "evoEF2Results" .evoEF2Results evoEF2ResultsCodec
        |> Codec.field "dfire2Results" .dfire2Results dfire2ResultsCodec
        |> Codec.field "rosettaResults" .rosettaResults rosettaResultsCodec
        |> Codec.field "aggrescan3dResults" .aggrescan3dResults aggrescan3DResultsCodec
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
-- {{{ BudeFFResults


type alias BudeFFResults =
    { totalEnergy : Maybe Float
    , steric : Maybe Float
    , desolvation : Maybe Float
    , charge : Maybe Float
    }


budeFFResultsCodec : Codec BudeFFResults
budeFFResultsCodec =
    Codec.object BudeFFResults
        |> Codec.field "totalEnergy" .totalEnergy (Codec.maybe Codec.float)
        |> Codec.field "steric" .steric (Codec.maybe Codec.float)
        |> Codec.field "desolvation" .desolvation (Codec.maybe Codec.float)
        |> Codec.field "charge" .charge (Codec.maybe Codec.float)
        |> Codec.buildObject



-- }}}
-- {{{ EvoEF2Results


type alias EvoEF2Results =
    { log_info : String
    , error_info : String
    , return_code : Int
    , reference_ALA : Maybe Float
    , reference_CYS : Maybe Float
    , reference_ASP : Maybe Float
    , reference_GLU : Maybe Float
    , reference_PHE : Maybe Float
    , reference_GLY : Maybe Float
    , reference_HIS : Maybe Float
    , reference_ILE : Maybe Float
    , reference_LYS : Maybe Float
    , reference_LEU : Maybe Float
    , reference_MET : Maybe Float
    , reference_ASN : Maybe Float
    , reference_PRO : Maybe Float
    , reference_GLN : Maybe Float
    , reference_ARG : Maybe Float
    , reference_SER : Maybe Float
    , reference_THR : Maybe Float
    , reference_VAL : Maybe Float
    , reference_TRP : Maybe Float
    , reference_TYR : Maybe Float
    , intraR_vdwatt : Maybe Float
    , intraR_vdwrep : Maybe Float
    , intraR_electr : Maybe Float
    , intraR_deslvP : Maybe Float
    , intraR_deslvH : Maybe Float
    , intraR_hbscbb_dis : Maybe Float
    , intraR_hbscbb_the : Maybe Float
    , intraR_hbscbb_phi : Maybe Float
    , aapropensity : Maybe Float
    , ramachandran : Maybe Float
    , dunbrack : Maybe Float
    , interS_vdwatt : Maybe Float
    , interS_vdwrep : Maybe Float
    , interS_electr : Maybe Float
    , interS_deslvP : Maybe Float
    , interS_deslvH : Maybe Float
    , interS_ssbond : Maybe Float
    , interS_hbbbbb_dis : Maybe Float
    , interS_hbbbbb_the : Maybe Float
    , interS_hbbbbb_phi : Maybe Float
    , interS_hbscbb_dis : Maybe Float
    , interS_hbscbb_the : Maybe Float
    , interS_hbscbb_phi : Maybe Float
    , interS_hbscsc_dis : Maybe Float
    , interS_hbscsc_the : Maybe Float
    , interS_hbscsc_phi : Maybe Float
    , interD_vdwatt : Maybe Float
    , interD_vdwrep : Maybe Float
    , interD_electr : Maybe Float
    , interD_deslvP : Maybe Float
    , interD_deslvH : Maybe Float
    , interD_ssbond : Maybe Float
    , interD_hbbbbb_dis : Maybe Float
    , interD_hbbbbb_the : Maybe Float
    , interD_hbbbbb_phi : Maybe Float
    , interD_hbscbb_dis : Maybe Float
    , interD_hbscbb_the : Maybe Float
    , interD_hbscbb_phi : Maybe Float
    , interD_hbscsc_dis : Maybe Float
    , interD_hbscsc_the : Maybe Float
    , interD_hbscsc_phi : Maybe Float
    , total : Maybe Float
    , ref_total : Maybe Float
    , intraR_total : Maybe Float
    , interS_total : Maybe Float
    , interD_total : Maybe Float
    }


evoEF2ResultsCodec : Codec EvoEF2Results
evoEF2ResultsCodec =
    Codec.object EvoEF2Results
        |> Codec.field "log_info" .log_info Codec.string
        |> Codec.field "error_info" .error_info Codec.string
        |> Codec.field "return_code" .return_code Codec.int
        |> Codec.field "reference_ALA" .reference_ALA (Codec.maybe Codec.float)
        |> Codec.field "reference_CYS" .reference_CYS (Codec.maybe Codec.float)
        |> Codec.field "reference_ASP" .reference_ASP (Codec.maybe Codec.float)
        |> Codec.field "reference_GLU" .reference_GLU (Codec.maybe Codec.float)
        |> Codec.field "reference_PHE" .reference_PHE (Codec.maybe Codec.float)
        |> Codec.field "reference_GLY" .reference_GLY (Codec.maybe Codec.float)
        |> Codec.field "reference_HIS" .reference_HIS (Codec.maybe Codec.float)
        |> Codec.field "reference_ILE" .reference_ILE (Codec.maybe Codec.float)
        |> Codec.field "reference_LYS" .reference_LYS (Codec.maybe Codec.float)
        |> Codec.field "reference_LEU" .reference_LEU (Codec.maybe Codec.float)
        |> Codec.field "reference_MET" .reference_MET (Codec.maybe Codec.float)
        |> Codec.field "reference_ASN" .reference_ASN (Codec.maybe Codec.float)
        |> Codec.field "reference_PRO" .reference_PRO (Codec.maybe Codec.float)
        |> Codec.field "reference_GLN" .reference_GLN (Codec.maybe Codec.float)
        |> Codec.field "reference_ARG" .reference_ARG (Codec.maybe Codec.float)
        |> Codec.field "reference_SER" .reference_SER (Codec.maybe Codec.float)
        |> Codec.field "reference_THR" .reference_THR (Codec.maybe Codec.float)
        |> Codec.field "reference_VAL" .reference_VAL (Codec.maybe Codec.float)
        |> Codec.field "reference_TRP" .reference_TRP (Codec.maybe Codec.float)
        |> Codec.field "reference_TYR" .reference_TYR (Codec.maybe Codec.float)
        |> Codec.field "intraR_vdwatt" .intraR_vdwatt (Codec.maybe Codec.float)
        |> Codec.field "intraR_vdwrep" .intraR_vdwrep (Codec.maybe Codec.float)
        |> Codec.field "intraR_electr" .intraR_electr (Codec.maybe Codec.float)
        |> Codec.field "intraR_deslvP" .intraR_deslvP (Codec.maybe Codec.float)
        |> Codec.field "intraR_deslvH" .intraR_deslvH (Codec.maybe Codec.float)
        |> Codec.field "intraR_hbscbb_dis" .intraR_hbscbb_dis (Codec.maybe Codec.float)
        |> Codec.field "intraR_hbscbb_the" .intraR_hbscbb_the (Codec.maybe Codec.float)
        |> Codec.field "intraR_hbscbb_phi" .intraR_hbscbb_phi (Codec.maybe Codec.float)
        |> Codec.field "aapropensity" .aapropensity (Codec.maybe Codec.float)
        |> Codec.field "ramachandran" .ramachandran (Codec.maybe Codec.float)
        |> Codec.field "dunbrack" .dunbrack (Codec.maybe Codec.float)
        |> Codec.field "interS_vdwatt" .interS_vdwatt (Codec.maybe Codec.float)
        |> Codec.field "interS_vdwrep" .interS_vdwrep (Codec.maybe Codec.float)
        |> Codec.field "interS_electr" .interS_electr (Codec.maybe Codec.float)
        |> Codec.field "interS_deslvP" .interS_deslvP (Codec.maybe Codec.float)
        |> Codec.field "interS_deslvH" .interS_deslvH (Codec.maybe Codec.float)
        |> Codec.field "interS_ssbond" .interS_ssbond (Codec.maybe Codec.float)
        |> Codec.field "interS_hbbbbb_dis" .interS_hbbbbb_dis (Codec.maybe Codec.float)
        |> Codec.field "interS_hbbbbb_the" .interS_hbbbbb_the (Codec.maybe Codec.float)
        |> Codec.field "interS_hbbbbb_phi" .interS_hbbbbb_phi (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscbb_dis" .interS_hbscbb_dis (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscbb_the" .interS_hbscbb_the (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscbb_phi" .interS_hbscbb_phi (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscsc_dis" .interS_hbscsc_dis (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscsc_the" .interS_hbscsc_the (Codec.maybe Codec.float)
        |> Codec.field "interS_hbscsc_phi" .interS_hbscsc_phi (Codec.maybe Codec.float)
        |> Codec.field "interD_vdwatt" .interD_vdwatt (Codec.maybe Codec.float)
        |> Codec.field "interD_vdwrep" .interD_vdwrep (Codec.maybe Codec.float)
        |> Codec.field "interD_electr" .interD_electr (Codec.maybe Codec.float)
        |> Codec.field "interD_deslvP" .interD_deslvP (Codec.maybe Codec.float)
        |> Codec.field "interD_deslvH" .interD_deslvH (Codec.maybe Codec.float)
        |> Codec.field "interD_ssbond" .interD_ssbond (Codec.maybe Codec.float)
        |> Codec.field "interD_hbbbbb_dis" .interD_hbbbbb_dis (Codec.maybe Codec.float)
        |> Codec.field "interD_hbbbbb_the" .interD_hbbbbb_the (Codec.maybe Codec.float)
        |> Codec.field "interD_hbbbbb_phi" .interD_hbbbbb_phi (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscbb_dis" .interD_hbscbb_dis (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscbb_the" .interD_hbscbb_the (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscbb_phi" .interD_hbscbb_phi (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscsc_dis" .interD_hbscsc_dis (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscsc_the" .interD_hbscsc_the (Codec.maybe Codec.float)
        |> Codec.field "interD_hbscsc_phi" .interD_hbscsc_phi (Codec.maybe Codec.float)
        |> Codec.field "total" .total (Codec.maybe Codec.float)
        |> Codec.field "ref_total" .ref_total (Codec.maybe Codec.float)
        |> Codec.field "intraR_total" .intraR_total (Codec.maybe Codec.float)
        |> Codec.field "interS_total" .interS_total (Codec.maybe Codec.float)
        |> Codec.field "interD_total" .interD_total (Codec.maybe Codec.float)
        |> Codec.buildObject



-- }}}
-- {{{ DFIRE2Results


type alias DFIRE2Results =
    { log_info : String
    , error_info : String
    , return_code : Int
    , total : Maybe Float
    }


dfire2ResultsCodec : Codec DFIRE2Results
dfire2ResultsCodec =
    Codec.object DFIRE2Results
        |> Codec.field "log_info" .log_info Codec.string
        |> Codec.field "error_info" .error_info Codec.string
        |> Codec.field "return_code" .return_code Codec.int
        |> Codec.field "total" .total (Codec.maybe Codec.float)
        |> Codec.buildObject



-- }}}
-- {{{ RosettaResults


type alias RosettaResults =
    { log_info : String
    , error_info : String
    , return_code : Int
    , dslf_fa13 : Maybe Float
    , fa_atr : Maybe Float
    , fa_dun : Maybe Float
    , fa_elec : Maybe Float
    , fa_intra_rep : Maybe Float
    , fa_intra_sol_xover4 : Maybe Float
    , fa_rep : Maybe Float
    , fa_sol : Maybe Float
    , hbond_bb_sc : Maybe Float
    , hbond_lr_bb : Maybe Float
    , hbond_sc : Maybe Float
    , hbond_sr_bb : Maybe Float
    , linear_chainbreak : Maybe Float
    , lk_ball_wtd : Maybe Float
    , omega : Maybe Float
    , overlap_chainbreak : Maybe Float
    , p_aa_pp : Maybe Float
    , pro_close : Maybe Float
    , rama_prepro : Maybe Float
    , ref : Maybe Float
    , score : Maybe Float
    , time : Maybe Float
    , total_score : Maybe Float
    , yhh_planarity : Maybe Float
    }


rosettaResultsCodec : Codec RosettaResults
rosettaResultsCodec =
    Codec.object RosettaResults
        |> Codec.field "log_info" .log_info Codec.string
        |> Codec.field "error_info" .error_info Codec.string
        |> Codec.field "return_code" .return_code Codec.int
        |> Codec.field "dslf_fa13" .dslf_fa13 (Codec.maybe Codec.float)
        |> Codec.field "fa_atr" .fa_atr (Codec.maybe Codec.float)
        |> Codec.field "fa_dun" .fa_dun (Codec.maybe Codec.float)
        |> Codec.field "fa_elec" .fa_elec (Codec.maybe Codec.float)
        |> Codec.field "fa_intra_rep" .fa_intra_rep (Codec.maybe Codec.float)
        |> Codec.field "fa_intra_sol_xover4" .fa_intra_sol_xover4 (Codec.maybe Codec.float)
        |> Codec.field "fa_rep" .fa_rep (Codec.maybe Codec.float)
        |> Codec.field "fa_sol" .fa_sol (Codec.maybe Codec.float)
        |> Codec.field "hbond_bb_sc" .hbond_bb_sc (Codec.maybe Codec.float)
        |> Codec.field "hbond_lr_bb" .hbond_lr_bb (Codec.maybe Codec.float)
        |> Codec.field "hbond_sc" .hbond_sc (Codec.maybe Codec.float)
        |> Codec.field "hbond_sr_bb" .hbond_sr_bb (Codec.maybe Codec.float)
        |> Codec.field "linear_chainbreak" .linear_chainbreak (Codec.maybe Codec.float)
        |> Codec.field "lk_ball_wtd" .lk_ball_wtd (Codec.maybe Codec.float)
        |> Codec.field "omega" .omega (Codec.maybe Codec.float)
        |> Codec.field "overlap_chainbreak" .overlap_chainbreak (Codec.maybe Codec.float)
        |> Codec.field "p_aa_pp" .p_aa_pp (Codec.maybe Codec.float)
        |> Codec.field "pro_close" .pro_close (Codec.maybe Codec.float)
        |> Codec.field "rama_prepro" .rama_prepro (Codec.maybe Codec.float)
        |> Codec.field "ref" .ref (Codec.maybe Codec.float)
        |> Codec.field "score" .score (Codec.maybe Codec.float)
        |> Codec.field "time" .time (Codec.maybe Codec.float)
        |> Codec.field "total_score" .total_score (Codec.maybe Codec.float)
        |> Codec.field "yhh_planarity" .yhh_planarity (Codec.maybe Codec.float)
        |> Codec.buildObject



-- }}}
-- {{{ Aggrescan3DResults


type alias Aggrescan3DResults =
    { log_info : String
    , error_info : String
    , return_code : Int
    , protein_list : Maybe String
    , chain_list : Maybe String
    , residue_number_list : Maybe String
    , residue_name_list : Maybe String
    , residue_score_list : Maybe String
    , max_value : Maybe Float
    , avg_value : Maybe Float
    , min_value : Maybe Float
    , total_value : Maybe Float
    }


aggrescan3DResultsCodec : Codec Aggrescan3DResults
aggrescan3DResultsCodec =
    Codec.object Aggrescan3DResults
        |> Codec.field "log_info" .log_info Codec.string
        |> Codec.field "error_info" .error_info Codec.string
        |> Codec.field "return_code" .return_code Codec.int
        |> Codec.field "protein_list" .protein_list (Codec.maybe Codec.string)
        |> Codec.field "chain_list" .chain_list (Codec.maybe Codec.string)
        |> Codec.field "residue_number_list" .residue_number_list (Codec.maybe Codec.string)
        |> Codec.field "residue_name_list" .residue_name_list (Codec.maybe Codec.string)
        |> Codec.field "residue_score_list" .residue_score_list (Codec.maybe Codec.string)
        |> Codec.field "max_value" .max_value (Codec.maybe Codec.float)
        |> Codec.field "avg_value" .avg_value (Codec.maybe Codec.float)
        |> Codec.field "min_value" .min_value (Codec.maybe Codec.float)
        |> Codec.field "total_value" .total_value (Codec.maybe Codec.float)
        |> Codec.buildObject



-- }}}
-- {{{ RefSetMetrics


type alias RefSetMetrics =
    { pdbCode : String
    , composition : Dict String Float
    , torsionAngles : Dict String ( Float, Float, Float )
    , hydrophobicFitness : Maybe Float
    , isoelectricPoint : Float
    , mass : Float
    , numberOfResidues : Int
    , packingDensity : Float
    , budeFFTotalEnergy : Maybe Float
    , evoEFTotalEnergy : Maybe Float
    , dfireTotalEnergy : Maybe Float
    , rosettaTotalEnergy : Maybe Float
    , aggrescan3dTotalValue : Maybe Float
    }


refSetMetricsCodec : Codec RefSetMetrics
refSetMetricsCodec =
    Codec.object RefSetMetrics
        |> Codec.field "pdbCode" .pdbCode Codec.string
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
        |> Codec.field "numberOfResidues" .numberOfResidues Codec.int
        |> Codec.field "packingDensity" .packingDensity Codec.float
        |> Codec.field "budeFFTotalEnergy" .budeFFTotalEnergy (Codec.maybe Codec.float)
        |> Codec.field "evoEFTotalEnergy" .evoEFTotalEnergy (Codec.maybe Codec.float)
        |> Codec.field "dfireTotalEnergy" .dfireTotalEnergy (Codec.maybe Codec.float)
        |> Codec.field "rosettaTotalEnergy" .rosettaTotalEnergy (Codec.maybe Codec.float)
        |> Codec.field "aggrescan3dTotalValue" .aggrescan3dTotalValue (Codec.maybe Codec.float)
        |> Codec.buildObject


type alias AggregateData =
    { composition : Dict String (Maybe MeanMedAndStdDev)
    , hydrophobicFitness : Maybe MeanMedAndStdDev
    , isoelectricPoint : Maybe MeanMedAndStdDev
    , numberOfResidues : Maybe MeanMedAndStdDev
    , packingDensity : Maybe MeanMedAndStdDev
    , budeFFTotalEnergy : Maybe MeanMedAndStdDev
    , evoEFTotalEnergy : Maybe MeanMedAndStdDev
    , dfireTotalEnergy : Maybe MeanMedAndStdDev
    , rosettaTotalEnergy : Maybe MeanMedAndStdDev
    , aggrescan3dTotalValue : Maybe MeanMedAndStdDev
    }


type alias MeanMedAndStdDev =
    { mean : Float
    , median : Float
    , stdDev : Float
    }


createAggregateData : List RefSetMetrics -> AggregateData
createAggregateData refSetMetricsList =
    { composition =
        refSetMetricsList
            |> List.map .composition
            |> calculateMeanComposition
    , hydrophobicFitness =
        refSetMetricsList
            |> List.filterMap .hydrophobicFitness
            |> calculateMeanMedAndStdDev
    , isoelectricPoint =
        refSetMetricsList
            |> List.map .isoelectricPoint
            |> calculateMeanMedAndStdDev
    , numberOfResidues =
        refSetMetricsList
            |> List.map .numberOfResidues
            |> List.map toFloat
            |> calculateMeanMedAndStdDev
    , packingDensity =
        refSetMetricsList
            |> List.map .packingDensity
            |> calculateMeanMedAndStdDev
    , budeFFTotalEnergy =
        refSetMetricsList
            |> List.filterMap .budeFFTotalEnergy
            |> calculateMeanMedAndStdDev
    , evoEFTotalEnergy =
        refSetMetricsList
            |> List.filterMap .evoEFTotalEnergy
            |> calculateMeanMedAndStdDev
    , dfireTotalEnergy =
        refSetMetricsList
            |> List.filterMap .dfireTotalEnergy
            |> calculateMeanMedAndStdDev
    , rosettaTotalEnergy =
        refSetMetricsList
            |> List.filterMap .rosettaTotalEnergy
            |> calculateMeanMedAndStdDev
    , aggrescan3dTotalValue =
        refSetMetricsList
            |> List.filterMap .aggrescan3dTotalValue
            |> calculateMeanMedAndStdDev
    }


aggregateDataCodec : Codec AggregateData
aggregateDataCodec =
    Codec.object AggregateData
        |> Codec.field "composition" .composition (Codec.dict (Codec.maybe meanAndStdDevCodec))
        |> Codec.field "hydrophobicFitness" .hydrophobicFitness (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "isoelectricPoint" .isoelectricPoint (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "numberOfResidues" .numberOfResidues (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "packingDensity" .packingDensity (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "budeFFTotalEnergy" .budeFFTotalEnergy (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "evoEFTotalEnergy" .evoEFTotalEnergy (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "dfireTotalEnergy" .dfireTotalEnergy (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "rosettaTotalEnergy" .rosettaTotalEnergy (Codec.maybe meanAndStdDevCodec)
        |> Codec.field "aggrescan3dTotalValue" .aggrescan3dTotalValue (Codec.maybe meanAndStdDevCodec)
        |> Codec.buildObject


meanAndStdDevCodec : Codec MeanMedAndStdDev
meanAndStdDevCodec =
    Codec.object MeanMedAndStdDev
        |> Codec.field "mean" .mean Codec.float
        |> Codec.field "median" .median Codec.float
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


type alias HistPlotData =
    { hydrophobicFitness : Float
    , isoelectricPoint : Float
    , mass : Float
    , numberOfResidues : Float
    , packingDensity : Float
    , budeFFTotalEnergy : Float
    , evoEFTotalEnergy : Float
    , dfireTotalEnergy : Float
    , rosettaTotalEnergy : Float
    , aggrescan3dTotalValue : Float
    }


makeHistPlotData :
    { a
        | hydrophobicFitness : Maybe Float
        , isoelectricPoint : Float
        , mass : Float
        , numberOfResidues : Int
        , packingDensity : Float
        , budeFFTotalEnergy : Maybe Float
        , evoEFTotalEnergy : Maybe Float
        , dfireTotalEnergy : Maybe Float
        , rosettaTotalEnergy : Maybe Float
        , aggrescan3dTotalValue : Maybe Float
    }
    -> HistPlotData
makeHistPlotData metrics =
    { hydrophobicFitness = Maybe.withDefault (0 / 0) metrics.hydrophobicFitness
    , isoelectricPoint = metrics.isoelectricPoint
    , mass = metrics.mass
    , numberOfResidues = metrics.numberOfResidues |> toFloat
    , packingDensity = metrics.packingDensity
    , budeFFTotalEnergy =
        Maybe.withDefault (0 / 0) metrics.budeFFTotalEnergy
    , evoEFTotalEnergy =
        Maybe.withDefault (0 / 0) metrics.evoEFTotalEnergy
    , dfireTotalEnergy =
        Maybe.withDefault (0 / 0) metrics.dfireTotalEnergy
    , rosettaTotalEnergy =
        Maybe.withDefault (0 / 0) metrics.rosettaTotalEnergy
    , aggrescan3dTotalValue =
        Maybe.withDefault (0 / 0) metrics.aggrescan3dTotalValue
    }


histPlotTuples : List ( String, HistPlotData -> Float )
histPlotTuples =
    [ ( "Hydrophobic Fitness", .hydrophobicFitness )
    , ( "Isoelectric Point", .isoelectricPoint )
    , ( "Mass (Da)", .mass )
    , ( "Number of Residues", .numberOfResidues )
    , ( "Mean Packing Density", .packingDensity )
    , ( "BUDE FF Total Energy", .budeFFTotalEnergy )
    , ( "EvoEF2 Total Energy", .evoEFTotalEnergy )
    , ( "dFire2 Total Energy", .dfireTotalEnergy )
    , ( "Rosetta Total Energy", .rosettaTotalEnergy )
    , ( "Aggrescan3D Total Value", .aggrescan3dTotalValue )
    ]


createAllHistogramsSpec : Device -> List HistPlotData -> List HistPlotData -> VL.Spec
createAllHistogramsSpec device designHistPlotData refSetHistPlotData =
    let
        refSetMetricValueColumn ( label, valueFn ) =
            VL.dataColumn
                label
                (VL.nums <|
                    List.map valueFn refSetHistPlotData
                )

        designMetricValueColumn ( label, valueFn ) =
            VL.dataColumn
                ("design_" ++ label)
                (VL.nums <|
                    List.map valueFn designHistPlotData
                )

        refSetData =
            VL.dataFromColumns []
                << (List.map refSetMetricValueColumn histPlotTuples
                        |> List.foldr (<<) identity
                   )

        designSetData =
            VL.dataFromColumns []
                << (List.map designMetricValueColumn histPlotTuples
                        |> List.foldr (<<) identity
                   )

        histogramSpec label =
            VL.asSpec
                [ VL.width 200
                , VL.layer
                    (VL.asSpec
                        [ VL.bar
                            [ VL.maTooltip VL.ttEncoding
                            ]
                        , refSetData []
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName label
                                , VL.pBin [ VL.biBase 10, VL.biDivide [ 4, 2 ] ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle label ]
                                ]
                            << VL.position VL.Y
                                [ VL.pAggregate VL.opCount
                                , VL.pMType VL.Quantitative
                                ]
                          )
                            []
                        ]
                        :: (if List.isEmpty designHistPlotData then
                                []

                            else
                                [ VL.asSpec
                                    [ VL.rule
                                        [ VL.maSize 3
                                        , VL.maTooltip VL.ttEncoding
                                        ]
                                    , designSetData []
                                    , (VL.encoding
                                        << VL.position VL.X
                                            [ VL.pName ("design_" ++ label)
                                            , VL.pMType VL.Quantitative
                                            ]
                                      )
                                        []
                                    ]
                                ]
                           )
                    )
                ]

        ( headTuples, tailTuples ) =
            List.indexedMap Tuple.pair histPlotTuples
                |> List.partition (\( n, _ ) -> n < (List.length histPlotTuples // 2))
                |> (\( a, b ) -> ( List.map Tuple.second a, List.map Tuple.second b ))
    in
    VL.toVegaLite
        [ VL.spacing 40
        , case ( device.class, device.orientation ) of
            ( Phone, Portrait ) ->
                histPlotTuples
                    |> List.map Tuple.first
                    |> List.map histogramSpec
                    |> VL.vConcat

            _ ->
                VL.hConcat
                    [ VL.asSpec
                        [ headTuples
                            |> List.map Tuple.first
                            |> List.map histogramSpec
                            |> VL.vConcat
                        ]
                    , VL.asSpec
                        [ tailTuples
                            |> List.map Tuple.first
                            |> List.map histogramSpec
                            |> VL.vConcat
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


calculateMeanComposition : List (Dict String Float) -> Dict String (Maybe MeanMedAndStdDev)
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
                (\v -> calculateMeanMedAndStdDev v)
            )
        |> Dict.fromList


calculateMeanMedAndStdDev : List Float -> Maybe MeanMedAndStdDev
calculateMeanMedAndStdDev values =
    case ( Stats.mean values, Stats.median values, Stats.stdDeviation values ) of
        ( Just mean, Just median, Just stdDev ) ->
            Just
                { mean = mean
                , median = median
                , stdDev = stdDev
                }

        _ ->
            Nothing


createCompositionSpec :
    Device
    -> AggregateData
    -> Maybe DesignMetrics
    -> VL.Spec
createCompositionSpec device aggregateData mDesignMetrics =
    case mDesignMetrics of
        Just designMetrics ->
            let
                designComposition =
                    designMetrics.composition
                        |> compositionDictWithDefaultValues
            in
            compositionSpec
                device
                (Just designComposition)
                (Dict.toList aggregateData.composition
                    |> List.map
                        (\( k, v ) -> ( k, Maybe.map .median v |> Maybe.withDefault 0 ))
                    |> Dict.fromList
                )

        Nothing ->
            compositionSpec
                device
                Nothing
                (Dict.toList aggregateData.composition
                    |> List.map
                        (\( k, v ) -> ( k, Maybe.map .median v |> Maybe.withDefault 0 ))
                    |> Dict.fromList
                )


compositionSpec : Device -> Maybe (Dict String Float) -> Dict String Float -> VL.Spec
compositionSpec device mDesignCompositionDict pdbCompositionDict =
    let
        data =
            VL.dataFromColumns []
                << VL.dataColumn
                    "Amino Acids"
                    (VL.strs <|
                        ((case mDesignCompositionDict of
                            Just designCompositionDict ->
                                Dict.keys designCompositionDict

                            Nothing ->
                                []
                         )
                            ++ Dict.keys pdbCompositionDict
                        )
                    )
                << VL.dataColumn
                    "Proportion"
                    (VL.nums <|
                        (case mDesignCompositionDict of
                            Just designCompositionDict ->
                                Dict.values designCompositionDict

                            Nothing ->
                                []
                        )
                            ++ Dict.values pdbCompositionDict
                    )
                << VL.dataColumn
                    "Set"
                    (VL.strs <|
                        (case mDesignCompositionDict of
                            Just designCompositionDict ->
                                List.repeat
                                    (List.length <| Dict.keys designCompositionDict)
                                    "Design"

                            Nothing ->
                                []
                        )
                            ++ List.repeat
                                (List.length <| Dict.keys pdbCompositionDict)
                                "Median Reference Set Value"
                    )
    in
    VL.toVegaLite
        [ data []
        , VL.spacing 2
        , VL.width <|
            case ( device.class, device.orientation ) of
                ( Phone, Portrait ) ->
                    12

                _ ->
                    25
        , VL.bar
            [ VL.maTooltip VL.ttEncoding
            ]
        , (VL.encoding
            << VL.column [ VL.fName "Amino Acids", VL.fNominal, VL.fSpacing 1 ]
            << VL.position VL.Y
                [ VL.pName "Proportion"
                , VL.pMType VL.Quantitative
                , VL.pAggregate VL.opSum
                , VL.pAxis [ VL.axTitle "Amino Acid Proportion", VL.axGrid False ]
                ]
            << VL.position VL.X
                [ VL.pName "Set"
                , VL.pMType VL.Nominal
                , VL.pAxis []
                ]
            << VL.color
                [ VL.mName "Set"
                , VL.mMType VL.Nominal
                , VL.mLegend
                    [ VL.leTitle "Set"
                    , VL.leOrient VL.loBottom
                    ]
                ]
          )
            []
        ]


type alias TorsionAnglesDict =
    Dict String ( Float, Float, Float )


createTorsionAngleSpec : Device -> Maybe DesignMetrics -> List RefSetMetrics -> VL.Spec
createTorsionAngleSpec device mDesignMetrics referenceSetMetricsList =
    case mDesignMetrics of
        Just designMetrics ->
            fullTorsionAnglesSpec
                device
                designMetrics.torsionAngles
                (List.map .torsionAngles referenceSetMetricsList)

        Nothing ->
            refSetTorsionAnglesSpec
                device
                (List.map .torsionAngles referenceSetMetricsList)


fullTorsionAnglesSpec : Device -> TorsionAnglesDict -> List TorsionAnglesDict -> VL.Spec
fullTorsionAnglesSpec device designTorsionAngles pdbTorsionAnglesDicts =
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

        widthAndHeight =
            case ( device.class, device.orientation ) of
                ( Phone, Portrait ) ->
                    200

                _ ->
                    400
    in
    VL.toVegaLite
        [ VL.vConcat
            [ VL.asSpec
                [ VL.height widthAndHeight
                , VL.width widthAndHeight
                , VL.layer
                    [ VL.asSpec
                        [ pdbData []
                        , VL.rect []
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pBin [ VL.biStep 9 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Phi" ]
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pBin [ VL.biStep 9 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Psi" ]
                                ]
                            << VL.color
                                [ VL.mAggregate VL.opCount
                                , VL.mMType VL.Quantitative
                                , VL.mLegend
                                    [ VL.leTitle "Count"
                                    , VL.leOrient VL.loTop
                                    ]
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
                [ VL.width widthAndHeight
                , designData []
                , sel []
                , VL.tick
                    [ VL.maTooltip VL.ttData
                    , VL.maOpacity 0.2
                    ]
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
                [ VL.width widthAndHeight
                , pdbData []
                , sel []
                , VL.tick
                    [ VL.maOpacity 0.01
                    ]
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


refSetTorsionAnglesSpec : Device -> List TorsionAnglesDict -> VL.Spec
refSetTorsionAnglesSpec device pdbTorsionAnglesDicts =
    let
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

        widthAndHeight =
            case ( device.class, device.orientation ) of
                ( Phone, Portrait ) ->
                    200

                _ ->
                    400
    in
    VL.toVegaLite
        [ VL.vConcat
            [ VL.asSpec
                [ VL.height widthAndHeight
                , VL.width widthAndHeight
                , VL.layer
                    [ VL.asSpec
                        [ pdbData []
                        , VL.rect []
                        , (VL.encoding
                            << VL.position VL.X
                                [ VL.pName "Phi"
                                , VL.pBin [ VL.biStep 9 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Phi" ]
                                ]
                            << VL.position VL.Y
                                [ VL.pName "Psi"
                                , VL.pBin [ VL.biStep 9 ]
                                , VL.pMType VL.Quantitative
                                , VL.pAxis [ VL.axTitle "Psi" ]
                                ]
                            << VL.color
                                [ VL.mAggregate VL.opCount
                                , VL.mMType VL.Quantitative
                                , VL.mLegend
                                    [ VL.leTitle "Count"
                                    , VL.leOrient VL.loTop
                                    ]
                                ]
                          )
                            []
                        ]
                    ]
                ]
            , VL.asSpec
                [ VL.width widthAndHeight
                , pdbData []
                , sel []
                , VL.tick
                    [ VL.maOpacity 0.01
                    ]
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
