module Table exposing (..)

-- Importing packages
import Browser
import Html exposing (Html, Attribute, div, input)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Element.Border
import Numeral
import Array
import Widget
import Widget.Style exposing (ButtonStyle, RowStyle)
import Widget.Style.Material as Material

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }

-- Defining custom types for EvoEF2, Msg, Option and Model that will be used through out the script

type alias EvoEF2 = 
    { log_info: String
    , reference_ALA: Float
    , reference_CYS: Float
    , reference_ASP: Float
    , reference_GLU: Float
    , reference_PHE: Float
    , reference_GLY: Float
    , reference_HIS: Float
    , reference_ILE: Float
    , reference_LYS: Float
    , reference_LEU: Float
    , reference_MET: Float
    , reference_ASN: Float
    , reference_PRO: Float
    , reference_GLN: Float
    , reference_ARG: Float
    , reference_SER: Float
    , reference_THR: Float
    , reference_VAL: Float
    , reference_TRP: Float
    , reference_TYR: Float
    , intraR_vdwatt: Float
    , intraR_vdwrep: Float
    , intraR_electr: Float
    , intraR_deslvP: Float
    , intraR_deslvH: Float
    , intraR_hbscbb_dis: Float
    , intraR_hbscbb_the: Float
    , intraR_hbscbb_phi: Float
    , aapropensity: Float
    , ramachandran: Float
    , dunbrack: Float
    , interS_vdwatt: Float
    , interS_vdwrep: Float
    , interS_electr: Float
    , interS_deslvP: Float
    , interS_deslvH: Float
    , interS_ssbond: Float
    , interS_hbbbbb_dis: Float
    , interS_hbbbbb_the: Float
    , interS_hbbbbb_phi: Float
    , interS_hbscbb_dis: Float
    , interS_hbscbb_the: Float
    , interS_hbscbb_phi: Float
    , interS_hbscsc_dis: Float
    , interS_hbscsc_the: Float
    , interS_hbscsc_phi: Float
    , interD_vdwatt: Float
    , interD_vdwrep: Float
    , interD_electr: Float
    , interD_deslvP: Float
    , interD_deslvH: Float
    , interD_ssbond: Float
    , interD_hbbbbb_dis: Float
    , interD_hbbbbb_the: Float
    , interD_hbbbbb_phi: Float
    , interD_hbscbb_dis: Float
    , interD_hbscbb_the: Float
    , interD_hbscbb_phi: Float
    , interD_hbscsc_dis: Float
    , interD_hbscsc_the: Float
    , interD_hbscsc_phi: Float
    , total: Float
    , ref_total: Float
    , intraR_total: Float
    , interS_total: Float
    , interD_total: Float
   }


type Msg
    = ChangeSelected Option


type Option 
    = Summary|Reference|IntraR|InterS|InterD



type alias Model
    = { selected_option: Option 
      , selected_columns: List (Column EvoEF2 Msg)
      , selected_sub_title: String }


-- Defining some evoef2_results which will be displayed in the table
evoef2_results: EvoEF2
evoef2_results = 
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
    , ref_total  = -21.77
    , intraR_total  = -18.04
    , interS_total  = -670.02
    , interD_total  = 0.0
 }



evoef2_summary_columns = [ { header = Element.text "Total EvoEF2 \nEnergy"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.total)) } 
                        , { header = Element.text "Reference \nEnergy"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.ref_total)) } 
                        , { header = Element.text "AA Propensity \nEnergy"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.aapropensity)) } 
                        , { header = Element.text "Ramachandran \nEnergy"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.ramachandran)) }
                         , { header = Element.text "Dunbrack \nEnergy"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.dunbrack)) }
                         , { header = Element.text "IntraR \nEnergy"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.intraR_total)) }
                         , { header = Element.text "InterS \nEnergy"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.interS_total)) }
                         , { header = Element.text "InterD \nEnergy"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.interD_total)) } ] 




evoef2_ref_columns = [ { header = Element.text "ALA"
                     , width = fill
                     , view =
                           \evoef2_result_ref ->
                               Element.text (String.fromInt (round evoef2_result_ref.reference_ALA))
                     }
                     , { header = Element.text "CYS"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_CYS))
                       }
                     , { header = Element.text "ASP"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_ASP))
                       }
                     , { header = Element.text "GLU"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_GLU))
                       }
                     , { header = Element.text "PHE"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_PHE))
                       }
                     , { header = Element.text "GLY"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_GLY))
                       }
                     , { header = Element.text "HIS"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_HIS))
                       }
                     , { header = Element.text "ILE"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_ILE))
                       }
                     , { header = Element.text "LYS"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_LYS))
                       }
                     , { header = Element.text "LEU"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_LEU))
                       }
                     , { header = Element.text "MET"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_MET))
                       }
                     , { header = Element.text "ASN"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_ASN))
                       }
                     , { header = Element.text "PRO"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_PRO))
                       }
                     , { header = Element.text "GLN"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_GLN))
                       }
                     , { header = Element.text "ARG"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_ARG))
                       }
                     , { header = Element.text "SER"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_SER))
                       }
                     , { header = Element.text "THR"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_THR))
                       }
                     , { header = Element.text "VAL"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_VAL))
                       }
                     , { header = Element.text "TRP"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_TRP))
                       }
                     , { header = Element.text "TYR"
                       , width = fill
                       , view =
                             \evoef2_result_ref ->
                                 Element.text (String.fromInt (round evoef2_result_ref.reference_TYR))
                       } ]


evoef2_intraR_columns = [ { header = Element.text "VDWATT"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.intraR_vdwatt)) } 
                        , { header = Element.text "VDWREP"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.intraR_vdwrep)) } 
                        , { header = Element.text "ELECTR"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.intraR_electr)) } 
                        , { header = Element.text "DESLVP"
                          , width = fill
                          , view =
                                \evoef2_result_tot ->
                                     Element.text (String.fromInt (round evoef2_result_tot.intraR_deslvP)) }
                         , { header = Element.text "DESLVH"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.intraR_deslvH)) }
                         , { header = Element.text "HBSCBB \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.intraR_hbscbb_dis)) }
                         , { header = Element.text "HBSCBB \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.intraR_hbscbb_the)) }
                         , { header = Element.text "HBSCBB \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_tot ->
                                      Element.text (String.fromInt (round evoef2_result_tot.intraR_hbscbb_phi)) } ] 


evoef2_interS_columns = [ { header = Element.text "VDWATT"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                     Element.text (String.fromInt (round evoef2_result_interS.interS_vdwatt)) } 
                        , { header = Element.text "VDWREP"
                          , width = fill
                          , view =
                                \evoef2_result_interS ->
                                     Element.text (String.fromInt (round evoef2_result_interS.interS_vdwrep)) } 
                        , { header = Element.text "ELECTR"
                          , width = fill
                          , view =
                                \evoef2_result_interS ->
                                     Element.text (String.fromInt (round evoef2_result_interS.interS_electr)) } 
                        , { header = Element.text "DESLVP"
                          , width = fill
                          , view =
                                \evoef2_result_interS ->
                                     Element.text (String.fromInt (round evoef2_result_interS.interS_deslvP)) }
                         , { header = Element.text "DESLVH"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_deslvH)) }
                         , { header = Element.text "HBBBBB \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbbbbb_dis)) }
                         , { header = Element.text "HBBBBB \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbbbbb_the)) }
                         , { header = Element.text "HBBBBB \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbbbbb_phi)) }
                         , { header = Element.text "HBSCBB \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscbb_dis)) }
                         , { header = Element.text "HBSCBB \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscbb_the)) }
                         , { header = Element.text "HBSCBB \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscbb_phi)) }
                         , { header = Element.text "HBSCSC \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscsc_dis)) }
                         , { header = Element.text "HBSCSC \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscsc_the)) }
                         , { header = Element.text "HBSCSC \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interS ->
                                      Element.text (String.fromInt (round evoef2_result_interS.interS_hbscsc_phi)) } ] 


evoef2_interD_columns = [ { header = Element.text "VDWATT"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                     Element.text (String.fromInt (round evoef2_result_interD.interD_vdwatt)) } 
                        , { header = Element.text "VDWREP"
                          , width = fill
                          , view =
                                \evoef2_result_interD ->
                                     Element.text (String.fromInt (round evoef2_result_interD.interD_vdwrep)) } 
                        , { header = Element.text "ELECTR"
                          , width = fill
                          , view =
                                \evoef2_result_interD ->
                                     Element.text (String.fromInt (round evoef2_result_interD.interD_electr)) } 
                        , { header = Element.text "DESLVP"
                          , width = fill
                          , view =
                                \evoef2_result_interD ->
                                     Element.text (String.fromInt (round evoef2_result_interD.interD_deslvP)) }
                         , { header = Element.text "DESLVH"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_deslvH)) }
                         , { header = Element.text "HBBBBB \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbbbbb_dis)) }
                         , { header = Element.text "HBBBBB \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbbbbb_the)) }
                         , { header = Element.text "HBBBBB \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbbbbb_phi)) }
                         , { header = Element.text "HBSCBB \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscbb_dis)) }
                         , { header = Element.text "HBSCBB \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscbb_the)) }
                         , { header = Element.text "HBSCBB \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscbb_phi)) }
                         , { header = Element.text "HBSCSC \nDIS"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscsc_dis)) }
                         , { header = Element.text "HBSCSC \nTHE"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscsc_the)) }
                         , { header = Element.text "HBSCSC \nPHI"
                           , width = fill
                           , view =
                                \evoef2_result_interD ->
                                      Element.text (String.fromInt (round evoef2_result_interD.interD_hbscsc_phi)) } ] 





sub_title_summary = "Summary"
sub_title_ref = "Reference Energy Values"
sub_title_intraR = "IntraR Energy Values"
sub_title_interS = "InterS Energy Values"
sub_title_interD = "InterD Energy Values"
initial_option = Summary


model : Model
model = 
    { selected_option = initial_option
    , selected_columns = evoef2_summary_columns
    , selected_sub_title = sub_title_summary }


init : Model
init = 
    model


-- UPDATE


update : Msg -> Model -> Model
update msg _ =
    case msg of
        ChangeSelected option -> 
            { selected_option = option 
            , selected_columns = 
                if option == Summary then evoef2_summary_columns
                else if option == Reference then evoef2_ref_columns
                else if option == IntraR then evoef2_intraR_columns
                else if option == InterS then evoef2_interS_columns
                else if option == InterD then evoef2_interD_columns
                else evoef2_summary_columns
            , selected_sub_title = 
                if option == Summary then sub_title_summary
                else if option == Reference then sub_title_ref
                else if option == IntraR then sub_title_intraR
                else if option == InterS then sub_title_interS
                else if option == InterD then sub_title_interD
                else "Error" }
            




-- VIEW

changed_selected: Option -> Msg
changed_selected option = 
    ChangeSelected option
    


    
view: Model -> Html Msg
view input_model = 


  Element.layout [] <|
    column[ width fill ] <|
       [ row [ width fill, centerY, spacing 30 ]
             [ el [ centerX 
                  , spacing 20 
                  , padding 20 ] <| 
                text "EvoEF2 Results" ] 
        , row [ width fill, centerY, spacing 30 ]
             [ el [ alignLeft
                  , spacing 20
                  , padding 20 ] <| 
                Input.radio [ padding 20
                                , spacing 20 ]
                                { onChange = changed_selected 
                                , selected = Just input_model.selected_option
                                , label = Input.labelAbove [] (text "Select Table View")
                                , options =
                                [ Input.option Summary (text "Summary")
                                , Input.option Reference (text "Reference")
                                , Input.option IntraR (text "IntraR")
                                , Input.option InterS (text "InterS")
                                , Input.option InterD (text "InterD") ] }
              ,  column[ width fill ] <| 
                    [ el [ centerY
                       , centerX
                       , padding 20 ] <| text input_model.selected_sub_title
                 ,  el [ centerY
                       , centerX
                       , padding 10 
                       , spacing 10 ] <| table [ centerX
                                               , centerY
                                               , spacing 25
                                               , padding 20
                                               , height ( px 150 )
                                               --, Element.Border.rounded 2
                                               --, Element.Border.color (rgba 0 0 0 1)
                                               --, Element.Border.widthXY 3  3
                                               , Background.color (rgba 0.6 0.76 0.88 1) ]
                                               { data = [ evoef2_results ]
                                               , columns = input_model.selected_columns } ] ]
              , el [ centerY
                   , centerX
                   , padding 20 ] <| text "EvoEF2 Log Information"
              , el [ centerX 
                   , spacing 20 
                   , padding 20
                   , Element.Border.rounded 1
                   , Element.Border.color (rgba 0 0 0 1)
                   , Element.Border.widthXY 2  2
                   ] <| text evoef2_results.log_info ] 


 
