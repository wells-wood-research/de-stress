module Pages.Glossary exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Shared.MetricDocumentation as MetricDocs
import Shared.Style as Style
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    Url Params


type alias Msg =
    Never


page : Page Params Model Msg
page =
    Page.static
        { view = view
        }



-- VIEW


rowViewGlossary : String -> String -> Element Msg
rowViewGlossary metricName metricDesc =
    row [ padding 5, spacing 5, width fill, Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 } ]
        [ el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricName ]
        , el [ width <| fillPortion 1, Font.alignLeft ] <| paragraph [] [ text metricDesc ]
        ]


aggrescan3DMetricTable : Element Msg
aggrescan3DMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.aggrescan3D.metricName.totalScore
            MetricDocs.softwareInfo.aggrescan3D.metricDesc.totalScore
        , rowViewGlossary
            MetricDocs.softwareInfo.aggrescan3D.metricName.averageScore
            MetricDocs.softwareInfo.aggrescan3D.metricDesc.averageScore
        , rowViewGlossary
            MetricDocs.softwareInfo.aggrescan3D.metricName.minimumScore
            MetricDocs.softwareInfo.aggrescan3D.metricDesc.minimumScore
        , rowViewGlossary
            MetricDocs.softwareInfo.aggrescan3D.metricName.maximumScore
            MetricDocs.softwareInfo.aggrescan3D.metricDesc.maximumScore
        ]


budeMetricTable : Element Msg
budeMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.bude.metricName.totalEnergy
            MetricDocs.softwareInfo.bude.metricDesc.totalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.bude.metricName.stericEnergy
            MetricDocs.softwareInfo.bude.metricDesc.stericEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.bude.metricName.desolvationEnergy
            MetricDocs.softwareInfo.bude.metricDesc.desolvationEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.bude.metricName.chargeEnergy
            MetricDocs.softwareInfo.bude.metricDesc.chargeEnergy
        ]


dfire2MetricTable : Element Msg
dfire2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.dfire2.metricName.totalEnergy
            MetricDocs.softwareInfo.dfire2.metricDesc.totalEnergy
        ]


dsspMetricTable : Element Msg
dsspMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.alpha_helix
            MetricDocs.softwareInfo.dssp.metricDesc.alpha_helix
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.beta_bridge
            MetricDocs.softwareInfo.dssp.metricDesc.beta_bridge
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.beta_strand
            MetricDocs.softwareInfo.dssp.metricDesc.beta_strand
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.three_ten_helix
            MetricDocs.softwareInfo.dssp.metricDesc.three_ten_helix
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.pi_helix
            MetricDocs.softwareInfo.dssp.metricDesc.pi_helix
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.turn
            MetricDocs.softwareInfo.dssp.metricDesc.turn
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.bend
            MetricDocs.softwareInfo.dssp.metricDesc.bend
        , rowViewGlossary
            MetricDocs.softwareInfo.dssp.metricName.loop
            MetricDocs.softwareInfo.dssp.metricDesc.loop
        ]


evoef2MetricTable : Element Msg
evoef2MetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.totalEnergy
            MetricDocs.softwareInfo.evoef2.metricDesc.totalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.refTotalEnergy
            MetricDocs.softwareInfo.evoef2.metricDesc.refTotalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRTotalEnergy
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRTotalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSTotalEnergy
            MetricDocs.softwareInfo.evoef2.metricDesc.interSTotalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDTotalEnergy
            MetricDocs.softwareInfo.evoef2.metricDesc.interDTotalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceALA
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceALA
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceCYS
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceCYS
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceASP
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceASP
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceGLU
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceGLU
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referencePHE
            MetricDocs.softwareInfo.evoef2.metricDesc.referencePHE
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceGLY
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceGLY
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceHIS
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceHIS
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceILE
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceILE
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceLYS
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceLYS
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceLEU
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceLEU
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceMET
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceMET
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceASN
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceASN
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referencePRO
            MetricDocs.softwareInfo.evoef2.metricDesc.referencePRO
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceGLN
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceGLN
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceARG
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceARG
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceSER
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceSER
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceTHR
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceTHR
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceVAL
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceVAL
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceTRP
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceTRP
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.referenceTYR
            MetricDocs.softwareInfo.evoef2.metricDesc.referenceTYR
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRVdwatt
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRVdwatt
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRVdwrep
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRVdwrep
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRElectr
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRElectr
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRDeslvP
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRDeslvP
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRDeslvH
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRDeslvH
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRHbscbbDis
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRHbscbbDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRHbscbbThe
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRHbscbbThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRHbscbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRHbscbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.intraRHbscbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.intraRHbscbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.aapropensity
            MetricDocs.softwareInfo.evoef2.metricDesc.aapropensity
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.ramachandran
            MetricDocs.softwareInfo.evoef2.metricDesc.ramachandran
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.dunbrack
            MetricDocs.softwareInfo.evoef2.metricDesc.dunbrack
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSVdwatt
            MetricDocs.softwareInfo.evoef2.metricDesc.interSVdwatt
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSVdwrep
            MetricDocs.softwareInfo.evoef2.metricDesc.interSVdwrep
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSElectr
            MetricDocs.softwareInfo.evoef2.metricDesc.interSElectr
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSDeslvP
            MetricDocs.softwareInfo.evoef2.metricDesc.interSDeslvP
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSDeslvH
            MetricDocs.softwareInfo.evoef2.metricDesc.interSDeslvH
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSSsbond
            MetricDocs.softwareInfo.evoef2.metricDesc.interSSsbond
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbbbbbDis
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbbbbbDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbbbbbThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbbbbbThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbbbbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbbbbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbscbbDis
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbscbbDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbscbbThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbscbbThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbscbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbscbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbscscThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbscscThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interSHbscscPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interSHbscscPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDVdwatt
            MetricDocs.softwareInfo.evoef2.metricDesc.interDVdwatt
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDVdwrep
            MetricDocs.softwareInfo.evoef2.metricDesc.interDVdwrep
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDElectr
            MetricDocs.softwareInfo.evoef2.metricDesc.interDElectr
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDDeslvP
            MetricDocs.softwareInfo.evoef2.metricDesc.interDDeslvP
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDDeslvH
            MetricDocs.softwareInfo.evoef2.metricDesc.interDDeslvH
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDSsbond
            MetricDocs.softwareInfo.evoef2.metricDesc.interDSsbond
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbbbbbDis
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbbbbbDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbbbbbThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbbbbbThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbbbbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbbbbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscbbDis
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscbbDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscbbThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscbbThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscbbPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscbbPhi
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscscDis
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscscDis
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscscThe
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscscThe
        , rowViewGlossary
            MetricDocs.softwareInfo.evoef2.metricName.interDHbscscPhi
            MetricDocs.softwareInfo.evoef2.metricDesc.interDHbscscPhi
        ]


hydroFitMetricTable : Element Msg
hydroFitMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.hydroFit.metricName.hydroFit
            MetricDocs.softwareInfo.hydroFit.metricDesc.hydroFit
        ]


packDensMetricTable : Element Msg
packDensMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.packDens.metricName.packDens
            MetricDocs.softwareInfo.packDens.metricDesc.packDens
        ]


rosettaMetricTable : Element Msg
rosettaMetricTable =
    column
        [ width fill ]
        [ row
            [ padding 5, width fill, Border.widthXY 0 2, Font.bold ]
            [ el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Name"
            , el [ width <| fillPortion 1, Font.alignLeft ] <| text "Metric Description"
            ]
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.totalEnergy
            MetricDocs.softwareInfo.rosetta.metricDesc.totalEnergy
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.reference
            MetricDocs.softwareInfo.rosetta.metricDesc.reference
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.vdwAtt
            MetricDocs.softwareInfo.rosetta.metricDesc.vdwAtt
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.vdwRep
            MetricDocs.softwareInfo.rosetta.metricDesc.vdwRep
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.vdwRepIntraR
            MetricDocs.softwareInfo.rosetta.metricDesc.vdwRepIntraR
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.electrostatics
            MetricDocs.softwareInfo.rosetta.metricDesc.electrostatics
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.solvIso
            MetricDocs.softwareInfo.rosetta.metricDesc.solvIso
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.solvAniso
            MetricDocs.softwareInfo.rosetta.metricDesc.solvAniso
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.solvIsoIntraR
            MetricDocs.softwareInfo.rosetta.metricDesc.solvIsoIntraR
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.hblrbb
            MetricDocs.softwareInfo.rosetta.metricDesc.hblrbb
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.hbsrbb
            MetricDocs.softwareInfo.rosetta.metricDesc.hbsrbb
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.hbbbsc
            MetricDocs.softwareInfo.rosetta.metricDesc.hbbbsc
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.hbscsc
            MetricDocs.softwareInfo.rosetta.metricDesc.hbscsc
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.ssBond
            MetricDocs.softwareInfo.rosetta.metricDesc.ssBond
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.rama
            MetricDocs.softwareInfo.rosetta.metricDesc.rama
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.aaProp
            MetricDocs.softwareInfo.rosetta.metricDesc.aaProp
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.dunbrack
            MetricDocs.softwareInfo.rosetta.metricDesc.dunbrack
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.omegaPen
            MetricDocs.softwareInfo.rosetta.metricDesc.omegaPen
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.openProPen
            MetricDocs.softwareInfo.rosetta.metricDesc.openProPen
        , rowViewGlossary
            MetricDocs.softwareInfo.rosetta.metricName.tyroPen
            MetricDocs.softwareInfo.rosetta.metricDesc.tyroPen
        ]


view : Url Params -> Document Msg
view { params } =
    { title = "Glossary"
    , body =
        [ column
            [ centerX
            , spacing 20
            , Style.pageWidths.singleColumn
            , Font.size 16
            ]
            [ paragraph [ spacing 20 ]
                [ text "Aggrescan3D 2.0"
                    |> Style.h3
                , aggrescan3DMetricTable
                , text "BUDE"
                    |> Style.h3
                , budeMetricTable
                ]
            , text "DFIRE2"
                |> Style.h3
            , dfire2MetricTable
            , text "DSSP"
                |> Style.h3
            , dsspMetricTable
            , text "EvoEF2"
                |> Style.h3
            , evoef2MetricTable
            , text "Hydrophobic Fitness"
                |> Style.h3
            , hydroFitMetricTable
            , text "Packing Density"
                |> Style.h3
            , packDensMetricTable
            , text "Rosetta"
                |> Style.h3
            , rosettaMetricTable
            ]
        ]
    }
