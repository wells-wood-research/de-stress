module Shared.Tooltips exposing
    ( HoverInfoOption(..)
    , agg3dAvgScoreHoverBox
    , agg3dMaxScoreHoverBox
    , agg3dMinScoreHoverBox
    , agg3dTotalScoreHoverBox
    , budeFFChargeHoverBox
    , budeFFDesolvationHoverBox
    , budeFFStericHoverBox
    , budeFFTotalEnergyHoverBox
    , dfire2TotalHoverBox
    , evoEF2InterDDesolvHHoverBox
    , evoEF2InterDDesolvPHoverBox
    , evoEF2InterDElecHoverBox
    , evoEF2InterDHBBBBBDisHoverBox
    , evoEF2InterDHBBBBBPhiHoverBox
    , evoEF2InterDHBBBBBTheHoverBox
    , evoEF2InterDHBSCBBDisHoverBox
    , evoEF2InterDHBSCBBPhiHoverBox
    , evoEF2InterDHBSCBBTheHoverBox
    , evoEF2InterDHBSCSCDisHoverBox
    , evoEF2InterDHBSCSCPhiHoverBox
    , evoEF2InterDHBSCSCTheHoverBox
    , evoEF2InterDSSbondHHoverBox
    , evoEF2InterDVDWAttHoverBox
    , evoEF2InterDVDWRepHoverBox
    , evoEF2InterSDesolvHHoverBox
    , evoEF2InterSDesolvPHoverBox
    , evoEF2InterSElecHoverBox
    , evoEF2InterSHBBBBBDisHoverBox
    , evoEF2InterSHBBBBBPhiHoverBox
    , evoEF2InterSHBBBBBTheHoverBox
    , evoEF2InterSHBSCBBDisHoverBox
    , evoEF2InterSHBSCBBPhiHoverBox
    , evoEF2InterSHBSCBBTheHoverBox
    , evoEF2InterSHBSCSCDisHoverBox
    , evoEF2InterSHBSCSCPhiHoverBox
    , evoEF2InterSHBSCSCTheHoverBox
    , evoEF2InterSSSbondHHoverBox
    , evoEF2InterSVDWAttHoverBox
    , evoEF2InterSVDWRepHoverBox
    , evoEF2IntraRAAPropHoverBox
    , evoEF2IntraRDesolvHHoverBox
    , evoEF2IntraRDesolvPHoverBox
    , evoEF2IntraRDunbrackHoverBox
    , evoEF2IntraRElecHoverBox
    , evoEF2IntraRHBSCBBDisHoverBox
    , evoEF2IntraRHBSCBBPhiHoverBox
    , evoEF2IntraRHBSCBBTheHoverBox
    , evoEF2IntraRRamaHoverBox
    , evoEF2IntraRVDWAttHoverBox
    , evoEF2IntraRVDWRepHoverBox
    , evoEF2RefALAHoverBox
    , evoEF2RefARGHoverBox
    , evoEF2RefASNHoverBox
    , evoEF2RefASPHoverBox
    , evoEF2RefCYSHoverBox
    , evoEF2RefGLNHoverBox
    , evoEF2RefGLUHoverBox
    , evoEF2RefGLYHoverBox
    , evoEF2RefHISHoverBox
    , evoEF2RefILEHoverBox
    , evoEF2RefLEUHoverBox
    , evoEF2RefLYSHoverBox
    , evoEF2RefMETHoverBox
    , evoEF2RefPHEHoverBox
    , evoEF2RefPROHoverBox
    , evoEF2RefSERHoverBox
    , evoEF2RefTHRHoverBox
    , evoEF2RefTRPHoverBox
    , evoEF2RefTYRHoverBox
    , evoEF2RefVALHoverBox
    , evoEF2SummaryInterDHoverBox
    , evoEF2SummaryInterSHoverBox
    , evoEF2SummaryIntraRHoverBox
    , evoEF2SummaryRefHoverBox
    , evoEF2SummaryTotalHoverBox
    , hoverInfoView
    , hydrophobicFitnessHoverBox
    , isoelectricPointHoverBox
    , massHoverBox
    , numOfResiduesHoverBox
    , packingDensityHoverBox
    , rosettaAAPropHoverBox
    , rosettaDunbrackHoverBox
    , rosettaElecHoverBox
    , rosettaHBBBSCHoverBox
    , rosettaHBLRBBHoverBox
    , rosettaHBSCSCHoverBox
    , rosettaHBSRBBHoverBox
    , rosettaOmegaPenHoverBox
    , rosettaOpenProPenHoverBox
    , rosettaRamaHoverBox
    , rosettaReferenceHoverBox
    , rosettaSSbondHoverBox
    , rosettaSolvAnisoHoverBox
    , rosettaSolvIsoHoverBox
    , rosettaSolvIsoIntraRHoverBox
    , rosettaTotalHoverBox
    , rosettaTyroPenHoverBox
    , rosettaVDWAttHoverBox
    , rosettaVDWRepHoverBox
    , rosettaVDWRepIntraRHoverBox
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Shared.Documentation as Docs
import Shared.Style as Style


type HoverInfoOption
    = HydrophobicFitness
    | IsoelectricPoint
    | NumOfResidues
    | Mass
    | PackingDensity
    | BudeFFTotalEnergy
    | BudeFFSteric
    | BudeFFDesolvation
    | BudeFFCharge
    | EvoEF2Total
    | EvoEF2RefTotal
    | EvoEF2IntaRTotal
    | EvoEF2InterSTotal
    | EvoEF2InterDTotal
    | EvoEF2RefALA
    | EvoEF2RefCYS
    | EvoEF2RefASP
    | EvoEF2RefGLU
    | EvoEF2RefPHE
    | EvoEF2RefGLY
    | EvoEF2RefHIS
    | EvoEF2RefILE
    | EvoEF2RefLYS
    | EvoEF2RefLEU
    | EvoEF2RefMET
    | EvoEF2RefASN
    | EvoEF2RefPRO
    | EvoEF2RefGLN
    | EvoEF2RefARG
    | EvoEF2RefSER
    | EvoEF2RefTHR
    | EvoEF2RefVAL
    | EvoEF2RefTRP
    | EvoEF2RefTYR
    | EvoEF2IntraRVDWAtt
    | EvoEF2IntraRVDWRep
    | EvoEF2IntraRElec
    | EvoEF2IntraRDesolvP
    | EvoEF2IntraRDesolvH
    | EvoEF2AAProp
    | EvoEF2Rama
    | EvoEF2Dunbrack
    | EvoEF2IntraRHBSCBBDis
    | EvoEF2IntraRHBSCBBThe
    | EvoEF2IntraRHBSCBBPhi
    | EvoEF2InterSVDWAtt
    | EvoEF2InterSVDWRep
    | EvoEF2InterSElec
    | EvoEF2InterSDesolvP
    | EvoEF2InterSDesolvH
    | EvoEF2InterSSSbond
    | EvoEF2InterSHBBBBBDis
    | EvoEF2InterSHBBBBBThe
    | EvoEF2InterSHBBBBBPhi
    | EvoEF2InterSHBSCBBDis
    | EvoEF2InterSHBSCBBThe
    | EvoEF2InterSHBSCBBPhi
    | EvoEF2InterSHBSCSCDis
    | EvoEF2InterSHBSCSCThe
    | EvoEF2InterSHBSCSCPhi
    | EvoEF2InterDVDWAtt
    | EvoEF2InterDVDWRep
    | EvoEF2InterDElec
    | EvoEF2InterDDesolvP
    | EvoEF2InterDDesolvH
    | EvoEF2InterDSSbond
    | EvoEF2InterDHBBBBBDis
    | EvoEF2InterDHBBBBBThe
    | EvoEF2InterDHBBBBBPhi
    | EvoEF2InterDHBSCBBDis
    | EvoEF2InterDHBSCBBThe
    | EvoEF2InterDHBSCBBPhi
    | EvoEF2InterDHBSCSCDis
    | EvoEF2InterDHBSCSCThe
    | EvoEF2InterDHBSCSCPhi
    | DFIRE2Total
    | RosettaTotal
    | RosettaReference
    | RosettaVDWAtt
    | RosettaVDWRep
    | RosettaVDWRepIntraR
    | RosettaElec
    | RosettaSolvIso
    | RosettaSolvAniso
    | RosettaSolvIsoIntraR
    | RosettaHBLRBB
    | RosettaHBSRBB
    | RosettaHBBBSC
    | RosettaHBSCSC
    | RosettaSSbond
    | RosettaRama
    | RosettaAAProp
    | RosettaDunbrack
    | RosettaOmegaPen
    | RosettaOpenProPen
    | RosettaTyroPen
    | Agg3dTotalScore
    | Agg3dAvgScore
    | Agg3dMinScore
    | Agg3dMaxScore
    | NoHoverInfo


hoverInfoView :
    { title : String
    , info : String
    , mouseEnterMsg : HoverInfoOption
    , hoverInfoOption : HoverInfoOption
    , changeMsg : HoverInfoOption -> msg
    }
    -> List (Attribute msg)
hoverInfoView { title, info, mouseEnterMsg, hoverInfoOption, changeMsg } =
    let
        content : HoverInfoOption -> List (Attribute msg)
        content option =
            if option == mouseEnterMsg then
                [ above
                    (column
                        [ spacing 15
                        , padding 15
                        , centerX
                        , width (px 180)
                        , Border.innerShadow
                            { offset = ( 0, 0 )
                            , size = 1
                            , blur = 0
                            , color = rgba 0.5 0.5 0.5 0.4
                            }
                        , Border.color Style.colorPalette.black
                        , Border.width 1
                        , Background.color Style.colorPalette.white
                        , Font.color Style.colorPalette.black
                        ]
                        [ paragraph
                            [ Font.size 14
                            ]
                            [ text title
                            ]
                        , paragraph
                            [ Font.size 12 ]
                            [ text info ]
                        ]
                    )
                ]

            else
                []
    in
    [ Events.onMouseEnter (changeMsg mouseEnterMsg)
    , Events.onMouseLeave (changeMsg NoHoverInfo)
    ]
        ++ content hoverInfoOption



-- {{{ Basic Metrics Tooltips


hydrophobicFitnessHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
hydrophobicFitnessHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.hydroFit.metricName.hydroFit
        , info = Docs.metricInfo.hydroFit.metricDesc.hydroFit
        , mouseEnterMsg = HydrophobicFitness
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


isoelectricPointHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
isoelectricPointHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.isoPoint.metricName.isoPoint
        , info = Docs.metricInfo.isoPoint.metricDesc.isoPoint
        , mouseEnterMsg = IsoelectricPoint
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


numOfResiduesHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
numOfResiduesHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.other.metricName.numberOfResidues
        , info = Docs.metricInfo.other.metricDesc.numberOfResidues
        , mouseEnterMsg = NumOfResidues
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


massHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
massHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.other.metricName.mass
        , info = Docs.metricInfo.other.metricDesc.mass
        , mouseEnterMsg = Mass
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


packingDensityHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
packingDensityHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.packDens.metricName.packDens
        , info = Docs.metricInfo.packDens.metricDesc.packDens
        , mouseEnterMsg = PackingDensity
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ BUDE FF Tooltips


budeFFTotalEnergyHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFTotalEnergyHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.bude.metricName.totalEnergy
        , info = Docs.metricInfo.bude.metricDesc.totalEnergy
        , mouseEnterMsg = BudeFFTotalEnergy
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFStericHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFStericHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.bude.metricName.stericEnergy
        , info = Docs.metricInfo.bude.metricDesc.stericEnergy
        , mouseEnterMsg = BudeFFSteric
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFDesolvationHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFDesolvationHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.bude.metricName.desolvationEnergy
        , info = Docs.metricInfo.bude.metricDesc.desolvationEnergy
        , mouseEnterMsg = BudeFFDesolvation
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFChargeHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFChargeHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.bude.metricName.chargeEnergy
        , info = Docs.metricInfo.bude.metricDesc.chargeEnergy
        , mouseEnterMsg = BudeFFCharge
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 Summary Tooltips


evoEF2SummaryTotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryTotalHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.totalEnergy
        , info = Docs.metricInfo.evoef2.metricDesc.totalEnergy
        , mouseEnterMsg = EvoEF2Total
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryRefHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryRefHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.refTotalEnergy
        , info = Docs.metricInfo.evoef2.metricDesc.refTotalEnergy
        , mouseEnterMsg = EvoEF2RefTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRTotalEnergy
        , info = Docs.metricInfo.evoef2.metricDesc.intraRTotalEnergy
        , mouseEnterMsg = EvoEF2IntaRTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryInterSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryInterSHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSTotalEnergy
        , info = Docs.metricInfo.evoef2.metricDesc.interSTotalEnergy
        , mouseEnterMsg = EvoEF2InterSTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryInterDHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryInterDHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDTotalEnergy
        , info = Docs.metricInfo.evoef2.metricDesc.interDTotalEnergy
        , mouseEnterMsg = EvoEF2InterDTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 Reference Tooltips


evoEF2RefALAHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefALAHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceALA
        , info = Docs.metricInfo.evoef2.metricDesc.referenceALA
        , mouseEnterMsg = EvoEF2RefALA
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefCYSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefCYSHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceCYS
        , info = Docs.metricInfo.evoef2.metricDesc.referenceCYS
        , mouseEnterMsg = EvoEF2RefCYS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefASPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefASPHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceASP
        , info = Docs.metricInfo.evoef2.metricDesc.referenceASP
        , mouseEnterMsg = EvoEF2RefASP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLUHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLUHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceGLU
        , info = Docs.metricInfo.evoef2.metricDesc.referenceGLU
        , mouseEnterMsg = EvoEF2RefGLU
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefPHEHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefPHEHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referencePHE
        , info = Docs.metricInfo.evoef2.metricDesc.referencePHE
        , mouseEnterMsg = EvoEF2RefPHE
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLYHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLYHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceGLY
        , info = Docs.metricInfo.evoef2.metricDesc.referenceGLY
        , mouseEnterMsg = EvoEF2RefGLY
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefHISHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefHISHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceHIS
        , info = Docs.metricInfo.evoef2.metricDesc.referenceHIS
        , mouseEnterMsg = EvoEF2RefHIS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefILEHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefILEHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceILE
        , info = Docs.metricInfo.evoef2.metricDesc.referenceILE
        , mouseEnterMsg = EvoEF2RefILE
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefLYSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefLYSHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceLYS
        , info = Docs.metricInfo.evoef2.metricDesc.referenceLYS
        , mouseEnterMsg = EvoEF2RefLYS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefLEUHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefLEUHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceLEU
        , info = Docs.metricInfo.evoef2.metricDesc.referenceLEU
        , mouseEnterMsg = EvoEF2RefLEU
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefMETHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefMETHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceMET
        , info = Docs.metricInfo.evoef2.metricDesc.referenceMET
        , mouseEnterMsg = EvoEF2RefMET
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefASNHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefASNHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceASN
        , info = Docs.metricInfo.evoef2.metricDesc.referenceASN
        , mouseEnterMsg = EvoEF2RefASN
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefPROHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefPROHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referencePRO
        , info = Docs.metricInfo.evoef2.metricDesc.referencePRO
        , mouseEnterMsg = EvoEF2RefPRO
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLNHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLNHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceGLN
        , info = Docs.metricInfo.evoef2.metricDesc.referenceGLN
        , mouseEnterMsg = EvoEF2RefGLN
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefARGHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefARGHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceARG
        , info = Docs.metricInfo.evoef2.metricDesc.referenceARG
        , mouseEnterMsg = EvoEF2RefARG
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefSERHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefSERHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceSER
        , info = Docs.metricInfo.evoef2.metricDesc.referenceSER
        , mouseEnterMsg = EvoEF2RefSER
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTHRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTHRHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceTHR
        , info = Docs.metricInfo.evoef2.metricDesc.referenceTHR
        , mouseEnterMsg = EvoEF2RefTHR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefVALHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefVALHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceVAL
        , info = Docs.metricInfo.evoef2.metricDesc.referenceVAL
        , mouseEnterMsg = EvoEF2RefVAL
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTRPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTRPHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceTRP
        , info = Docs.metricInfo.evoef2.metricDesc.referenceTRP
        , mouseEnterMsg = EvoEF2RefTRP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTYRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTYRHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.referenceTYR
        , info = Docs.metricInfo.evoef2.metricDesc.referenceTYR
        , mouseEnterMsg = EvoEF2RefTYR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 IntraR Tooltips


evoEF2IntraRVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRVdwatt
        , info = Docs.metricInfo.evoef2.metricDesc.intraRVdwatt
        , mouseEnterMsg = EvoEF2IntraRVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRVdwrep
        , info = Docs.metricInfo.evoef2.metricDesc.intraRVdwrep
        , mouseEnterMsg = EvoEF2IntraRVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRElecHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRElectr
        , info = Docs.metricInfo.evoef2.metricDesc.intraRElectr
        , mouseEnterMsg = EvoEF2IntraRElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRDeslvP
        , info = Docs.metricInfo.evoef2.metricDesc.intraRDeslvP
        , mouseEnterMsg = EvoEF2IntraRDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRDeslvH
        , info = Docs.metricInfo.evoef2.metricDesc.intraRDeslvH
        , mouseEnterMsg = EvoEF2IntraRDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRAAPropHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRAAPropHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.aapropensity
        , info = Docs.metricInfo.evoef2.metricDesc.aapropensity
        , mouseEnterMsg = EvoEF2AAProp
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRRamaHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRRamaHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.ramachandran
        , info = Docs.metricInfo.evoef2.metricDesc.ramachandran
        , mouseEnterMsg = EvoEF2Rama
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDunbrackHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDunbrackHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.dunbrack
        , info = Docs.metricInfo.evoef2.metricDesc.dunbrack
        , mouseEnterMsg = EvoEF2Dunbrack
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRHbscbbDis
        , info = Docs.metricInfo.evoef2.metricDesc.intraRHbscbbDis
        , mouseEnterMsg = EvoEF2IntraRHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRHbscbbThe
        , info = Docs.metricInfo.evoef2.metricDesc.intraRHbscbbThe
        , mouseEnterMsg = EvoEF2IntraRHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.intraRHbscbbPhi
        , info = Docs.metricInfo.evoef2.metricDesc.intraRHbscbbPhi
        , mouseEnterMsg = EvoEF2IntraRHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 InterS Tooltips


evoEF2InterSVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSVdwatt
        , info = Docs.metricInfo.evoef2.metricDesc.interSVdwatt
        , mouseEnterMsg = EvoEF2InterSVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSVdwrep
        , info = Docs.metricInfo.evoef2.metricDesc.interSVdwrep
        , mouseEnterMsg = EvoEF2InterSVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSElecHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSElectr
        , info = Docs.metricInfo.evoef2.metricDesc.interSElectr
        , mouseEnterMsg = EvoEF2InterSElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSDeslvP
        , info = Docs.metricInfo.evoef2.metricDesc.interSDeslvP
        , mouseEnterMsg = EvoEF2InterSDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSDeslvH
        , info = Docs.metricInfo.evoef2.metricDesc.interSDeslvH
        , mouseEnterMsg = EvoEF2InterSDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSSSbondHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSSSbondHHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSSsbond
        , info = Docs.metricInfo.evoef2.metricDesc.interSSsbond
        , mouseEnterMsg = EvoEF2InterSSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbbbbbDis
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbbbbbDis
        , mouseEnterMsg = EvoEF2InterSHBBBBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbbbbbThe
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbbbbbThe
        , mouseEnterMsg = EvoEF2InterSHBBBBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbbbbbPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbbbbbPhi
        , mouseEnterMsg = EvoEF2InterSHBBBBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscbbDis
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscbbDis
        , mouseEnterMsg = EvoEF2InterSHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscbbThe
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscbbThe
        , mouseEnterMsg = EvoEF2InterSHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscbbPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscbbPhi
        , mouseEnterMsg = EvoEF2InterSHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscscDis
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscscDis
        , mouseEnterMsg = EvoEF2InterSHBSCSCDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscscThe
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscscThe
        , mouseEnterMsg = EvoEF2InterSHBSCSCThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interSHbscscPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interSHbscscPhi
        , mouseEnterMsg = EvoEF2InterSHBSCSCPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 InterD Tooltips


evoEF2InterDVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDVdwatt
        , info = Docs.metricInfo.evoef2.metricDesc.interDVdwatt
        , mouseEnterMsg = EvoEF2InterDVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDVdwrep
        , info = Docs.metricInfo.evoef2.metricDesc.interDVdwrep
        , mouseEnterMsg = EvoEF2InterDVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDElecHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDElectr
        , info = Docs.metricInfo.evoef2.metricDesc.interDElectr
        , mouseEnterMsg = EvoEF2InterDElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDDeslvP
        , info = Docs.metricInfo.evoef2.metricDesc.interDDeslvP
        , mouseEnterMsg = EvoEF2InterDDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDDeslvH
        , info = Docs.metricInfo.evoef2.metricDesc.interDDeslvH
        , mouseEnterMsg = EvoEF2InterDDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDSSbondHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDSSbondHHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDSsbond
        , info = Docs.metricInfo.evoef2.metricDesc.interDSsbond
        , mouseEnterMsg = EvoEF2InterDSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbbbbbDis
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbbbbbDis
        , mouseEnterMsg = EvoEF2InterDHBBBBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbbbbbThe
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbbbbbThe
        , mouseEnterMsg = EvoEF2InterDHBBBBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbbbbbPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbbbbbPhi
        , mouseEnterMsg = EvoEF2InterDHBBBBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscbbDis
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscbbDis
        , mouseEnterMsg = EvoEF2InterDHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscbbThe
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscbbThe
        , mouseEnterMsg = EvoEF2InterDHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscbbPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscbbPhi
        , mouseEnterMsg = EvoEF2InterDHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCDisHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscscDis
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscscDis
        , mouseEnterMsg = EvoEF2InterDHBSCSCDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCTheHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscscThe
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscscThe
        , mouseEnterMsg = EvoEF2InterDHBSCSCThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCPhiHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.evoef2.metricName.interDHbscscPhi
        , info = Docs.metricInfo.evoef2.metricDesc.interDHbscscPhi
        , mouseEnterMsg = EvoEF2InterDHBSCSCPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ DFIRE2 Total Tooltip


dfire2TotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
dfire2TotalHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.dfire2.metricName.totalEnergy
        , info = Docs.metricInfo.dfire2.metricDesc.totalEnergy
        , mouseEnterMsg = DFIRE2Total
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ Rosetta Tooltips


rosettaTotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaTotalHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.totalEnergy
        , info = Docs.metricInfo.rosetta.metricDesc.totalEnergy
        , mouseEnterMsg = RosettaTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaReferenceHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaReferenceHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.reference
        , info = Docs.metricInfo.rosetta.metricDesc.reference
        , mouseEnterMsg = RosettaReference
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.vdwAtt
        , info = Docs.metricInfo.rosetta.metricDesc.vdwAtt
        , mouseEnterMsg = RosettaVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.vdwRep
        , info = Docs.metricInfo.rosetta.metricDesc.vdwRep
        , mouseEnterMsg = RosettaVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWRepIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWRepIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.vdwRepIntraR
        , info = Docs.metricInfo.rosetta.metricDesc.vdwRepIntraR
        , mouseEnterMsg = RosettaVDWRepIntraR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaElecHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.electrostatics
        , info = Docs.metricInfo.rosetta.metricDesc.electrostatics
        , mouseEnterMsg = RosettaElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvIsoHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvIsoHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.solvIso
        , info = Docs.metricInfo.rosetta.metricDesc.solvIso
        , mouseEnterMsg = RosettaSolvIso
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvAnisoHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvAnisoHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.solvAniso
        , info = Docs.metricInfo.rosetta.metricDesc.solvAniso
        , mouseEnterMsg = RosettaSolvAniso
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvIsoIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvIsoIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.solvIsoIntraR
        , info = Docs.metricInfo.rosetta.metricDesc.solvIsoIntraR
        , mouseEnterMsg = RosettaSolvIsoIntraR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBLRBBHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBLRBBHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.hblrbb
        , info = Docs.metricInfo.rosetta.metricDesc.hblrbb
        , mouseEnterMsg = RosettaHBLRBB
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBSRBBHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBSRBBHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.hbsrbb
        , info = Docs.metricInfo.rosetta.metricDesc.hbsrbb
        , mouseEnterMsg = RosettaHBSRBB
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBBBSCHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBBBSCHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.hbbbsc
        , info = Docs.metricInfo.rosetta.metricDesc.hbbbsc
        , mouseEnterMsg = RosettaHBBBSC
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBSCSCHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBSCSCHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.hbscsc
        , info = Docs.metricInfo.rosetta.metricDesc.hbscsc
        , mouseEnterMsg = RosettaHBSCSC
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSSbondHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSSbondHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.ssBond
        , info = Docs.metricInfo.rosetta.metricDesc.ssBond
        , mouseEnterMsg = RosettaSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaRamaHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaRamaHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.rama
        , info = Docs.metricInfo.rosetta.metricDesc.rama
        , mouseEnterMsg = RosettaRama
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaAAPropHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaAAPropHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.aaProp
        , info = Docs.metricInfo.rosetta.metricDesc.aaProp
        , mouseEnterMsg = RosettaAAProp
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaDunbrackHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaDunbrackHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.dunbrack
        , info = Docs.metricInfo.rosetta.metricDesc.dunbrack
        , mouseEnterMsg = RosettaDunbrack
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaOmegaPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaOmegaPenHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.omegaPen
        , info = Docs.metricInfo.rosetta.metricDesc.omegaPen
        , mouseEnterMsg = RosettaOmegaPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaOpenProPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaOpenProPenHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.openProPen
        , info = Docs.metricInfo.rosetta.metricDesc.openProPen
        , mouseEnterMsg = RosettaOpenProPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaTyroPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaTyroPenHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.rosetta.metricName.tyroPen
        , info = Docs.metricInfo.rosetta.metricDesc.tyroPen
        , mouseEnterMsg = RosettaTyroPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ Aggrescan3d Tooltips


agg3dTotalScoreHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
agg3dTotalScoreHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.aggrescan3D.metricName.totalScore
        , info = Docs.metricInfo.aggrescan3D.metricDesc.totalScore
        , mouseEnterMsg = Agg3dTotalScore
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


agg3dAvgScoreHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
agg3dAvgScoreHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.aggrescan3D.metricName.averageScore
        , info = Docs.metricInfo.aggrescan3D.metricDesc.averageScore
        , mouseEnterMsg = Agg3dAvgScore
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


agg3dMinScoreHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
agg3dMinScoreHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.aggrescan3D.metricName.minimumScore
        , info = Docs.metricInfo.aggrescan3D.metricDesc.minimumScore
        , mouseEnterMsg = Agg3dMinScore
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


agg3dMaxScoreHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
agg3dMaxScoreHoverBox option changeMsg =
    hoverInfoView
        { title = Docs.metricInfo.aggrescan3D.metricName.maximumScore
        , info = Docs.metricInfo.aggrescan3D.metricDesc.maximumScore
        , mouseEnterMsg = Agg3dMaxScore
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
