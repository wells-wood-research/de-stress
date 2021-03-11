module Shared.Tooltips exposing
    ( HoverInfoOption(..)
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
import Shared.Style as Style


type HoverInfoOption
    = BudeFFTotalEnergy
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



-- {{{ BUDE FF Tooltips


budeFFTotalEnergyHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFTotalEnergyHoverBox option changeMsg =
    hoverInfoView
        { title = "BUDE FF Total Energy"
        , info = """This value is the total BUDE force field energy. It is the sum of
                 the steric, desolvation and charge components.
                 """
        , mouseEnterMsg = BudeFFTotalEnergy
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFStericHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFStericHoverBox option changeMsg =
    hoverInfoView
        { title = "BUDE FF Steric Energy"
        , info = """This value is the steric component of the BUDE force field energy.
                 It is calculated with a simplified Leonard-Jones potential. It is
                 softer than the steric component of many other force fields.
                 """
        , mouseEnterMsg = BudeFFSteric
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFDesolvationHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFDesolvationHoverBox option changeMsg =
    hoverInfoView
        { title = "BUDE FF Desolvation Energy"
        , info = """This value is the desolvation component of the BUDE force field energy.
                 """
        , mouseEnterMsg = BudeFFDesolvation
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


budeFFChargeHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
budeFFChargeHoverBox option changeMsg =
    hoverInfoView
        { title = "BUDE FF Charge Energy"
        , info = """This value is the charge component of the BUDE force field energy.
                 """
        , mouseEnterMsg = BudeFFCharge
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 Summary Tooltips


evoEF2SummaryTotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryTotalHoverBox option changeMsg =
    hoverInfoView
        { title = "Total EvoEF2"
        , info = """This value is the total EvoEF2 energy. It is the sum of the reference, 
                    intra residue, inter residue - same chain and inter residue - different 
                    chains, energy values. In the EvoEF2 output this field is called `Total`."""
        , mouseEnterMsg = EvoEF2Total
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryRefHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryRefHoverBox option changeMsg =
    hoverInfoView
        { title = "Reference"
        , info = """This value is the total reference energy. This value is not included in 
                    the EvoEF2 output and is calculated in DE-STRESS."""
        , mouseEnterMsg = EvoEF2RefTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = "Intra Residue"
        , info = """This value is the total energy for intra residue interactions. This value is 
                    not included in the EvoEF2 output and is calculated in DE-STRESS."""
        , mouseEnterMsg = EvoEF2IntaRTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryInterSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryInterSHoverBox option changeMsg =
    hoverInfoView
        { title = "Inter Residue - Same Chain"
        , info = """This value is the total energy for inter residue interactions in the same chain. 
                    This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
        , mouseEnterMsg = EvoEF2InterSTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2SummaryInterDHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2SummaryInterDHoverBox option changeMsg =
    hoverInfoView
        { title = "Inter Residue - Different Chains"
        , info = """This value is the total energy for inter residue interactions in different chains. 
                    This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
        , mouseEnterMsg = EvoEF2InterDTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 Reference Tooltips


evoEF2RefALAHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefALAHoverBox option changeMsg =
    hoverInfoView
        { title = "ALA - Reference"
        , info = """This value is reference energy for the amino acid Alanine (ALA). In the EvoEF2
                    output this value is called `reference_ALA`."""
        , mouseEnterMsg = EvoEF2RefALA
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefCYSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefCYSHoverBox option changeMsg =
    hoverInfoView
        { title = "CYS - Reference"
        , info = """This value is reference energy for the amino acid Cysteine (CYS). In the EvoEF2
                    output this value is called `reference_CYS`."""
        , mouseEnterMsg = EvoEF2RefCYS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefASPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefASPHoverBox option changeMsg =
    hoverInfoView
        { title = "ASP - Reference"
        , info = """This value is reference energy for the amino acid Aspartic acid (ASP). In the EvoEF2
                    output this value is called `reference_ASP`."""
        , mouseEnterMsg = EvoEF2RefASP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLUHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLUHoverBox option changeMsg =
    hoverInfoView
        { title = "GLU - Reference"
        , info = """This value is reference energy for the amino acid Glutamic acid (GLU). In the EvoEF2
                    output this value is called `reference_GLU`."""
        , mouseEnterMsg = EvoEF2RefGLU
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefPHEHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefPHEHoverBox option changeMsg =
    hoverInfoView
        { title = "PHE - Reference"
        , info = """This value is reference energy for the amino acid Phenylalanine (PHE). In the EvoEF2
                    output this value is called `reference_PHE`."""
        , mouseEnterMsg = EvoEF2RefPHE
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLYHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLYHoverBox option changeMsg =
    hoverInfoView
        { title = "GLY - Reference"
        , info = """This value is reference energy for the amino acid glycine (GLY). In the EvoEF2
                    output this value is called `reference_GLY`."""
        , mouseEnterMsg = EvoEF2RefGLY
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefHISHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefHISHoverBox option changeMsg =
    hoverInfoView
        { title = "HIS - Reference"
        , info = """This value is reference energy for the amino acid Histidine (HIS). In the EvoEF2
                    output this value is called `reference_HIS`."""
        , mouseEnterMsg = EvoEF2RefHIS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefILEHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefILEHoverBox option changeMsg =
    hoverInfoView
        { title = "ILE - Reference"
        , info = """This value is reference energy for the amino acid Isoleucine (ILE). In the EvoEF2
                    output this value is called `reference_ILE`."""
        , mouseEnterMsg = EvoEF2RefILE
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefLYSHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefLYSHoverBox option changeMsg =
    hoverInfoView
        { title = "LYS - Reference"
        , info = """This value is reference energy for the amino acid Lysine (LYS). In the EvoEF2
                    output this value is called `reference_LYS`."""
        , mouseEnterMsg = EvoEF2RefLYS
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefLEUHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefLEUHoverBox option changeMsg =
    hoverInfoView
        { title = "LEU - Reference"
        , info = """This value is reference energy for the amino acid Leucine (LEU). In the EvoEF2
                    output this value is called `reference_LEU`."""
        , mouseEnterMsg = EvoEF2RefLEU
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefMETHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefMETHoverBox option changeMsg =
    hoverInfoView
        { title = "MET - Reference"
        , info = """This value is reference energy for the amino acid Methionine (MET). In the EvoEF2
                    output this value is called `reference_MET`."""
        , mouseEnterMsg = EvoEF2RefMET
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefASNHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefASNHoverBox option changeMsg =
    hoverInfoView
        { title = "ASN - Reference"
        , info = """This value is reference energy for the amino acid Asparagine (ASN). In the EvoEF2
                    output this value is called `reference_ASN`."""
        , mouseEnterMsg = EvoEF2RefASN
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefPROHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefPROHoverBox option changeMsg =
    hoverInfoView
        { title = "PRO - Reference"
        , info = """This value is reference energy for the amino acid Proline (PRO). In the EvoEF2
                    output this value is called `reference_PRO`."""
        , mouseEnterMsg = EvoEF2RefPRO
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefGLNHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefGLNHoverBox option changeMsg =
    hoverInfoView
        { title = "GLN - Reference"
        , info = """This value is reference energy for the amino acid Glutamine (GLN). In the EvoEF2
                    output this value is called `reference_GLN`."""
        , mouseEnterMsg = EvoEF2RefGLN
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefARGHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefARGHoverBox option changeMsg =
    hoverInfoView
        { title = "ARG - Reference"
        , info = """This value is reference energy for the amino acid Arginine (ARG). In the EvoEF2
                    output this value is called `reference_ARG`."""
        , mouseEnterMsg = EvoEF2RefARG
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefSERHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefSERHoverBox option changeMsg =
    hoverInfoView
        { title = "SER - Reference"
        , info = """This value is reference energy for the amino acid Serine  (SER). In the EvoEF2
                    output this value is called `reference_SER`."""
        , mouseEnterMsg = EvoEF2RefSER
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTHRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTHRHoverBox option changeMsg =
    hoverInfoView
        { title = "THR - Reference"
        , info = """This value is reference energy for the amino acid Threonine (THR). In the EvoEF2
                    output this value is called `reference_THR`."""
        , mouseEnterMsg = EvoEF2RefTHR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefVALHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefVALHoverBox option changeMsg =
    hoverInfoView
        { title = "VAL - Reference"
        , info = """This value is reference energy for the amino acid Valine (VAL). In the EvoEF2
                    output this value is called `reference_VAL`."""
        , mouseEnterMsg = EvoEF2RefVAL
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTRPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTRPHoverBox option changeMsg =
    hoverInfoView
        { title = "TRP - Reference"
        , info = """This value is reference energy for the amino acid Tryptophan (TRP). In the EvoEF2
                    output this value is called `reference_TRP`."""
        , mouseEnterMsg = EvoEF2RefTRP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2RefTYRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2RefTYRHoverBox option changeMsg =
    hoverInfoView
        { title = "TYR - Reference"
        , info = """This value is reference energy for the amino acid Tyrosine (TYR). In the EvoEF2
                    output this value is called `reference_TYR`."""
        , mouseEnterMsg = EvoEF2RefTYR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 IntraR Tooltips


evoEF2IntraRVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Attractive - Intra Residue"
        , info = """This value is the Van der Waals attractive energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_vdwatt`."""
        , mouseEnterMsg = EvoEF2IntraRVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Repulsive - Intra Residue"
        , info = """This value is the Van der Waals repulsive energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_vdwrep`."""
        , mouseEnterMsg = EvoEF2IntraRVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRElecHoverBox option changeMsg =
    hoverInfoView
        { title = "Electrostatics - Intra Residue"
        , info = """This value is the Coulomb’s electrostatics energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_electr`."""
        , mouseEnterMsg = EvoEF2IntraRElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Polar - Intra Residue"
        , info = """This value is the polar atoms desolvation energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_deslvP`."""
        , mouseEnterMsg = EvoEF2IntraRDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Non Polar - Intra Residue"
        , info = """This value is the non polar atoms desolvation energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_deslvH`."""
        , mouseEnterMsg = EvoEF2IntraRDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRAAPropHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRAAPropHoverBox option changeMsg =
    hoverInfoView
        { title = "Amino Acid Propensity - Intra Residue"
        , info = """This value is the amino acid propensity energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `aapropensity`."""
        , mouseEnterMsg = EvoEF2AAProp
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRRamaHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRRamaHoverBox option changeMsg =
    hoverInfoView
        { title = "Ramachandran - Intra Residue"
        , info = """This value is the Ramachandran energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `ramachandran`."""
        , mouseEnterMsg = EvoEF2Rama
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRDunbrackHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRDunbrackHoverBox option changeMsg =
    hoverInfoView
        { title = "Dunbrack Rotamer - Intra Residue"
        , info = """This value is the Dunbrack Rotamer energy for intra residue interactions. 
                    In the EvoEF2 output this value is called `dunbrack`."""
        , mouseEnterMsg = EvoEF2Dunbrack
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Distance - Intra Residue"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from sidechain - backbone and intra residue interactions. 
                    In the EvoEF2 output this value is called `intraR_hbscbb_dis`."""
        , mouseEnterMsg = EvoEF2IntraRHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Theta - Intra Residue"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from sidechain - backbone 
                    and intra residue interactions. In the EvoEF2 output this value is 
                    called `intraR_hbscbb_the`."""
        , mouseEnterMsg = EvoEF2IntraRHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2IntraRHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2IntraRHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Phi - Intra Residue"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from sidechain - backbone 
                    and intra residue interactions. In the EvoEF2 output this value is 
                    called `intraR_hbscbb_phi`."""
        , mouseEnterMsg = EvoEF2IntraRHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 InterS Tooltips


evoEF2InterSVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Attractive - Inter Residue - Same Chain"
        , info = """This value is the Van der Waals attractive energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_vdwatt`."""
        , mouseEnterMsg = EvoEF2InterSVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Repulsive - Inter Residue - Same Chain"
        , info = """This value is the Van der Waals repulsive energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_vdwrep`."""
        , mouseEnterMsg = EvoEF2InterSVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSElecHoverBox option changeMsg =
    hoverInfoView
        { title = "Electrostatics - Inter Residue - Same Chain"
        , info = """This value is the Coulomb’s electrostatics energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_electr`."""
        , mouseEnterMsg = EvoEF2InterSElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Polar - Inter Residue - Same Chain"
        , info = """This value is the polar atoms desolvation energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_deslvP`."""
        , mouseEnterMsg = EvoEF2InterSDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Non Polar - Inter Residue - Same Chain"
        , info = """This value is the non polar atoms desolvation energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_deslvH`."""
        , mouseEnterMsg = EvoEF2InterSDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSSSbondHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSSSbondHHoverBox option changeMsg =
    hoverInfoView
        { title = "Disulfide Bonding - Inter Residue - Same Chain"
        , info = """This value is the disulfide bonding energy for inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_ssbond`."""
        , mouseEnterMsg = EvoEF2InterSSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Distance - Inter Residue - Same Chain"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from backbone - backbone and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbbbbb_dis`."""
        , mouseEnterMsg = EvoEF2InterSHBBBBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Theta - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from backbone - backbone 
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbbbbb_the`."""
        , mouseEnterMsg = EvoEF2InterSHBBBBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBBBBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBBBBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Phi - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from backbone - backbone 
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbbbbb_phi`."""
        , mouseEnterMsg = EvoEF2InterSHBBBBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Distance - Inter Residue - Same Chain"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from side chain - backbone and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscbb_dis`."""
        , mouseEnterMsg = EvoEF2InterSHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Theta - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from side chain - backbone 
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscbb_the`."""
        , mouseEnterMsg = EvoEF2InterSHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Phi - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from side chain - backbone 
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscbb_phi`."""
        , mouseEnterMsg = EvoEF2InterSHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Distance - Inter Residue - Same Chain"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from side chain - side chain and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscsc_dis`."""
        , mouseEnterMsg = EvoEF2InterSHBSCSCDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Theta - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from side chain - side chain 
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscsc_the`."""
        , mouseEnterMsg = EvoEF2InterSHBSCSCThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterSHBSCSCPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterSHBSCSCPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Phi - Inter Residue - Same Chain"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from side chain - side chain
                    and inter residue interactions - same chain. 
                    In the EvoEF2 output this value is called `interS_hbscsc_phi`."""
        , mouseEnterMsg = EvoEF2InterSHBSCSCPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ EvoEF2 InterD Tooltips


evoEF2InterDVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Attractive - Inter Residue - Different Chains"
        , info = """This value is the Van der Waals attractive energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_vdwatt`."""
        , mouseEnterMsg = EvoEF2InterDVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Repulsive - Inter Residue - Different Chains"
        , info = """This value is the Van der Waals repulsive energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_vdwrep`."""
        , mouseEnterMsg = EvoEF2InterDVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDElecHoverBox option changeMsg =
    hoverInfoView
        { title = "Electrostatics - Inter Residue - Different Chains"
        , info = """This value is the Coulomb’s electrostatics energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_electr`."""
        , mouseEnterMsg = EvoEF2InterDElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDDesolvPHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDDesolvPHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Polar - Inter Residue - Different Chains"
        , info = """This value is the polar atoms desolvation energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_deslvP`."""
        , mouseEnterMsg = EvoEF2InterDDesolvP
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDDesolvHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDDesolvHHoverBox option changeMsg =
    hoverInfoView
        { title = "Desolvation Non Polar - Inter Residue - Different Chains"
        , info = """This value is the non polar atoms desolvation energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_deslvH`."""
        , mouseEnterMsg = EvoEF2InterDDesolvH
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDSSbondHHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDSSbondHHoverBox option changeMsg =
    hoverInfoView
        { title = "Disulfide Bonding - Inter Residue - Different Chains"
        , info = """This value is the disulfide bonding energy for inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_ssbond`."""
        , mouseEnterMsg = EvoEF2InterDSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Distance - Inter Residue - Different Chains"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from backbone - backbone and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbbbbb_dis`."""
        , mouseEnterMsg = EvoEF2InterDHBBBBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Theta - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from backbone - backbone 
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbbbbb_the`."""
        , mouseEnterMsg = EvoEF2InterDHBBBBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBBBBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBBBBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Backbone Phi - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from backbone - backbone 
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbbbbb_phi`."""
        , mouseEnterMsg = EvoEF2InterDHBBBBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Distance - Inter Residue - Different Chains"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from side chain - backbone and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscbb_dis`."""
        , mouseEnterMsg = EvoEF2InterDHBSCBBDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Theta - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from side chain - backbone 
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscbb_the`."""
        , mouseEnterMsg = EvoEF2InterDHBSCBBThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCBBPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCBBPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Backbone Phi - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from side chain - backbone 
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscbb_phi`."""
        , mouseEnterMsg = EvoEF2InterDHBSCBBPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCDisHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCDisHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Distance - Inter Residue - Different Chains"
        , info = """This value is the energy for the hydrogen-acceptor distance
                    from side chain - side chain and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscsc_dis`."""
        , mouseEnterMsg = EvoEF2InterDHBSCSCDis
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCTheHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCTheHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Theta - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the donor, 
                    hydrogen and acceptor atoms (theta), from side chain - side chain 
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscsc_the`."""
        , mouseEnterMsg = EvoEF2InterDHBSCSCThe
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


evoEF2InterDHBSCSCPhiHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
evoEF2InterDHBSCSCPhiHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain Phi - Inter Residue - Different Chains"
        , info = """This value is the energy for the angle between the hydrogen, 
                    acceptor and base atoms (phi), from side chain - side chain
                    and inter residue interactions - different chains. 
                    In the EvoEF2 output this value is called `interD_hbscsc_phi`."""
        , mouseEnterMsg = EvoEF2InterDHBSCSCPhi
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ DFIRE2 Total Tooltip


dfire2TotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
dfire2TotalHoverBox option changeMsg =
    hoverInfoView
        { title = "Total DFIRE2"
        , info = """This value is the total DFIRE2 energy. This is the only field that is returned from
                    running DFIRE2 on a pdb file."""
        , mouseEnterMsg = DFIRE2Total
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
-- {{{ Rosetta Tooltips


rosettaTotalHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaTotalHoverBox option changeMsg =
    hoverInfoView
        { title = "Total Rosetta"
        , info = """This value is the total Rosetta energy. It is a weighted sum of the different 
                    Rosetta energy values. In the Rosetta `score.sc` output file, this value is called 
                    `total_score`."""
        , mouseEnterMsg = RosettaTotal
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaReferenceHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaReferenceHoverBox option changeMsg =
    hoverInfoView
        { title = "Reference"
        , info = """This value is the reference energy for the different amino acids. 
                    In the Rosetta `score.sc` output file, this value is called `ref`."""
        , mouseEnterMsg = RosettaReference
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWAttHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWAttHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Attractive"
        , info = """This value is the attractive energy between two atoms on different residues 
                    separated by distance, d. In the Rosetta `score.sc` output file, this value 
                    is called `fa_atr`."""
        , mouseEnterMsg = RosettaVDWAtt
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWRepHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWRepHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Repulsive"
        , info = """This value is the repulsive energy between two atoms on different residues 
                    separated by distance, d. In the Rosetta `score.sc` output file, this value 
                    is called `fa_rep`."""
        , mouseEnterMsg = RosettaVDWRep
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaVDWRepIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaVDWRepIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = "VDW Repulsive Intra Residue"
        , info = """This value is the repulsive energy between two atoms on the same residue 
                    separated by distance, d. In the Rosetta `score.sc` output file, this value 
                    is called `fa_intra_rep`."""
        , mouseEnterMsg = RosettaVDWRepIntraR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaElecHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaElecHoverBox option changeMsg =
    hoverInfoView
        { title = "Electrostatics"
        , info = """This value is the energy of interaction between two non-bonded charged atoms 
                    separated by distance, d. In the Rosetta `score.sc` output file, this value 
                    is called `fa_elec`."""
        , mouseEnterMsg = RosettaElec
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvIsoHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvIsoHoverBox option changeMsg =
    hoverInfoView
        { title = "Solvation Isotropic"
        , info = """This value is the Gaussian exclusion implicit solvation energy between 
                    protein atoms in different residues. In the Rosetta `score.sc` output file, 
                    this value is called `fa_sol`."""
        , mouseEnterMsg = RosettaSolvIso
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvAnisoHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvAnisoHoverBox option changeMsg =
    hoverInfoView
        { title = "Solvation Anisotropic Polar Atoms"
        , info = """This value is the orientation-dependent solvation of polar atoms 
                    assuming ideal water geometry. In the Rosetta `score.sc` output file, 
                    this value is called `lk_ball_wtd`."""
        , mouseEnterMsg = RosettaSolvAniso
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSolvIsoIntraRHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSolvIsoIntraRHoverBox option changeMsg =
    hoverInfoView
        { title = "Solvation Isotropic Intra Residue"
        , info = """This value is the Gaussian exclusion implicit solvation energy between 
                    protein atoms in the same residue. In the Rosetta `score.sc` output file, 
                    this value is called `fa_sol_intraR`."""
        , mouseEnterMsg = RosettaSolvIsoIntraR
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBLRBBHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBLRBBHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Long Range Backbone"
        , info = """This value is the energy of long range hydrogen bonds. In the Rosetta `score.sc` 
                    output file, this value is called `hbond_lr_bb`."""
        , mouseEnterMsg = RosettaHBLRBB
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBSRBBHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBSRBBHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Short Range Backbone"
        , info = """This value is the energy of short range hydrogen bonds. In the Rosetta `score.sc` 
                    output file, this value is called `hbond_sr_bb`."""
        , mouseEnterMsg = RosettaHBSRBB
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBBBSCHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBBBSCHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Backbone Sidechain"
        , info = """This value is the energy of backbone-side chain hydrogen bonds. In the Rosetta `score.sc` 
                    output file, this value is called `hbond_bb_sc`."""
        , mouseEnterMsg = RosettaHBBBSC
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaHBSCSCHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaHBSCSCHoverBox option changeMsg =
    hoverInfoView
        { title = "HB Sidechain Sidechain"
        , info = """This value is the energy of side chain-side chain hydrogen bonds. In the Rosetta `score.sc` 
                    output file, this value is called `hbond_sc`."""
        , mouseEnterMsg = RosettaHBSCSC
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaSSbondHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaSSbondHoverBox option changeMsg =
    hoverInfoView
        { title = "Disulfide Bridges"
        , info = """This value is the energy of disulfide bridges. In the Rosetta `score.sc` 
                    output file, this value is called `dslf_fa13`."""
        , mouseEnterMsg = RosettaSSbond
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaRamaHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaRamaHoverBox option changeMsg =
    hoverInfoView
        { title = "Backbone Torsion Preference"
        , info = """This value is the probability of backbone ϕ, ψ angles given the amino acid type. 
                    In the Rosetta `score.sc` output file, this value is called `rama_prepro`."""
        , mouseEnterMsg = RosettaRama
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaAAPropHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaAAPropHoverBox option changeMsg =
    hoverInfoView
        { title = "Amino Acid Propensity"
        , info = """This value is the probability of amino acid identity given the backbone ϕ, ψ angles. 
                    In the Rosetta `score.sc` output file, this value is called `p_aa_pp`."""
        , mouseEnterMsg = RosettaAAProp
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaDunbrackHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaDunbrackHoverBox option changeMsg =
    hoverInfoView
        { title = "Dunbrack Rotamer"
        , info = """This value is the probability that a chosen rotamer is native-like given 
                    backbone ϕ, ψ angles. In the Rosetta `score.sc` output file, this value 
                    is called `fa_dun`."""
        , mouseEnterMsg = RosettaDunbrack
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaOmegaPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaOmegaPenHoverBox option changeMsg =
    hoverInfoView
        { title = "Omega Penalty"
        , info = """This value is a backbone-dependent penalty for cis ω dihedrals that deviate 
                    from 0° and trans ω dihedrals that deviate from 180°. In the Rosetta `score.sc` 
                    output file, this value is called `omega`."""
        , mouseEnterMsg = RosettaOmegaPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaOpenProPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaOpenProPenHoverBox option changeMsg =
    hoverInfoView
        { title = "Open Proline Penalty"
        , info = """This value is a penalty for an open proline ring and proline ω bonding energy. 
                    In the Rosetta `score.sc` output file, this value is called `pro_close`."""
        , mouseEnterMsg = RosettaOpenProPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }


rosettaTyroPenHoverBox : HoverInfoOption -> (HoverInfoOption -> msg) -> List (Attribute msg)
rosettaTyroPenHoverBox option changeMsg =
    hoverInfoView
        { title = "Tyrosine χ3 Dihedral Angle Penalty"
        , info = """This value is a sinusoidal penalty for non-planar tyrosine χ3 dihedral angle. 
                    In the Rosetta `score.sc` output file, this value is called `yhh_planarity`."""
        , mouseEnterMsg = RosettaTyroPen
        , hoverInfoOption = option
        , changeMsg = changeMsg
        }



-- }}}
