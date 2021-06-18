module Shared.Documentation exposing
    ( MetricInfo
    , SoftwareInfo
    , metricInfo
    , softwareInfo
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Shared.Style as Style


type alias SoftwareInfo =
    { aggrescan3D : CitationConventionInfo
    , bude : CitationConventionInfo
    , dfire2 : CitationConventionInfo
    , dssp : CitationConventionInfo
    , evoef2 : CitationConventionInfo
    , hydroFit : CitationConventionInfo
    , packDens : CitationConventionInfo
    , rosetta : CitationConventionInfo
    }


type alias CitationConventionInfo =
    { citations : List String
    , convention : String
    }


type alias MetricInfo =
    { aggrescan3D : Aggrescan3DMetricInfo
    , bude : BUDEMetricInfo
    , dfire2 : DFIRE2MetricInfo
    , dssp : DSSPMetricInfo
    , evoef2 : EvoEF2MetricInfo
    , hydroFit : HydroFitMetricInfo
    , packDens : PackDensMetricInfo
    , rosetta : RosettaMetricInfo
    }


softwareInfo : SoftwareInfo
softwareInfo =
    { aggrescan3D = aggrescan3DSoftwareInfo
    , bude = budeSoftwareInfo
    , dfire2 = dfire2SoftwareInfo
    , dssp = dsspSoftwareInfo
    , evoef2 = evoef2SoftwareInfo
    , hydroFit = hydroFitSoftwareInfo
    , packDens = packDensSoftwareInfo
    , rosetta = rosettaSoftwareInfo
    }


metricInfo : MetricInfo
metricInfo =
    { aggrescan3D = aggrescan3DMetricInfo
    , bude = budeMetricInfo
    , dfire2 = dfire2MetricInfo
    , dssp = dsspMetricInfo
    , evoef2 = evoef2MetricInfo
    , hydroFit = hydroFitMetricInfo
    , packDens = packDensMetricInfo
    , rosetta = rosettaMetricInfo
    }



-- {{{ Aggrescan3D


type alias Aggrescan3DMetrics =
    { totalScore : String
    , averageScore : String
    , minimumScore : String
    , maximumScore : String
    }


type alias Aggrescan3DMetricInfo =
    { metricName : Aggrescan3DMetrics
    , metricDesc : Aggrescan3DMetrics
    , metricVarName : Aggrescan3DMetrics
    }


aggrescan3DSoftwareInfo : CitationConventionInfo
aggrescan3DSoftwareInfo =
    { citations = [ """Kuriata et al. (2019). Aggrescan3D standalone package for
                structure-based prediction of protein aggregation properties.
                Bioinformatics 35, 3834–3835.
                """ ]
    , convention = ""
    }


aggrescan3DMetricNames : Aggrescan3DMetrics
aggrescan3DMetricNames =
    { totalScore = "Total Score"
    , averageScore = "Average Score"
    , minimumScore = "Minimum Score"
    , maximumScore = "Maximum Score"
    }


aggrescan3DMetricDescs : Aggrescan3DMetrics
aggrescan3DMetricDescs =
    { totalScore = """This value is a global indicator of the aggregation propensity/solubility of the protein structure. 
                                           It depends on the protein size. It allows assessing changes in solubility promoted by 
                                           amino acid substitutions in a particular protein structure. The more negative the value, 
                                           the highest the global solubility."""
    , averageScore = """This value is a normalized indicator of the aggregation propensity/solubility of the protein structure. 
                                             Allows comparing the solubility of different protein structures. It also allows assessing 
                                             changes in solubility promoted by amino acid substitutions in a particular protein structure. 
                                             The more negative the value, the highest the normalized solubility."""
    , minimumScore = """This is the value of the most soluble residue in the structural context."""
    , maximumScore = """This is the value of the most aggregation-prone residue in the structural context."""
    }


aggrescan3DMetricVarName : Aggrescan3DMetrics
aggrescan3DMetricVarName =
    { totalScore = "aggrescan3d: total_value"
    , averageScore = "aggrescan3d: avg_value"
    , minimumScore = "aggrescan3d: min_value"
    , maximumScore = "aggrescan3d: max_value"
    }


aggrescan3DMetricInfo : Aggrescan3DMetricInfo
aggrescan3DMetricInfo =
    { metricName = aggrescan3DMetricNames
    , metricDesc = aggrescan3DMetricDescs
    , metricVarName = aggrescan3DMetricVarName
    }



-- }}}
-- {{{ BUDE


type alias BUDEMetrics =
    { totalEnergy : String
    , stericEnergy : String
    , desolvationEnergy : String
    , chargeEnergy : String
    }


type alias BUDEMetricInfo =
    { metricName : BUDEMetrics
    , metricDesc : BUDEMetrics
    , metricVarName : BUDEMetrics
    }


budeSoftwareInfo : CitationConventionInfo
budeSoftwareInfo =
    { citations =
        [ """McIntosh-Smith et al. (2012). Benchmarking Energy Efficiency,
                    Power Costs and Carbon Emissions on Heterogeneous Systems.  The
                    Computer Journal 55, 192–205.
                    """
        , """McIntosh-Smith et al. (2015). High performance in silico virtual
                    drug screening on many-core processors.  The International Journal
                    of High Performance Computing Applications 29, 119–134.
                    """
        ]
    , convention = ""
    }


budeMetricNames : BUDEMetrics
budeMetricNames =
    { totalEnergy = "Total Energy"
    , stericEnergy = "Steric Energy"
    , desolvationEnergy = "Desolvation Energy"
    , chargeEnergy = "Charge Energy"
    }


budeMetricDescs : BUDEMetrics
budeMetricDescs =
    { totalEnergy = """This value is the total BUDE force field energy. It is the sum of
                                     the steric, desolvation and charge components."""
    , stericEnergy = """This value is the steric component of the BUDE force field energy.
                                      It is calculated with a simplified Leonard-Jones potential. It is
                                      softer than the steric component of many other force fields."""
    , desolvationEnergy = """This value is the desolvation component of the BUDE force field energy."""
    , chargeEnergy = """This value is the charge component of the BUDE force field energy."""
    }


budeMetricVarName : BUDEMetrics
budeMetricVarName =
    { totalEnergy = "budeff: total"
    , stericEnergy = "budeff: steric"
    , desolvationEnergy = "budeff: desolvation"
    , chargeEnergy = "budeff: charge"
    }


budeMetricInfo : BUDEMetricInfo
budeMetricInfo =
    { metricName = budeMetricNames
    , metricDesc = budeMetricDescs
    , metricVarName = budeMetricVarName
    }



-- }}}
-- {{{ DFIRE2


type alias DFIRE2Metrics =
    { totalEnergy : String }


type alias DFIRE2MetricInfo =
    { metricName : DFIRE2Metrics
    , metricDesc : DFIRE2Metrics
    , metricVarName : DFIRE2Metrics
    }


dfire2SoftwareInfo : CitationConventionInfo
dfire2SoftwareInfo =
    { citations = [ """Yang et al. (2008). Ab initio folding of terminal segments with
                secondary structures reveals the fine difference between two closely
                related all-atom statistical energy functions.  Protein Science 17,
                1212–1219.
                """ ]
    , convention = ""
    }


dfire2MetricNames : DFIRE2Metrics
dfire2MetricNames =
    { totalEnergy = "Total Energy" }


dfire2MetricDescs : DFIRE2Metrics
dfire2MetricDescs =
    { totalEnergy = """This value is the total DFIRE2 energy. This is the only field that is returned from
                                       running DFIRE2 on a pdb file.""" }


dfire2MetricVarName : DFIRE2Metrics
dfire2MetricVarName =
    { totalEnergy = "dfire2 - total"
    }


dfire2MetricInfo : DFIRE2MetricInfo
dfire2MetricInfo =
    { metricName = dfire2MetricNames
    , metricDesc = dfire2MetricDescs
    , metricVarName = dfire2MetricVarName
    }



-- }}}
-- {{{ DSSP


type alias DSSPMetrics =
    { alpha_helix : String
    , beta_bridge : String
    , beta_strand : String
    , three_ten_helix : String
    , pi_helix : String
    , turn : String
    , bend : String
    , loop : String
    }


type alias DSSPMetricInfo =
    { metricName : DSSPMetrics
    , metricDesc : DSSPMetrics
    , metricVarName : DSSPMetrics
    }


dsspSoftwareInfo : CitationConventionInfo
dsspSoftwareInfo =
    { citations =
        [ """Kabsch et al. (1983). Dictionary of protein secondary structure:
                    Pattern recognition of hydrogen-bonded and geometrical features.
                    Biopolymers 22, 2577–2637.
                    """
        , """Touw et al. (2015). A series of PDB-related databanks for
                    everyday needs.  Nucleic Acids Research 43, D364–D368.
                    """
        ]
    , convention = ""
    }


dsspMetricNames : DSSPMetrics
dsspMetricNames =
    { alpha_helix = "H"
    , beta_bridge = "B"
    , beta_strand = "E"
    , three_ten_helix = "G"
    , pi_helix = "I"
    , turn = "T"
    , bend = "S"
    , loop = "-"
    }


dsspMetricDescs : DSSPMetrics
dsspMetricDescs =
    { alpha_helix = "α-helix"
    , beta_bridge = "Isolated β-bridge"
    , beta_strand = "Extended β-strand"
    , three_ten_helix = "3-10 helix"
    , pi_helix = "π-helix"
    , turn = "Hydrogen-bonded turn"
    , bend = "Bend"
    , loop = "Loop"
    }


dsspMetricVarName : DSSPMetrics
dsspMetricVarName =
    { alpha_helix = "Not included in csv output"
    , beta_bridge = "Not included in csv output"
    , beta_strand = "Not included in csv output"
    , three_ten_helix = "Not included in csv output"
    , pi_helix = "Not included in csv output"
    , turn = "Not included in csv output"
    , bend = "Not included in csv output"
    , loop = "Not included in csv output"
    }


dsspMetricInfo : DSSPMetricInfo
dsspMetricInfo =
    { metricName = dsspMetricNames
    , metricDesc = dsspMetricDescs
    , metricVarName = dsspMetricVarName
    }



-- }}}
-- {{{ EvoEF2


type alias EvoEF2Metrics =
    { totalEnergy : String
    , refTotalEnergy : String
    , intraRTotalEnergy : String
    , interSTotalEnergy : String
    , interDTotalEnergy : String
    , referenceALA : String
    , referenceCYS : String
    , referenceASP : String
    , referenceGLU : String
    , referencePHE : String
    , referenceGLY : String
    , referenceHIS : String
    , referenceILE : String
    , referenceLYS : String
    , referenceLEU : String
    , referenceMET : String
    , referenceASN : String
    , referencePRO : String
    , referenceGLN : String
    , referenceARG : String
    , referenceSER : String
    , referenceTHR : String
    , referenceVAL : String
    , referenceTRP : String
    , referenceTYR : String
    , intraRVdwatt : String
    , intraRVdwrep : String
    , intraRElectr : String
    , intraRDeslvP : String
    , intraRDeslvH : String
    , intraRHbscbbDis : String
    , intraRHbscbbThe : String
    , intraRHbscbbPhi : String
    , aapropensity : String
    , ramachandran : String
    , dunbrack : String
    , interSVdwatt : String
    , interSVdwrep : String
    , interSElectr : String
    , interSDeslvP : String
    , interSDeslvH : String
    , interSSsbond : String
    , interSHbbbbbDis : String
    , interSHbbbbbThe : String
    , interSHbbbbbPhi : String
    , interSHbscbbDis : String
    , interSHbscbbThe : String
    , interSHbscbbPhi : String
    , interSHbscscDis : String
    , interSHbscscThe : String
    , interSHbscscPhi : String
    , interDVdwatt : String
    , interDVdwrep : String
    , interDElectr : String
    , interDDeslvP : String
    , interDDeslvH : String
    , interDSsbond : String
    , interDHbbbbbDis : String
    , interDHbbbbbThe : String
    , interDHbbbbbPhi : String
    , interDHbscbbDis : String
    , interDHbscbbThe : String
    , interDHbscbbPhi : String
    , interDHbscscDis : String
    , interDHbscscThe : String
    , interDHbscscPhi : String
    }


type alias EvoEF2MetricInfo =
    { metricName : EvoEF2Metrics
    , metricDesc : EvoEF2Metrics
    , metricVarName : EvoEF2Metrics
    }


evoef2SoftwareInfo : CitationConventionInfo
evoef2SoftwareInfo =
    { citations = [ """Huang et al. (2020). EvoEF2: accurate and fast energy function for
                computational protein design.  Bioinformatics 36, 1135–1142.
                """ ]
    , convention = ""
    }


evoef2MetricNames : EvoEF2Metrics
evoef2MetricNames =
    { totalEnergy = "Total Energy"
    , refTotalEnergy = "Reference Energy"
    , intraRTotalEnergy = "Intra Residue Energy"
    , interSTotalEnergy = "Inter Residue - Same Chain Energy"
    , interDTotalEnergy = "Inter Residue - Different Chains Energy"
    , referenceALA = "ALA - Reference"
    , referenceCYS = "CYS - Reference"
    , referenceASP = "ASP - Reference"
    , referenceGLU = "GLU - Reference"
    , referencePHE = "PHE - Reference"
    , referenceGLY = "GLY - Reference"
    , referenceHIS = "HIS - Reference"
    , referenceILE = "ILE - Reference"
    , referenceLYS = "LYS - Reference"
    , referenceLEU = "LEU - Reference"
    , referenceMET = "MET - Reference"
    , referenceASN = "ASN - Reference"
    , referencePRO = "PRO - Reference"
    , referenceGLN = "GLN - Reference"
    , referenceARG = "ARG - Reference"
    , referenceSER = "SER - Reference"
    , referenceTHR = "THR - Reference"
    , referenceVAL = "VAL - Reference"
    , referenceTRP = "TRP - Reference"
    , referenceTYR = "TYR - Reference"
    , intraRVdwatt = "VDW Attractive - Intra Residue"
    , intraRVdwrep = "VDW Repulsive - Intra Residue"
    , intraRElectr = "Electrostatics - Intra Residue"
    , intraRDeslvP = "Desolvation Polar - Intra Residue"
    , intraRDeslvH = "Desolvation Non Polar - Intra Residue"
    , intraRHbscbbDis = "HB Sidechain Backbone Distance - Intra Residue"
    , intraRHbscbbThe = "HB Sidechain Backbone Theta - Intra Residue"
    , intraRHbscbbPhi = "HB Sidechain Backbone Phi - Intra Residue"
    , aapropensity = "Amino Acid Propensity - Intra Residue"
    , ramachandran = "Ramachandran - Intra Residue"
    , dunbrack = "Dunbrack Rotamer - Intra Residue"
    , interSVdwatt = "VDW Attractive - Inter Residue - Same Chain"
    , interSVdwrep = "VDW Repulsive - Inter Residue - Same Chain"
    , interSElectr = "Electrostatics - Inter Residue - Same Chain"
    , interSDeslvP = "Desolvation Polar - Inter Residue - Same Chain"
    , interSDeslvH = "Desolvation Non Polar - Inter Residue - Same Chain"
    , interSSsbond = "Disulfide Bonding - Inter Residue - Same Chain"
    , interSHbbbbbDis = "HB Backbone Backbone Distance - Inter Residue - Same Chain"
    , interSHbbbbbThe = "HB Backbone Backbone Theta - Inter Residue - Same Chain"
    , interSHbbbbbPhi = "HB Backbone Backbone Phi - Inter Residue - Same Chain"
    , interSHbscbbDis = "HB Sidechain Backbone Distance - Inter Residue - Same Chain"
    , interSHbscbbThe = "HB Sidechain Backbone Theta - Inter Residue - Same Chain"
    , interSHbscbbPhi = "HB Sidechain Backbone Phi - Inter Residue - Same Chain"
    , interSHbscscDis = "HB Sidechain Sidechain Distance - Inter Residue - Same Chain"
    , interSHbscscThe = "HB Sidechain Sidechain Theta - Inter Residue - Same Chain"
    , interSHbscscPhi = "HB Sidechain Sidechain Phi - Inter Residue - Same Chain"
    , interDVdwatt = "VDW Attractive - Inter Residue - Different Chains"
    , interDVdwrep = "VDW Repulsive - Inter Residue - Different Chains"
    , interDElectr = "Electrostatics - Inter Residue - Different Chains"
    , interDDeslvP = "Desolvation Polar - Inter Residue - Different Chains"
    , interDDeslvH = "Desolvation Non Polar - Inter Residue - Different Chains"
    , interDSsbond = "Disulfide Bonding - Inter Residue - Different Chains"
    , interDHbbbbbDis = "HB Backbone Backbone Distance - Inter Residue - Different Chains"
    , interDHbbbbbThe = "HB Backbone Backbone Theta - Inter Residue - Different Chains"
    , interDHbbbbbPhi = "HB Backbone Backbone Phi - Inter Residue - Different Chains"
    , interDHbscbbDis = "HB Sidechain Backbone Distance - Inter Residue - Different Chains"
    , interDHbscbbThe = "HB Sidechain Backbone Theta - Inter Residue - Different Chains"
    , interDHbscbbPhi = "HB Sidechain Backbone Phi - Inter Residue - Different Chains"
    , interDHbscscDis = "HB Sidechain Sidechain Distance - Inter Residue - Different Chains"
    , interDHbscscThe = "HB Sidechain Sidechain Theta - Inter Residue - Different Chains"
    , interDHbscscPhi = "HB Sidechain Sidechain Phi - Inter Residue - Different Chains"
    }


evoef2MetricDescs : EvoEF2Metrics
evoef2MetricDescs =
    { totalEnergy = """This value is the total EvoEF2 energy. It is the sum of the reference, 
                                       intra residue, inter residue - same chain and inter residue - different 
                                       chains, energy values. In the EvoEF2 output this field is called `Total`."""
    , refTotalEnergy = """This value is the total reference energy. This value is not included in 
                                          the EvoEF2 output and is calculated in DE-STRESS."""
    , intraRTotalEnergy = """This value is the total energy for intra residue interactions. This value is 
                                             not included in the EvoEF2 output and is calculated in DE-STRESS."""
    , interSTotalEnergy = """This value is the total energy for inter residue interactions in the same chain. 
                                             This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
    , interDTotalEnergy = """This value is the total energy for inter residue interactions in different chains. 
                                             This value is not included in the EvoEF2 output and is calculated in DE-STRESS."""
    , referenceALA = """This value is reference energy for the amino acid Alanine (ALA). In the EvoEF2
                                      output this value is called `reference_ALA`."""
    , referenceCYS = """This value is reference energy for the amino acid Cysteine (CYS). In the EvoEF2
                                      output this value is called `reference_CYS`."""
    , referenceASP = """This value is reference energy for the amino acid Aspartic acid (ASP). In the EvoEF2
                                      output this value is called `reference_ASP`."""
    , referenceGLU = """This value is reference energy for the amino acid Glutamic acid (GLU). In the EvoEF2
                                      output this value is called `reference_GLU`."""
    , referencePHE = """This value is reference energy for the amino acid Phenylalanine (PHE). In the EvoEF2
                                      output this value is called `reference_PHE`."""
    , referenceGLY = """This value is reference energy for the amino acid glycine (GLY). In the EvoEF2
                                      output this value is called `reference_GLY`."""
    , referenceHIS = """This value is reference energy for the amino acid Histidine (HIS). In the EvoEF2
                                      output this value is called `reference_HIS`."""
    , referenceILE = """This value is reference energy for the amino acid Isoleucine (ILE). In the EvoEF2
                                      output this value is called `reference_ILE`."""
    , referenceLYS = """This value is reference energy for the amino acid Lysine (LYS). In the EvoEF2
                                      output this value is called `reference_LYS`."""
    , referenceLEU = """This value is reference energy for the amino acid Leucine (LEU). In the EvoEF2
                                      output this value is called `reference_LEU`."""
    , referenceMET = """This value is reference energy for the amino acid Methionine (MET). In the EvoEF2
                                      output this value is called `reference_MET`."""
    , referenceASN = """This value is reference energy for the amino acid Asparagine (ASN). In the EvoEF2
                                      output this value is called `reference_ASN`."""
    , referencePRO = """This value is reference energy for the amino acid Proline (PRO). In the EvoEF2
                                      output this value is called `reference_PRO`."""
    , referenceGLN = """This value is reference energy for the amino acid Glutamine (GLN). In the EvoEF2
                                      output this value is called `reference_GLN`."""
    , referenceARG = """This value is reference energy for the amino acid Arginine (ARG). In the EvoEF2
                                      output this value is called `reference_ARG`."""
    , referenceSER = """This value is reference energy for the amino acid Serine  (SER). In the EvoEF2
                                      output this value is called `reference_SER`."""
    , referenceTHR = """This value is reference energy for the amino acid Threonine (THR). In the EvoEF2
                                      output this value is called `reference_THR`."""
    , referenceVAL = """This value is reference energy for the amino acid Valine (VAL). In the EvoEF2
                                      output this value is called `reference_VAL`."""
    , referenceTRP = """This value is reference energy for the amino acid Tryptophan (TRP). In the EvoEF2
                                      output this value is called `reference_TRP`."""
    , referenceTYR = """This value is reference energy for the amino acid Tyrosine (TYR). In the EvoEF2
                                      output this value is called `reference_TYR`."""
    , intraRVdwatt = """This value is the Van der Waals attractive energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `intraR_vdwatt`."""
    , intraRVdwrep = """This value is the Van der Waals repulsive energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `intraR_vdwrep`."""
    , intraRElectr = """This value is the Coulomb’s electrostatics energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `intraR_electr`."""
    , intraRDeslvP = """This value is the polar atoms desolvation energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `intraR_deslvP`."""
    , intraRDeslvH = """This value is the non polar atoms desolvation energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `intraR_deslvH`."""
    , intraRHbscbbDis = """This value is the energy for the hydrogen-acceptor distance
                                         from sidechain - backbone and intra residue interactions. 
                                         In the EvoEF2 output this value is called `intraR_hbscbb_dis`."""
    , intraRHbscbbThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from sidechain - backbone 
                                         and intra residue interactions. In the EvoEF2 output this value is 
                                         called `intraR_hbscbb_the`."""
    , intraRHbscbbPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from sidechain - backbone 
                                         and intra residue interactions. In the EvoEF2 output this value is 
                                         called `intraR_hbscbb_phi`."""
    , aapropensity = """This value is the amino acid propensity energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `aapropensity`."""
    , ramachandran = """This value is the Ramachandran energy for intra residue interactions. 
                                      In the EvoEF2 output this value is called `ramachandran`."""
    , dunbrack = """This value is the Dunbrack Rotamer energy for intra residue interactions. 
                                  In the EvoEF2 output this value is called `dunbrack`."""
    , interSVdwatt = """This value is the Van der Waals attractive energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_vdwatt`."""
    , interSVdwrep = """This value is the Van der Waals repulsive energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_vdwrep`."""
    , interSElectr = """This value is the Coulomb’s electrostatics energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_electr`."""
    , interSDeslvP = """This value is the polar atoms desolvation energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_deslvP`."""
    , interSDeslvH = """This value is the non polar atoms desolvation energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_deslvH`."""
    , interSSsbond = """This value is the disulfide bonding energy for inter residue interactions - same chain. 
                                      In the EvoEF2 output this value is called `interS_ssbond`."""
    , interSHbbbbbDis = """This value is the energy for the hydrogen-acceptor distance
                                         from backbone - backbone and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbbbbb_dis`."""
    , interSHbbbbbThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from backbone - backbone 
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbbbbb_the`."""
    , interSHbbbbbPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from backbone - backbone 
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbbbbb_phi`."""
    , interSHbscbbDis = """This value is the energy for the hydrogen-acceptor distance
                                         from side chain - backbone and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscbb_dis`."""
    , interSHbscbbThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from side chain - backbone 
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscbb_the`."""
    , interSHbscbbPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from side chain - backbone 
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscbb_phi`."""
    , interSHbscscDis = """This value is the energy for the hydrogen-acceptor distance
                                         from side chain - side chain and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscsc_dis`."""
    , interSHbscscThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from side chain - side chain 
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscsc_the`."""
    , interSHbscscPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from side chain - side chain
                                         and inter residue interactions - same chain. 
                                         In the EvoEF2 output this value is called `interS_hbscsc_phi`."""
    , interDVdwatt = """This value is the Van der Waals attractive energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_vdwatt`."""
    , interDVdwrep = """This value is the Van der Waals repulsive energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_vdwrep`."""
    , interDElectr = """This value is the Coulomb’s electrostatics energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_electr`."""
    , interDDeslvP = """This value is the polar atoms desolvation energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_deslvP`."""
    , interDDeslvH = """This value is the non polar atoms desolvation energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_deslvH`."""
    , interDSsbond = """This value is the disulfide bonding energy for inter residue interactions - different chains. 
                                      In the EvoEF2 output this value is called `interD_ssbond`."""
    , interDHbbbbbDis = """This value is the energy for the hydrogen-acceptor distance
                                         from backbone - backbone and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbbbbb_dis`."""
    , interDHbbbbbThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from backbone - backbone 
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbbbbb_the`."""
    , interDHbbbbbPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from backbone - backbone 
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbbbbb_phi`."""
    , interDHbscbbDis = """This value is the energy for the hydrogen-acceptor distance
                                         from side chain - backbone and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscbb_dis`."""
    , interDHbscbbThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from side chain - backbone 
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscbb_the`."""
    , interDHbscbbPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from side chain - backbone 
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscbb_phi`."""
    , interDHbscscDis = """This value is the energy for the hydrogen-acceptor distance
                                         from side chain - side chain and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscsc_dis`."""
    , interDHbscscThe = """This value is the energy for the angle between the donor, 
                                         hydrogen and acceptor atoms (theta), from side chain - side chain 
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscsc_the`."""
    , interDHbscscPhi = """This value is the energy for the angle between the hydrogen, 
                                         acceptor and base atoms (phi), from side chain - side chain
                                         and inter residue interactions - different chains. 
                                         In the EvoEF2 output this value is called `interD_hbscsc_phi`."""
    }


evoef2MetricVarName : EvoEF2Metrics
evoef2MetricVarName =
    { totalEnergy = "evoef2: total"
    , refTotalEnergy = "evoef2: ref total"
    , intraRTotalEnergy = "evoef2: intraR total"
    , interSTotalEnergy = "evoef2: interS total"
    , interDTotalEnergy = "evoef2 - interD total"
    , referenceALA = "Not included in csv output"
    , referenceCYS = "Not included in csv output"
    , referenceASP = "Not included in csv output"
    , referenceGLU = "Not included in csv output"
    , referencePHE = "Not included in csv output"
    , referenceGLY = "Not included in csv output"
    , referenceHIS = "Not included in csv output"
    , referenceILE = "Not included in csv output"
    , referenceLYS = "Not included in csv output"
    , referenceLEU = "Not included in csv output"
    , referenceMET = "Not included in csv output"
    , referenceASN = "Not included in csv output"
    , referencePRO = "Not included in csv output"
    , referenceGLN = "Not included in csv output"
    , referenceARG = "Not included in csv output"
    , referenceSER = "Not included in csv output"
    , referenceTHR = "Not included in csv output"
    , referenceVAL = "Not included in csv output"
    , referenceTRP = "Not included in csv output"
    , referenceTYR = "Not included in csv output"
    , intraRVdwatt = "Not included in csv output"
    , intraRVdwrep = "Not included in csv output"
    , intraRElectr = "Not included in csv output"
    , intraRDeslvP = "Not included in csv output"
    , intraRDeslvH = "Not included in csv output"
    , intraRHbscbbDis = "Not included in csv output"
    , intraRHbscbbThe = "Not included in csv output"
    , intraRHbscbbPhi = "Not included in csv output"
    , aapropensity = "Not included in csv output"
    , ramachandran = "Not included in csv output"
    , dunbrack = "Not included in csv output"
    , interSVdwatt = "Not included in csv output"
    , interSVdwrep = "Not included in csv output"
    , interSElectr = "Not included in csv output"
    , interSDeslvP = "Not included in csv output"
    , interSDeslvH = "Not included in csv output"
    , interSSsbond = "Not included in csv output"
    , interSHbbbbbDis = "Not included in csv output"
    , interSHbbbbbThe = "Not included in csv output"
    , interSHbbbbbPhi = "Not included in csv output"
    , interSHbscbbDis = "Not included in csv output"
    , interSHbscbbThe = "Not included in csv output"
    , interSHbscbbPhi = "Not included in csv output"
    , interSHbscscDis = "Not included in csv output"
    , interSHbscscThe = "Not included in csv output"
    , interSHbscscPhi = "Not included in csv output"
    , interDVdwatt = "Not included in csv output"
    , interDVdwrep = "Not included in csv output"
    , interDElectr = "Not included in csv output"
    , interDDeslvP = "Not included in csv output"
    , interDDeslvH = "Not included in csv output"
    , interDSsbond = "Not included in csv output"
    , interDHbbbbbDis = "Not included in csv output"
    , interDHbbbbbThe = "Not included in csv output"
    , interDHbbbbbPhi = "Not included in csv output"
    , interDHbscbbDis = "Not included in csv output"
    , interDHbscbbThe = "Not included in csv output"
    , interDHbscbbPhi = "Not included in csv output"
    , interDHbscscDis = "Not included in csv output"
    , interDHbscscThe = "Not included in csv output"
    , interDHbscscPhi = "Not included in csv output"
    }


evoef2MetricInfo : EvoEF2MetricInfo
evoef2MetricInfo =
    { metricName = evoef2MetricNames
    , metricDesc = evoef2MetricDescs
    , metricVarName = evoef2MetricVarName
    }



-- }}}
-- {{{ Hydrophobic Fitness


type alias HydroFitMetrics =
    { hydroFit : String }


type alias HydroFitMetricInfo =
    { metricName : HydroFitMetrics
    , metricDesc : HydroFitMetrics
    , metricVarName : HydroFitMetrics
    }


hydroFitSoftwareInfo : CitationConventionInfo
hydroFitSoftwareInfo =
    { citations =
        [ """Huang et al. (1995). Recognizing native folds by the arrangement
                    of hydrophobic and polar residues. J Mol Biol 252, 709–720.
                    """
        , """Wood et al. (2017). ISAMBARD: an open-source computational
                    environment for biomolecular analysis, modelling and design.
                    Bioinformatics 33, 3043–3050.
                    """
        ]
    , convention = ""
    }


hydroFitMetricNames : HydroFitMetrics
hydroFitMetricNames =
    { hydroFit = "Hydrophobic Fitness" }


hydroFitMetricDescs : HydroFitMetrics
hydroFitMetricDescs =
    { hydroFit = """ This value is an efficient centroid-based method for calculating the packing quality of the protein
                                       structure. For this method C, F, I, L, M, V, W and Y are considered hydrophobic.""" }


hydroFitMetricVarName : HydroFitMetrics
hydroFitMetricVarName =
    { hydroFit = "hydrophobic fitness" }


hydroFitMetricInfo : HydroFitMetricInfo
hydroFitMetricInfo =
    { metricName = hydroFitMetricNames
    , metricDesc = hydroFitMetricDescs
    , metricVarName = hydroFitMetricVarName
    }



-- }}}
-- {{{ Packing Density


type alias PackDensMetrics =
    { packDens : String }


type alias PackDensMetricInfo =
    { metricName : PackDensMetrics
    , metricDesc : PackDensMetrics
    , metricVarName : PackDensMetrics
    }


packDensSoftwareInfo : CitationConventionInfo
packDensSoftwareInfo =
    { citations =
        [ """Weiss (2007). On the interrelationship between atomic
                    displacement parameters (ADPs) and coordinates in protein
                    structures. Acta Crystallogr D Biol Crystallogr 63, 1235–1242.
                    """
        , """Wood et al. (2017). ISAMBARD: an open-source computational
                    environment for biomolecular analysis, modelling and design.
                    Bioinformatics 33, 3043–3050.
                    """
        ]
    , convention = ""
    }


packDensMetricNames : PackDensMetrics
packDensMetricNames =
    { packDens = "Packing Density" }


packDensMetricDescs : PackDensMetrics
packDensMetricDescs =
    { packDens = """ This value is an efficient centroid-based method for calculating the packing quality of the protein
                                       structure. For this method C, F, I, L, M, V, W and Y are considered hydrophobic.""" }


packDensMetricVarName : PackDensMetrics
packDensMetricVarName =
    { packDens = "packing density" }


packDensMetricInfo : PackDensMetricInfo
packDensMetricInfo =
    { metricName = packDensMetricNames
    , metricDesc = packDensMetricDescs
    , metricVarName = packDensMetricVarName
    }



-- }}}
-- {{{ Rosetta


type alias RosettaMetrics =
    { totalEnergy : String
    , reference : String
    , vdwAtt : String
    , vdwRep : String
    , vdwRepIntraR : String
    , electrostatics : String
    , solvIso : String
    , solvAniso : String
    , solvIsoIntraR : String
    , hblrbb : String
    , hbsrbb : String
    , hbbbsc : String
    , hbscsc : String
    , ssBond : String
    , rama : String
    , aaProp : String
    , dunbrack : String
    , omegaPen : String
    , openProPen : String
    , tyroPen : String
    }


type alias RosettaMetricInfo =
    { metricName : RosettaMetrics
    , metricDesc : RosettaMetrics
    , metricVarName : RosettaMetrics
    }


rosettaSoftwareInfo : CitationConventionInfo
rosettaSoftwareInfo =
    { citations = [ """Alford et al. (2017). The Rosetta All-Atom Energy Function for
                Macromolecular Modeling and Design.  J. Chem. Theory Comput. 13,
                3031–3048.
                """ ]
    , convention = ""
    }


rosettaMetricNames : RosettaMetrics
rosettaMetricNames =
    { totalEnergy = "Total Energy"
    , reference = "Reference"
    , vdwAtt = "VDW Attractive"
    , vdwRep = "VDW Repulsive"
    , vdwRepIntraR = "VDW Repulsive Intra Residue"
    , electrostatics = "Electrostatics"
    , solvIso = "Solvation Isotropic"
    , solvAniso = "Solvation Anisotropic Polar Atoms"
    , solvIsoIntraR = "Solvation Isotropic Intra Residue"
    , hblrbb = "HB Long Range Backbone"
    , hbsrbb = "HB Short Range Backbone"
    , hbbbsc = "HB Backbone Sidechain"
    , hbscsc = "HB Sidechain Sidechain"
    , ssBond = "Disulfide Bridges"
    , rama = "Backbone Torsion Preference"
    , aaProp = "Amino Acid Propensity"
    , dunbrack = "Dunbrack Rotamer"
    , omegaPen = "Omega Penalty"
    , openProPen = "Open Proline Penalty"
    , tyroPen = "Tyrosine χ3 Dihedral Angle Penalty"
    }


rosettaMetricDescs : RosettaMetrics
rosettaMetricDescs =
    { totalEnergy = """This value is the total Rosetta energy. It is a weighted sum of the different 
                                        Rosetta energy values. In the Rosetta `score.sc` output file, this value is called 
                                        `total_score`."""
    , reference = """This value is the reference energy for the different amino acids. 
                                      In the Rosetta `score.sc` output file, this value is called `ref`."""
    , vdwAtt = """This value is the attractive energy between two atoms on different residues 
                                   separated by distance, d. In the Rosetta `score.sc` output file, this value 
                                   is called `fa_atr`."""
    , vdwRep = """This value is the repulsive energy between two atoms on different residues 
                                   separated by distance, d. In the Rosetta `score.sc` output file, this value 
                                   is called `fa_rep`."""
    , vdwRepIntraR = """This value is the repulsive energy between two atoms on the same residue 
                                         separated by distance, d. In the Rosetta `score.sc` output file, this value 
                                         is called `fa_intra_rep`."""
    , electrostatics = """This value is the energy of interaction between two non-bonded charged atoms 
                                           separated by distance, d. In the Rosetta `score.sc` output file, this value 
                                           is called `fa_elec`."""
    , solvIso = """This value is the Gaussian exclusion implicit solvation energy between 
                                    protein atoms in different residues. In the Rosetta `score.sc` output file, 
                                    this value is called `fa_sol`."""
    , solvAniso = """This value is the orientation-dependent solvation of polar atoms 
                                      assuming ideal water geometry. In the Rosetta `score.sc` output file, 
                                      this value is called `lk_ball_wtd`."""
    , solvIsoIntraR = """This value is the Gaussian exclusion implicit solvation energy between 
                                          protein atoms in the same residue. In the Rosetta `score.sc` output file, 
                                          this value is called `fa_sol_intraR`."""
    , hblrbb = """This value is the energy of long range hydrogen bonds. In the Rosetta `score.sc` 
                                   output file, this value is called `hbond_lr_bb`."""
    , hbsrbb = """This value is the energy of short range hydrogen bonds. In the Rosetta `score.sc` 
                                   output file, this value is called `hbond_sr_bb`."""
    , hbbbsc = """This value is the energy of backbone-side chain hydrogen bonds. In the Rosetta `score.sc` 
                                   output file, this value is called `hbond_bb_sc`."""
    , hbscsc = """This value is the energy of side chain-side chain hydrogen bonds. In the Rosetta `score.sc` 
                                   output file, this value is called `hbond_sc`."""
    , ssBond = """This value is the energy of disulfide bridges. In the Rosetta `score.sc` 
                                   output file, this value is called `dslf_fa13`."""
    , rama = """This value is the probability of backbone ϕ, ψ angles given the amino acid type. 
                                 In the Rosetta `score.sc` output file, this value is called `rama_prepro`."""
    , aaProp = """This value is the probability of amino acid identity given the backbone ϕ, ψ angles. 
                                   In the Rosetta `score.sc` output file, this value is called `p_aa_pp`."""
    , dunbrack = """This value is the probability that a chosen rotamer is native-like given 
                                     backbone ϕ, ψ angles. In the Rosetta `score.sc` output file, this value 
                                     is called `fa_dun`."""
    , omegaPen = """This value is a backbone-dependent penalty for cis ω dihedrals that deviate 
                                     from 0° and trans ω dihedrals that deviate from 180°. In the Rosetta `score.sc` 
                                     output file, this value is called `omega`."""
    , openProPen = """This value is a penalty for an open proline ring and proline ω bonding energy. 
                                       In the Rosetta `score.sc` output file, this value is called `pro_close`."""
    , tyroPen = """This value is a sinusoidal penalty for non-planar tyrosine χ3 dihedral angle. 
                                    In the Rosetta `score.sc` output file, this value is called `yhh_planarity`."""
    }


rosettaMetricVarName : RosettaMetrics
rosettaMetricVarName =
    { totalEnergy = "rosetta - total"
    , reference = "Not included in csv output"
    , vdwAtt = "rosetta - fa_atr"
    , vdwRep = "rosetta - fa_rep"
    , vdwRepIntraR = "rosetta - fa_intra_rep"
    , electrostatics = "rosetta - fa_elec"
    , solvIso = "rosetta - fa_sol"
    , solvAniso = "rosetta - lk_ball_wtd"
    , solvIsoIntraR = "rosetta - fa_intra_sol_xover4"
    , hblrbb = "rosetta - hbond_lr_bb"
    , hbsrbb = "rosetta - hbond_sr_bb"
    , hbbbsc = "rosetta - hbond_bb_sc"
    , hbscsc = "rosetta - hbond_sc"
    , ssBond = "rosetta - dslf_fa13"
    , rama = "rosetta - rama_prepro"
    , aaProp = "rosetta - p_aa_pp"
    , dunbrack = "rosetta - fa_dun"
    , omegaPen = "rosetta - omega"
    , openProPen = "rosetta - pro_close"
    , tyroPen = "rosetta - yhh_planarity"
    }


rosettaMetricInfo : RosettaMetricInfo
rosettaMetricInfo =
    { metricName = rosettaMetricNames
    , metricDesc = rosettaMetricDescs
    , metricVarName = rosettaMetricVarName
    }
