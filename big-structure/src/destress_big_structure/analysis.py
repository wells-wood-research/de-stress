"""Contains function for running the analytics sweet."""
from collections import Counter
from typing import Dict, List, Optional, Tuple, Set
import os
import pathlib
import subprocess
import tempfile
import re
import json
import csv
import warnings

from bs4 import BeautifulSoup
import ampal
import budeff
import budeff.force_field
import isambard.evaluation as ev
import numpy as np
import requests
import logging

from .elm_types import (
    DesignMetrics,
    BudeFFOutput,
    EvoEF2Output,
    DFIRE2Output,
    RosettaOutput,
    Aggrescan3DOutput,
    SequenceInfo,
)
from destress_big_structure.settings import (
    EVOEF2_BINARY_PATH,
    DFIRE2_FOLDER_PATH,
    ROSETTA_BINARY_PATH,
    AGGRESCAN3D_SCRIPT_PATH,
    MAX_RUN_TIME,
)

MAX_RUN_TIME = float(MAX_RUN_TIME)


# We're suppressing warnings about atoms not being parameterised in BUDE FF
# I'm not reporting this as the user should look into BUDE FF to understand how it works
warnings.simplefilter("ignore", budeff.force_field.NotParameterisedWarning)

# {{{ Input Validation


def find_disallowed_monomers(assembly: ampal.Assembly) -> Optional[Set[str]]:
    """Tests for valid monomers in assembly.

    Defining a function to test that the assembly object only contains
    the 20 canonical amino acids and H20 monomer codes. This is because
    some of the design metrics in DE-STRESS will only work for these
    monomers.

    Parameters
    ----------
    assembly: ampal.Assembly
        An assembly object which is created from an input PDB file. The Ample
        python package is used to create this object.

    Returns
    -------
    molset: Set[str]
        A set containing the monomer codes that are not included in the
        accepted monomers set.
    """

    # Creating a set of the 20 canonical amino acid and H2O codes
    # These are the monomers that are accepted for the PDB descriptive statistics
    allowed_monomers = {
        "ALA",
        "ARG",
        "ASN",
        "ASP",
        "CYS",
        "GLN",
        "GLU",
        "GLY",
        "HIS",
        "ILE",
        "LEU",
        "LYS",
        "MET",
        "PHE",
        "PRO",
        "SER",
        "THR",
        "TRP",
        "TYR",
        "VAL",
        "HOH",
    }

    # Creating a set so that we only have the unique mol codes
    mol_set = {x.mol_code for x in assembly.get_monomers()}
    disallowed_monomers = mol_set.difference(allowed_monomers)

    return disallowed_monomers


def convert_string_to_float(input_string: str) -> Optional[float]:

    """Defining a function to convert a string to a float while handling the
    error when the string cannot be converted.
    Parameters
    ----------
    input_string: str
        Input string to be converted to a float.
    Returns
    -------
    output_float: Optional[float]
        Output float if the conversion is successful. None if the conversion has failed.
    """

    output_float: Optional[float]
    try:
        output_float = float(input_string)
    except ValueError:
        output_float = None

    return output_float


# }}}
# {{{ Jpred Submission


class JpredSubmissionError(Exception):
    pass


class JpredStatusError(Exception):
    pass


class JpredResultsError(Exception):
    pass


class JpredSubmission:
    host = "http://www.compbio.dundee.ac.uk/jpred4/"
    rest_endpoint = host + "cgi-bin/rest/"
    results_endpoint = host + "results/"

    def __init__(self, sequences: List[str]):
        assert sequences, "No sequences provided."
        assert all(sequences), "An empty string was given as an input."
        assert all(
            s.isalnum() for s in sequences
        ), "Sequences should use single letter codes"
        self.unique_sequences = set(sequences)
        self.total_sequences = len(list(self.unique_sequences))
        self.inprogress_job_urls: List[str] = []
        self.complete_job_urls: List[str] = []

    def make_queries(self) -> List[str]:
        options: Dict[str, str] = {
            "format": "single",
            "skipPDB": "on",
        }
        queries = []
        for seq in self.unique_sequences:
            option_strings = ["=".join(i) for i in options.items()]
            seq_string = f">query\n{seq}"
            queries.append("£€£€".join(option_strings + [seq_string]))
        return queries

    def submit_jobs(self):
        submission_reponses = [
            requests.post(
                f"{self.rest_endpoint}/job",
                data=query.encode("utf-8"),
                headers={"Content-type": "text/txt"},
            )
            for query in self.make_queries()
        ]
        for sr in submission_reponses:
            if sr.status_code == 202:
                self.inprogress_job_urls.append(sr.headers["Location"])
            else:
                raise JpredSubmissionError(
                    "Failed to submit job to JPred, error code: {sr.stastatus_code}"
                )
        return

    def update_status(self) -> bool:
        """Updates the status of Jpred jobs and returns true if all are complete."""
        status_responses = [requests.get(url) for url in self.inprogress_job_urls]
        updated_in_progress: List[str] = []
        for url in self.inprogress_job_urls:
            response = requests.get(url)
            if response.reason != "OK":
                raise JpredStatusError(
                    "An error has occurred with a JPred job: {sr.reason}"
                )
                if "finished" in response.text.lower():
                    self.complete_job_urls.append(url)
                else:
                    updated_in_progress.append(url)
        self.inprogress_job_urls = updated_in_progress
        assert (
            len(self.inprogress_job_urls) + len(self.complete_job_urls)
        ) == self.total_sequences, (
            "Sequence JPred jobs have been lost, how did this happen!"
        )
        return len(self.complete_job_urls) == self.total_sequences

    def get_results(self) -> Optional[Dict[str, str]]:
        if len(self.complete_job_urls) == self.total_sequences:
            results_dict: Dict[str, str] = {}
            for url in self.complete_job_urls:
                job_id_match = re.search(r"(jp_.*)$", url)
                if job_id_match:
                    job_id = job_id_match.group(1)
                else:
                    raise JpredResultsError("No job id found: {url}")
                simple_results = requests.get(
                    self.results_endpoint + f"/{job_id}/{job_id}.simple.html"
                )
                (seq, pred) = BeautifulSoup(simple_results.text).code.text.split("\n")[
                    :2
                ]
                results_dict[seq] = pred
            return results_dict
        else:
            return None


# }}}
# {{{ Analyse Design


def create_metrics_from_pdb(pdb_string: str) -> DesignMetrics:

    ampal_assembly = ampal.load_pdb(pdb_string, path=False)
    # relabel everything to remove annoying insertion codes!
    ampal_assembly.relabel_all()
    if isinstance(ampal_assembly, ampal.AmpalContainer):
        ampal_assembly = ampal_assembly[0]
    if not ampal_assembly._molecules:
        raise ValueError("No PDB format data found in file.")
    design_metrics = analyse_design(ampal_assembly)
    return design_metrics


def analyse_design(design: ampal.Assembly) -> DesignMetrics:
    assert (
        EVOEF2_BINARY_PATH
    ), "EVOEF2_BINARY_PATH is not defined, check you `.env` file"
    assert (
        DFIRE2_FOLDER_PATH
    ), "DFIRE2_FOLDER_PATH is not defined, check you `.env` file"
    assert (
        ROSETTA_BINARY_PATH
    ), "ROSETTA_BINARY_PATH is not defined, check you `.env` file"
    assert (
        AGGRESCAN3D_SCRIPT_PATH
    ), "AGGRESCAN3D_SCRIPT_PATH is not defined, check you `.env` file"

    try:
        ev.tag_dssp_data(design)
        sequence_info = {
        chain.id: SequenceInfo(
            sequence="".join(m.mol_letter for m in chain),
            dssp_assignment="".join(
                m.tags["dssp_data"]["ss_definition"] for m in chain
            ),
        )
        for chain in design
        if isinstance(chain, ampal.Polypeptide)
    }
    except subprocess.CalledProcessError as se:
        logging.debug(f"Cannot compute the DSSP assignment due to a CalledProcessError:\n {se}")

        sequence_info = {
            chain.id: SequenceInfo(
                sequence="".join(m.mol_letter for m in chain),
                dssp_assignment="",
            )
            for chain in design
            if isinstance(chain, ampal.Polypeptide)
        }

    except ValueError as ve:
        logging.debug(f"Cannot compute the DSSP assignment due to a ValueError: {ve}")

        sequence_info = {
            chain.id: SequenceInfo(
                sequence="".join(m.mol_letter for m in chain),
                dssp_assignment="",
            )
            for chain in design
            if isinstance(chain, ampal.Polypeptide)
        }


    except Exception as ue:
        logging.debug(f"Cannot compute DSSP due to unknown exception: {ue}")

        sequence_info = {
            chain.id: SequenceInfo(
                sequence="".join(m.mol_letter for m in chain),
                dssp_assignment="",
            )
            for chain in design
            if isinstance(chain, ampal.Polypeptide)
        }

    full_sequence = "".join(si.sequence for si in sequence_info.values())
    dssp_assignment = "".join(
        si.dssp_assignment for si in sequence_info.values()
    ).replace(" ", "-")
    num_of_residues = len(full_sequence)
    isoelectric_point = ampal.analyse_protein.sequence_isoelectric_point(
        full_sequence.replace("X", "")
    )
    charge = ampal.analyse_protein.sequence_charge(full_sequence.replace("X", ""))
    mass = ampal.analyse_protein.sequence_molecular_weight(
        full_sequence.replace("X", "")
    )
    design_metrics = DesignMetrics(
        sequence_info=sequence_info,
        full_sequence=full_sequence,
        dssp_assignment=dssp_assignment,
        composition={
            k: v / num_of_residues for (k, v) in Counter(full_sequence).items()
        },
        torsion_angles=design_torsion_angles(design),
        hydrophobic_fitness=design_hydrophobic_fitness(design),
        isoelectric_point=isoelectric_point,
        charge=charge,
        num_of_residues=num_of_residues,
        mass=mass,
        packing_density=design_mean_packing_density(design),
        budeFF_results=run_bude_ff(design),
        evoEF2_results=run_evoef2(design.pdb, EVOEF2_BINARY_PATH),
        dfire2_results=run_dfire2(design.pdb, DFIRE2_FOLDER_PATH),
        rosetta_results=run_rosetta(design.pdb, ROSETTA_BINARY_PATH),
        aggrescan3d_results=run_aggrescan3d(design.pdb, AGGRESCAN3D_SCRIPT_PATH),
    )
    return design_metrics


# }}}
# {{{ DesignMetrics
def design_hydrophobic_fitness(design: ampal.Assembly) -> Optional[float]:
    try:
        hydrophobic_fitness = ev.calculate_hydrophobic_fitness(design)
    except ZeroDivisionError as ze:
        logging.debug(f"ZeroDivisionError when computing hydrophobic_fitness: {ze}")
        
        hydrophobic_fitness = None
    except KeyError as ke:
        logging.debug(f"KeyError when computing hydrophobic_fitness: {ke}")
        
        hydrophobic_fitness = None
    return hydrophobic_fitness


def design_mean_packing_density(design: ampal.Assembly) -> float:
    ev.tag_packing_density(design)
    mean_packing_density = np.mean(
        [a.tags["packing density"] for a in design.get_atoms() if a.element != "H"]
    )
    return mean_packing_density


def design_torsion_angles(
    design: ampal.Assembly,
) -> Dict[str, Tuple[float, float, float]]:
    
    try:
        design.tag_torsion_angles()
        torsion_angles = {}
        for residue in list(design.get_monomers()):
            if "tas" in residue.tags:
                tas = residue.tags["tas"]
                if all(tas):
                    (ch, (ic, rn, _)) = residue.unique_id
                    id_string = f"{ch}{rn}{ic}".strip()
                    torsion_angles[id_string] = tas

    except KeyError as k:
        logging.debug(f"KeyError when computing torsion angles: {k}")

        torsion_angles = {}


    except Exception as ue:
        logging.debug(f"Unknown error when computing torsion angles: {ue}")

        torsion_angles = {}
    return torsion_angles


def analyse_chain(chain: ampal.Polymer) -> Dict:
    sequence = chain_sequence(chain)
    chain_metrics = {"sequence": sequence}
    return chain_metrics


def chain_sequence(chain: ampal.Polypeptide) -> str:
    return chain.sequence


# }}}
# {{{ BUDE FF


def run_bude_ff(ampal_assembly: ampal.Assembly) -> BudeFFOutput:
    """Calculates the BUDE FF internal energy for the design."""
    try:
        budeff_score = budeff.get_internal_energy(ampal_assembly)
        budeff_output = BudeFFOutput(
            total_energy=budeff_score.total_energy,
            steric=budeff_score.steric,
            desolvation=budeff_score.desolvation,
            charge=budeff_score.charge,
        )
    except KeyError as e:
        logging.debug(f"KeyError when computing BUDE: {e}")
        # Contains an unknown atom
        budeff_output = BudeFFOutput(
            total_energy=None,
            steric=None,
            desolvation=None,
            charge=None,
        )
    return budeff_output


# }}}
# {{{ EvoEF2Output


def run_evoef2(pdb_string: str, evoef2_binary_path: str) -> EvoEF2Output:
    """Defining a function to run EvoEF2 on an input PDB file.
    EvoEF2 is an energy function that was optimised by sequence recapitulation
    and can be used to estimate protein stability. First this function runs
    EvoEF2 on the input PDB file and then the output is parsed into a
    dictionary and then converted into and EvoEF2Output object.
    Notes
    -----
    Reference: Xiaoqiang Huang, Robin Pearce, Yang Zhang. EvoEF2: accurate and
    fast energy function for computational protein design. Bioinformatics
    (2020) 36:1135-1142
    Parameters
    ----------
    pdb_file_path: str
        File path for the PDB file.
    evoef2_path: str
        File path for the EvoEF2.
    Returns
    -------
    evoef2_output: EvoEF2Output
        EvoEF2Output object which contains the log information from the
        EvoEF2 run, the energy function output and the time it took for
        EvoEF2 to run.
    """

    starting_directory = pathlib.Path.cwd()
    try:
        with tempfile.NamedTemporaryFile(mode="w") as tmp:
            # changing working dir so that EvoEF doesn't create files in the users cwd
            temp_folder = pathlib.Path(tmp.name).parent
            os.chdir(temp_folder)

            # writing the pdb string to a temp file as input for EvoEF
            tmp.write(pdb_string)

            # Creating bash command
            cmd = [
                evoef2_binary_path,
                "--command=ComputeStability",
                "--pdb=" + tmp.name,
            ]

            # Using subprocess to run this command and capturing the output
            evoef2_stdout = subprocess.run(
                cmd, capture_output=True, timeout=MAX_RUN_TIME
            )

    finally:
        # Change back to starting directory before checking return code
        os.chdir(starting_directory)


    # Creating a list of the energy value fields
    energy_field_list = [
        "reference_ALA",
        "reference_CYS",
        "reference_ASP",
        "reference_GLU",
        "reference_PHE",
        "reference_GLY",
        "reference_HIS",
        "reference_ILE",
        "reference_LYS",
        "reference_LEU",
        "reference_MET",
        "reference_ASN",
        "reference_PRO",
        "reference_GLN",
        "reference_ARG",
        "reference_SER",
        "reference_THR",
        "reference_VAL",
        "reference_TRP",
        "reference_TYR",
        "intraR_vdwatt",
        "intraR_vdwrep",
        "intraR_electr",
        "intraR_deslvP",
        "intraR_deslvH",
        "intraR_hbscbb_dis",
        "intraR_hbscbb_the",
        "intraR_hbscbb_phi",
        "aapropensity",
        "ramachandran",
        "dunbrack",
        "interS_vdwatt",
        "interS_vdwrep",
        "interS_electr",
        "interS_deslvP",
        "interS_deslvH",
        "interS_ssbond",
        "interS_hbbbbb_dis",
        "interS_hbbbbb_the",
        "interS_hbbbbb_phi",
        "interS_hbscbb_dis",
        "interS_hbscbb_the",
        "interS_hbscbb_phi",
        "interS_hbscsc_dis",
        "interS_hbscsc_the",
        "interS_hbscsc_phi",
        "interD_vdwatt",
        "interD_vdwrep",
        "interD_electr",
        "interD_deslvP",
        "interD_deslvH",
        "interD_ssbond",
        "interD_hbbbbb_dis",
        "interD_hbbbbb_the",
        "interD_hbbbbb_phi",
        "interD_hbscbb_dis",
        "interD_hbscbb_the",
        "interD_hbscbb_phi",
        "interD_hbscsc_dis",
        "interD_hbscsc_the",
        "interD_hbscsc_phi",
        "total",
        "time_spent",
    ]

    try:
        evoef2_stdout.check_returncode()

        # Splitting the result string at a substring with 92 #'s
        # then splitting the string at "Structure energy details:\n"
        # which can separate the EvoEF2 log information from the actual structure
        # energy details
        log_info, _, result_string = (
            evoef2_stdout.stdout.decode()
            .partition("#" * 92)[2]
            .partition("Structure energy details:\n")
        )

        # Finding lines with key-value pairs seperated by either ":" or "=" then creating
        # a dictionary from the pairs.
        energy_values = {
            k.strip(): convert_string_to_float(v.strip())
            for k, v in re.findall(r"(.+)[=:](.+)", result_string)
        }

        # Renaming some of the keys in the output dictionary
        energy_values["total"] = energy_values.pop("Total")
        energy_values["time_spent"] = energy_values.pop("Time spent")

    except subprocess.CalledProcessError as e:
        logging.debug(f"subprocess.CalledProcessError when computing EvoEF2: {e}")

        log_info = evoef2_stdout.stdout.decode()

        # Setting all the energy values to None
        energy_values = dict(zip(energy_field_list, [None] * len(energy_field_list)))

    except subprocess.TimeoutExpired as te:
        logging.debug(f"subprocess.TimeoutExpired when computing EvoEF2: {te}")

        log_info = evoef2_stdout.stdout.decode()

        # Setting all the energy values to None
        energy_values = dict(zip(energy_field_list, [None] * len(energy_field_list)))

    # Extracting error information and the return code
    error_info = evoef2_stdout.stderr.decode()
    return_code = evoef2_stdout.returncode

    # There should be 63 energy components
    assert len(energy_values) == 63

    # Creating an EvoEF2 object by unpacking the output dictionary
    evoef2_output = EvoEF2Output(
        log_info=log_info,
        error_info=error_info,
        return_code=return_code,
        **energy_values,
    )

    return evoef2_output


# }}}
# {{{ DFIRE2Output


def run_dfire2(pdb_string: str, dfire2_folder_path: str) -> DFIRE2Output:
    """Defining a function to run DFIRE2 on an input PDB file.
    DFIRE2 is an energy function that was optimised by sequence recapitulation
    and can be used to estimate protein stability. First this function runs
    DFIRE2 on the input PDB file and then the output is parsed into a
    dictionary and then converted into and DFIRE2Output object.
    Notes
    -----
    References:
    1. Specific interactions for ab initio folding of protein terminal regions with secondary structures.
    Proteins 72, 793-803 (2008)
    2. Ab initio folding of terminal segments with secondary structures reveals the fine difference between
    two closely-related all-atom statistical energy functions. Protein Science 17 1212-1219, (2008)
    Parameters
    ----------
    pdb_file_path: str
        File path for the PDB file.
    dfire2_folder_path: str
        Folder path for dfire2.
    Returns
    -------
    dfire2_output: DFIRE2Output
        DFIRE2Output object
    """

    starting_directory = pathlib.Path.cwd()
    try:
        with tempfile.NamedTemporaryFile(mode="w") as tmp:
            # Changing working dir so that dfire2 doesn't create files in the users cwd
            temp_folder = pathlib.Path(tmp.name).parent
            os.chdir(temp_folder)

            # Writing the pdb string to a temp file as input for dfire2
            tmp.write(pdb_string)

            # Creating bash command
            cmd = [
                dfire2_folder_path + "calene",
                dfire2_folder_path + "dfire_pair.lib",
                tmp.name,
            ]

            # Using subprocess to run this command and capturing the output
            dfire2_stdout = subprocess.run(
                cmd, capture_output=True, timeout=MAX_RUN_TIME
            )
    finally:
        # Change back to starting directory before checking return code
        os.chdir(starting_directory)

    # Setting the stdout as the log info
    log_info = dfire2_stdout.stdout.decode()

    # Extracting error information and the return code
    error_info = dfire2_stdout.stderr.decode()
    return_code = dfire2_stdout.returncode

    try:
        dfire2_stdout.check_returncode()

        # Extracting the energy value from the output
        dfire2_total_energy = convert_string_to_float(
            input_string=dfire2_stdout.stdout.decode().partition(" ")[2].strip()
        )

    except subprocess.CalledProcessError as e:
        logging.debug(f"subprocess.CalledProcessError when computing DFIRE2: {e}")

        # Setting total energy to None if there has been an error
        dfire2_total_energy = None


    except subprocess.TimeoutExpired as te:
        logging.debug(f"subprocess.TimeoutExpired when computing DFIRE2: {te}")

        # Setting total energy to None if there has been an error
        dfire2_total_energy = None

    # Creating the DFIRE2Output object
    dfire2_output = DFIRE2Output(
        log_info=log_info,
        error_info=error_info,
        return_code=return_code,
        total=dfire2_total_energy,
    )

    return dfire2_output


# }}}
# {{{ RosettaOutput


def run_rosetta(pdb_string: str, rosetta_binary_path: str) -> RosettaOutput:
    """Defining a function to run the Rosetta energy function on an input PDB file,
       parse the output file and return a RosettaOutput object.

    Notes
    -----
    Reference: Alford, R. F., Leaver-Fay, A., Jeliazkov, J. R., O’Meara, M. J., DiMaio, F. P., Park, H.,
               Shapovalov, M. V., Renfrew, P. D., Mulligan, V. K., Kappel, K., Labonte, J. W., Pacella, M. S.,
               Bonneau, R., Bradley, P., Dunbrack, R. L., Das, R., Baker, D., Kuhlman, B., Kortemme, T., & Gray,
               J. J. (2017). The Rosetta All-Atom Energy Function for Macromolecular Modeling and Design. Journal
               of Chemical Theory and Computation, 13(6), 3031–3048. https://doi.org/10.1021/acs.jctc.7b00125

    Parameters
    ----------
    pdb_file_path: str
        File path for the PDB file.
    rosetta_binary_path: str
        File path for the Rosetta energy function.

    Returns
    -------
    rosetta_output: RosettaOutput
        RosettaOutput object which contains the log and error information from the
        Rosetta run and the energy function output.
    """

    starting_directory = pathlib.Path.cwd()
    try:
        with tempfile.TemporaryDirectory() as tmp:

            # Changing working directory to the temporary folder so that Rosetta
            # doesn't create files in the users working directory
            os.chdir(tmp)

            with tempfile.NamedTemporaryFile(mode="w") as pdb:

                # Writing the pdb string to a temp file as input for Rosetta
                pdb.write(pdb_string)

                # Creating bash command
                cmd = [
                    rosetta_binary_path,
                    "-in:file:s",
                    pdb.name,
                    "-ignore_unrecognized_res",
                    "-scorefile_format json",
                ]

                # Using subprocess to run this command and capturing the output
                rosetta_stdout = subprocess.run(
                    cmd, capture_output=True, timeout=MAX_RUN_TIME
                )

                # Creating a list of the energy value fields
                energy_field_list = [
                    "dslf_fa13",
                    "fa_atr",
                    "fa_dun",
                    "fa_elec",
                    "fa_intra_rep",
                    "fa_intra_sol_xover4",
                    "fa_rep",
                    "fa_sol",
                    "hbond_bb_sc",
                    "hbond_lr_bb",
                    "hbond_sc",
                    "hbond_sr_bb",
                    "linear_chainbreak",
                    "lk_ball_wtd",
                    "omega",
                    "overlap_chainbreak",
                    "p_aa_pp",
                    "pro_close",
                    "rama_prepro",
                    "ref",
                    "score",
                    "time",
                    "total_score",
                    "yhh_planarity",
                ]

            try:
                rosetta_stdout.check_returncode()

                # Opening the json file score.sc to get the energy values
                with open("score.sc") as json_file:
                    energy_values = json.load(json_file)

                    # Removing decoy key that is not needed
                    energy_values.pop("decoy", None)

            except subprocess.CalledProcessError as e:
                logging.debug(f"subprocess.CalledProcessError when computing Rosetta: {e}")

                # Setting all the energy values to None
                energy_values = dict(
                    zip(energy_field_list, [None] * len(energy_field_list))
                )

            except subprocess.TimeoutExpired as te:
                logging.debug(f"subprocess.TimeoutExpired when computing Rosetta: {te}")

                # Setting all the energy values to None
                energy_values = dict(
                    zip(energy_field_list, [None] * len(energy_field_list))
                )
    finally:
        # Change back to starting directory
        os.chdir(starting_directory)

    # Extracting the log information
    log_info = rosetta_stdout.stdout.decode()

    # Extracting error information and the return code
    error_info = rosetta_stdout.stderr.decode()
    return_code = rosetta_stdout.returncode

    # There should be 24 energy components
    assert len(energy_values) == 24

    # Creating an RosettaOutput object by unpacking the output dictionary
    rosetta_output = RosettaOutput(
        log_info=log_info,
        error_info=error_info,
        return_code=return_code,
        **energy_values,
    )

    # Returning the output
    return rosetta_output


# }}}
# # {{{ Aggrescan3DOutput


def run_aggrescan3d(pdb_string: str, aggrescan3d_script_path: str) -> Aggrescan3DOutput:
    """Defining a function to run the aggrescan3D function on an input PDB file,
       parse the output file and return a Aggrescan3DOutput object.

    Notes
    -----
    Reference: Kuriata, A., Iglesias, V., Kurcinski, M., Ventura, S., & Kmiecik, S. (2019).
               Aggrescan3D standalone package for structure-based prediction of protein
               aggregation properties. Bioinformatics, 35(19), 3834–3835.
               https://doi.org/10.1093/bioinformatics/btz143

    Parameters
    ----------
    pdb_file_path: str
        File path for the PDB file.
    aggrescan3d_script_path: str
        Folder path for the Aggrescan3D function.

    Returns
    -------
    aggrescan3D_output: Aggrescan3DOutput
        Aggrescan3DOutput object which contains the log and error information from the
        Aggrescan3D run and the function output.
    """

    # Creating a list of the energy value fields
    aggrescan3d_field_list = [
        "protein_list",
        "chain_list",
        "residue_number_list",
        "residue_name_list",
        "residue_score_list",
        "max_value",
        "avg_value",
        "min_value",
        "total_value",
    ]

    # Creating a dictionary of all these set to None
    # which we will return if aggrescan3d returns any
    # errors
    aggrescan3d_none_dict = dict(
        zip(aggrescan3d_field_list, [None] * len(aggrescan3d_field_list))
    )

    starting_directory = pathlib.Path.cwd()
    try:
        with tempfile.TemporaryDirectory() as tmp:

            # Changing the working directory to the temporary folder so that Aggrescan3D
            # does not create files in the users working directory
            os.chdir(tmp)

            with tempfile.NamedTemporaryFile(mode="w") as pdb:

                # Writing the pdb string to a temp file as the input for Aggrescan3D
                pdb.write(pdb_string)

                # Creating bash command
                cmd = [
                    "python2",
                    aggrescan3d_script_path,
                    pdb.name,
                ]

                # Using subprocess to run this command and capturing the output
                aggrescan3D_stdout = subprocess.run(
                    cmd, capture_output=True, timeout=MAX_RUN_TIME
                )

            try:
                aggrescan3D_stdout.check_returncode()

                try:
                    assert os.path.exists("output/tmp/folded_stats") and os.path.exists(
                        "output/A3D.csv"
                    )

                    # Firstly getting the summary aggrescan3d score values
                    # from a json file
                    with open("output/tmp/folded_stats") as json_file:
                        aggrescan3d_summary = json.load(json_file)["All"]

                    # Now getting the residue level aggrescan3d score values
                    # from a csv file
                    with open("output/A3D.csv") as csv_file:

                        # Reading csv file
                        csv_reader = csv.reader(csv_file, delimiter=",")

                        # Initialising lists to capture the output
                        protein_list = []
                        chain_list = []
                        residue_number_list = []
                        residue_name_list = []
                        residue_score_list = []

                        # Looping through each row in the csv file and appending to
                        # the lists that were initialised above
                        line_count = 0
                        for row in csv_reader:
                            if line_count > 0:
                                protein_list.append(row[0])
                                chain_list.append(row[1])
                                residue_number_list.append(row[2])
                                residue_name_list.append(row[3])
                                residue_score_list.append(row[4])
                            line_count += 1

                        # Converting to floats and then back to strings.
                        # This is to ensure the values are floats but they
                        # need to be strings to be inserted into the sql table
                        residue_score_list = list(
                            map(convert_string_to_float, residue_score_list)
                        )
                        residue_score_list = list(map(str, residue_score_list))

                        # Converting these lists into strings so that they can be inputted into
                        # the sql database
                        protein_list = ";".join(protein_list)
                        chain_list = ";".join(chain_list)
                        residue_number_list = ";".join(residue_number_list)
                        residue_name_list = ";".join(residue_name_list)
                        residue_score_list = ";".join(residue_score_list)

                        # Creating a dictionary of these lists
                        aggrescan3d_residue = {
                            "protein_list": protein_list,
                            "chain_list": chain_list,
                            "residue_number_list": residue_number_list,
                            "residue_name_list": residue_name_list,
                            "residue_score_list": residue_score_list,
                        }

                        # Combining the two dictionaries
                        aggrescan3d_results = {
                            **aggrescan3d_summary,
                            **aggrescan3d_residue,
                        }

                except AssertionError as ae:
                    logging.debug(f"AssertionError when computing Aggrescan3d: {ae}")

                    # Setting all the aggrescan3d_results to None
                    aggrescan3d_results = aggrescan3d_none_dict

            except subprocess.CalledProcessError as se:
                logging.debug(f"subprocess.CalledProcessError when computing Aggrescan3d: {se}")

                # Setting all the aggrescan3d_results to None
                aggrescan3d_results = aggrescan3d_none_dict

            
            except subprocess.TimeoutExpired as te:
                logging.debug(f"subprocess.TimeoutExpired when computing Aggrescan3d: {te}")

                # Setting all the aggrescan3d_results to None
                aggrescan3d_results = aggrescan3d_none_dict
    finally:
        # Change back to starting directory
        os.chdir(starting_directory)

    # Extracting the log information
    log_info = aggrescan3D_stdout.stdout.decode()

    # Extracting error information and the return code
    error_info = aggrescan3D_stdout.stderr.decode()
    return_code = aggrescan3D_stdout.returncode

    # There should be 9 aggrescan3d_results fields
    assert len(aggrescan3d_results) == 9

    # Creating an Aggrescan3DOutput object by unpacking the output dictionary
    aggrescan3d_output = Aggrescan3DOutput(
        log_info=log_info,
        error_info=error_info,
        return_code=return_code,
        **aggrescan3d_results,
    )

    # Returning the output
    return aggrescan3d_output


# }}}
