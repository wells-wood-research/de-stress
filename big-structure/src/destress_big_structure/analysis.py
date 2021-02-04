"""Contains function for running the analytics sweet."""
from collections import Counter
from typing import Dict, List, Optional, Tuple, Set
import os
import pathlib
import subprocess
import tempfile
import re

from bs4 import BeautifulSoup
import ampal
import isambard.evaluation as ev
import numpy as np
import requests

from .elm_types import DesignMetrics, EvoEF2Output, SequenceInfo
from destress_big_structure.settings import EVOEF2_BINARY_PATH

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
# {{{ DesignMetrics


def create_metrics_from_pdb(pdb_string: str) -> DesignMetrics:
    ampal_assembly = ampal.load_pdb(pdb_string, path=False)
    if isinstance(ampal_assembly, ampal.AmpalContainer):
        ampal_assembly = ampal_assembly[0]
    if not ampal_assembly._molecules:
        raise ValueError("No PDB format data found in file.")
    design_metrics = analyse_design(ampal_assembly)
    return design_metrics


def analyse_design(design: ampal.Assembly) -> DesignMetrics:
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
    full_sequence = "".join(si.sequence for si in sequence_info.values())
    num_of_residues = len(full_sequence)
    isoelectric_point = ampal.analyse_protein.sequence_isoelectric_point(
        full_sequence.replace("X", "")
    )
    mass = ampal.analyse_protein.sequence_molecular_weight(
        full_sequence.replace("X", "")
    )
    design_metrics = DesignMetrics(
        sequence_info=sequence_info,
        composition={
            k: v / num_of_residues for (k, v) in Counter(full_sequence).items()
        },
        torsion_angles=design_torsion_angles(design),
        hydrophobic_fitness=design_hydrophobic_fitness(design),
        isoelectric_point=isoelectric_point,
        num_of_residues=num_of_residues,
        mass=mass,
        packing_density=design_mean_packing_density(design),
        evoEF2_results=run_evoef2(design.pdb, EVOEF2_BINARY_PATH),
    )
    return design_metrics


def design_hydrophobic_fitness(design: ampal.Assembly) -> Optional[float]:
    try:
        hydrophobic_fitness = ev.calculate_hydrophobic_fitness(design)
    except ZeroDivisionError:
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
    design.tag_torsion_angles()
    torsion_angles = {}
    for residue in list(design.get_monomers()):
        if "tas" in residue.tags:
            tas = residue.tags["tas"]
            if all(tas):
                (ch, (ic, rn, _)) = residue.unique_id
                id_string = f"{ch}{rn}{ic}".strip()
                torsion_angles[id_string] = tas
    return torsion_angles


def analyse_chain(chain: ampal.Polymer) -> Dict:
    sequence = chain_sequence(chain)
    chain_metrics = {"sequence": sequence}
    return chain_metrics


def chain_sequence(chain: ampal.Polypeptide) -> str:
    return chain.sequence


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
    with tempfile.NamedTemporaryFile(mode="w") as tmp:
        # changing working dir so that EvoEF doesn't create files in the users cwd
        temp_folder = pathlib.Path(tmp.name).parent
        os.chdir(temp_folder)

        # writing the pdb string to a temp file as input for EvoEF
        tmp.write(pdb_string)

        # Creating bash command
        cmd = [evoef2_binary_path, "--command=ComputeStability", "--pdb=" + tmp.name]

        # Using subprocess to run this command and capturing the output
        evoef2_stdout = subprocess.run(cmd, capture_output=True)

        # Change back to starting directory before checking return code
        os.chdir(starting_directory)
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
        k.strip(): float(v.strip())
        for k, v in re.findall(r"(.+)[=:](.+)", result_string)
    }

    # There should be 63 energy components
    assert len(energy_values) == 63

    # Renaming some of the keys in the output dictionary
    energy_values["total"] = energy_values.pop("Total")
    energy_values["time_spent"] = energy_values.pop("Time spent")

    # Creating an EvoEF2 object by unpacking the output dictionary
    evoef2_output = EvoEF2Output(log_info=log_info, **energy_values)

    return evoef2_output


# }}}
