"""Contains function for running the analytics sweet."""
from collections import Counter
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple
import re

from bs4 import BeautifulSoup
from dataclasses_json import dataclass_json, LetterCase
import ampal
import isambard
import isambard.evaluation as ev
import numpy as np
import requests

from .elm_types import DesignMetrics, SequenceInfo


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
        self.unique_sequences = set(sequences)
        self.total_sequences = len(list(self.unique_sequences))
        self.submission_reponses = [
            requests.post(
                f"{self.rest_endpoint}/job",
                data=query.encode("utf-8"),
                headers={"Content-type": "text/txt"},
            )
            for query in self.make_queries()
        ]
        self.inprogress_job_urls: List[str] = []
        for sr in self.submission_reponses:
            if sr.status_code == 202:
                self.inprogress_job_urls.append(sr.headers["Location"])
            else:
                raise JpredSubmissionError(
                    "Failed to submit job to JPred, error code: {sr.stastatus_code}"
                )
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

