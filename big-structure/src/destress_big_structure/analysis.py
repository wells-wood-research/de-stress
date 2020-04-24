"""Contains function for running the analytics sweet."""
from collections import Counter
from typing import Dict, Optional

import ampal
import isambard
import isambard.evaluation as ev
import numpy as np


def analyse_state(state: ampal.Assembly) -> Dict:
    sequences = [
        chain.sequence for chain in state if isinstance(chain, ampal.Polypeptide)
    ]
    full_sequence = "".join(sequences)
    num_of_residues = len(full_sequence)
    isoelectric_point = ampal.analyse_protein.sequence_isoelectric_point(
        full_sequence.replace("X", "")
    )
    mass = ampal.analyse_protein.sequence_molecular_weight(
        full_sequence.replace("X", "")
    )
    state_analytics = {
        "composition": ";".join(
            f"{k}:{v/num_of_residues:.2f}" for (k, v) in Counter(full_sequence).items()
        ),
        "torsion_angles": state_torsion_angles(state),
        "hydrophobic_fitness": state_hydrophobic_fitness(state),
        "is_protein_only": state_is_protein_only(state),
        "isoelectric_point": isoelectric_point,
        "num_of_residues": num_of_residues,
        "mass": mass,
        "mean_packing_density": state_mean_packing_density(state),
    }
    return state_analytics


def state_hydrophobic_fitness(state: ampal.Assembly) -> Optional[float]:
    try:
        hydrophobic_fitness = ev.calculate_hydrophobic_fitness(state)
    except ZeroDivisionError:
        hydrophobic_fitness = None
    return hydrophobic_fitness


def state_is_protein_only(state: ampal.Assembly) -> bool:
    return all([isinstance(chain, ampal.Polypeptide) for chain in state])


def state_mean_packing_density(state: ampal.Assembly) -> float:
    ev.tag_packing_density(state)
    mean_packing_density = np.mean(
        [a.tags["packing density"] for a in state.get_atoms() if a.element != "H"]
    )
    return mean_packing_density


def state_torsion_angles(state: ampal.Assembly) -> str:
    state.tag_torsion_angles()
    torsion_angle_strings = []
    for residue in list(state.get_monomers()):
        if "tas" in residue.tags:
            tas = residue.tags["tas"]
            if all(tas):
                om, ph, ps = tas
                (ch, (ic, rn, _)) = residue.unique_id
                id_string = f"{ch}{rn}{ic}".strip()
                torsion_angle_strings.append(f"{id_string}({om:.0f},{ph:.0f},{ps:.0f})")
    return "".join(torsion_angle_strings)


def analyse_chain(chain: ampal.Polymer) -> Dict:
    sequence = chain_sequence(chain)
    chain_analytics = {"sequence": sequence}
    return chain_analytics


def chain_sequence(chain: ampal.Polypeptide) -> str:
    return chain.sequence
