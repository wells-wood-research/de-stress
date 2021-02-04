import gzip as gz
from pathlib import Path
import typing as tp

import ampal

from destress_big_structure.big_structure_models import (
    PdbModel,
    BiolUnitModel,
    StateModel,
    ChainModel,
    EvoEF2ResultsModel,
)
from destress_big_structure.design_models import (
    DesignModel,
    DesignChainModel,
)
from destress_big_structure import analysis

from .settings import EVOEF2_BINARY_PATH


def create_biounit_entry(
    pdb_path: Path,
    biounit_num: int,
    pdb_entry: PdbModel,
    is_deposited_pdb: bool,
    preferred_biol_unit: tp.Optional[int],
) -> BiolUnitModel:
    with gz.open(str(pdb_path)) as inf:
        contents = inf.read().decode()
    pdb_ampal = ampal.load_pdb(contents, pdb_id=pdb_path.name, path=False)
    is_preferred_biol_unit = (
        False if preferred_biol_unit is None else biounit_num == preferred_biol_unit
    )
    biounit_model = BiolUnitModel(
        biol_unit_number=biounit_num,
        is_deposited_pdb=is_deposited_pdb,
        is_preferred_biol_unit=is_preferred_biol_unit,
        pdb=pdb_entry,
    )
    if isinstance(pdb_ampal, ampal.Assembly):
        states = [create_state_entry(pdb_ampal, 0, biounit_model)]
    else:
        states = []
        for i, state in enumerate(pdb_ampal):
            states.append(create_state_entry(state, i, biounit_model))
    return biounit_model


def create_state_entry(
    ampal_assembly: ampal.Assembly, state_number: int, biounit_entry: BiolUnitModel
) -> StateModel:
    # Generate raw metrics
    state_analytics = analysis.analyse_design(ampal_assembly)
    # Convert the DesignMetrics into a StateModel
    state_model = StateModel(
        state_number=state_number,
        biol_unit=biounit_entry,
        composition=";".join(
            f"{k}:{v:.2f}" for (k, v) in state_analytics.composition.items()
        ),
        torsion_angles="".join(
            f"{id_string}({tas[0]:.0f},{tas[1]:.0f},{tas[2]:.0f})"
            for id_string, tas in state_analytics.torsion_angles.items()
        ),
        hydrophobic_fitness=state_analytics.hydrophobic_fitness,
        is_protein_only=all(
            [isinstance(chain, ampal.Polypeptide) for chain in ampal_assembly]
        ),
        isoelectric_point=state_analytics.isoelectric_point,
        num_of_residues=state_analytics.num_of_residues,
        mass=state_analytics.mass,
        mean_packing_density=state_analytics.packing_density,
    )
    for chain in ampal_assembly:
        if isinstance(chain, ampal.Polypeptide):
            create_chain_entry(chain, state_model)

    create_evoef2_results_entry(ampal_assembly, state_model, EVOEF2_BINARY_PATH)

    return state_model


def create_chain_entry(chain: ampal.Polypeptide, state_model: StateModel) -> ChainModel:
    chain_analytics = analysis.analyse_chain(chain)
    chain_model = ChainModel(chain_label=chain.id, state=state_model, **chain_analytics)
    return chain_model


def create_evoef2_results_entry(
    ampal_assembly: ampal.Assembly, state_model: StateModel, evoef2_binary_path: str
) -> EvoEF2ResultsModel:
    evoef2_results = analysis.run_evoef2(ampal_assembly.pdb, evoef2_binary_path)
    evoef2_results_model = EvoEF2ResultsModel(
        state=state_model, **evoef2_results.__dict__
    )

    return evoef2_results_model
