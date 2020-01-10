import gzip as gz
from pathlib import Path
import typing as tp

import ampal

from destres_big_structure.big_structure_models import (
    PdbModel,
    BiolUnitModel,
    StateModel,
    ChainModel,
)
from destres_big_structure.design_models import (
    DesignModel,
    DesignChainModel,
)
from destres_big_structure import analysis


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
    state_analytics = analysis.analyse_state(ampal_assembly)
    state_model = StateModel(
        state_number=state_number, biol_unit=biounit_entry, **state_analytics
    )
    for chain in ampal_assembly:
        if isinstance(chain, ampal.Polypeptide):
            create_chain_entry(chain, state_model)
    return state_model


def create_chain_entry(chain: ampal.Polypeptide, state_model: StateModel) -> ChainModel:
    chain_analytics = analysis.analyse_chain(chain)
    chain_model = ChainModel(chain_label=chain.id, state=state_model, **chain_analytics)
    return chain_model


def create_design_entry(ampal_assembly: ampal.Assembly) -> DesignModel:
    design_analytics = analysis.analyse_state(ampal_assembly)
    design_model = DesignModel(**design_analytics)
    for chain in ampal_assembly:
        if isinstance(chain, ampal.Polypeptide):
            create_design_chain_entry(chain, design_model)
    return design_model


def create_design_chain_entry(
    chain: ampal.Polypeptide, design_model: DesignModel
) -> ChainModel:
    chain_analytics = analysis.analyse_chain(chain)
    chain_model = DesignChainModel(
        chain_label=chain.id, design=design_model, **chain_analytics
    )
    return chain_model
