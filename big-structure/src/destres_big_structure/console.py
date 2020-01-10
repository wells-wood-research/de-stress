from datetime import datetime
import gzip as gz
from pathlib import Path
import random
import re
import typing as tp

import click
import bs4
import ampal

from destres_big_structure import app
from destres_big_structure.big_structure_models import (
    big_structure_engine,
    big_structure_db_session,
    BigStructureBase,
    PdbModel,
    BiolUnitModel,
)
from destres_big_structure.design_models import designs_engine, DesignsBase
import destres_big_structure.create_entry as create_entry


def dev_run():
    app.run()


def create_designs_tables():
    DesignsBase.metadata.create_all(bind=designs_engine)


@click.command()
@click.argument("path_to_data", type=click.Path(exists=True))
@click.option("--take", default=-1, help="Number of entries to process.")
@click.option(
    "--shuffle/--no-shuffle",
    default=False,
    help=(
        "Shuffles the order of the input paths, useful with the `--take` flag "
        "to make testing databases."
    ),
)
def dbs_db_from_scratch(path_to_data: str, take: int, shuffle: bool):
    """Creates the full database for the DeStrES Big Structure application."""
    data_dir = Path(path_to_data)
    pdb_data = data_dir / "pdb"
    assert pdb_data.exists(), f"Can't find `pdb` folder in `{data_dir}`."
    biounit_data = data_dir / "biounit"
    assert biounit_data.exists(), f"Can't find `biounit` folder in `{data_dir}`."
    xml_data = data_dir / "XML"
    assert xml_data.exists(), f"Can't find `XML` folder in `{data_dir}`."

    pdb_paths = list(pdb_data.glob("**/*.gz"))
    if shuffle:
        random.shuffle(pdb_paths)

    # Create the database tables
    BigStructureBase.metadata.create_all(bind=big_structure_engine)

    taken = 0
    failed: tp.Dict[str, str] = {}
    for pdb_path in pdb_paths:
        try:
            pdb_model = process_pdb(pdb_path, biounit_data, xml_data)
        except AssertionError as e:
            failed[str(pdb_path)] = str(e)
            continue
        big_structure_db_session.add_all([pdb_model])
        big_structure_db_session.commit()
        print(f"Added {pdb_path}.")
        taken += 1
        if taken == take:
            break
    for (k, v) in failed.items():
        print(f"----\n{k}\n{v}\n")


def process_pdb(pdb_path: Path, biounit_data: Path, xml_data: Path) -> PdbModel:
    pdb_code = pdb_path.name[3:7]
    biounit_paths = list((biounit_data / pdb_code[1:3]).glob(f"{pdb_code}.pdb*.gz"))
    xml_path = xml_data / pdb_code[1:3] / f"{pdb_code}-noatom.xml.gz"
    assert biounit_paths, f"No biological units found for {pdb_code}."
    assert xml_path.exists(), f"No PDBML file found for {pdb_code}."
    pdb_information = get_pdb_information(xml_path)
    pdb_model = PdbModel(
        pdb_code=pdb_code,
        deposition_date=datetime.strptime(
            pdb_information["deposition_date"], "%Y-%m-%d"
        ).date(),
        method=pdb_information["method"],
    )
    _ = process_biounits(pdb_path, biounit_paths, pdb_model)
    return pdb_model


def get_pdb_information(xml_path: Path) -> tp.Dict[str, str]:
    with gz.open(str(xml_path)) as inf:
        parsed_xml = bs4.BeautifulSoup(inf.read(), "xml")
    pdb_information = {
        "deposition_date": parsed_xml.find_all(
            "PDBx:recvd_initial_deposition_date", limit=1
        )[0].get_text(),
        "method": parsed_xml.find_all("PDBx:exptl")[0]["method"],
    }
    return pdb_information


def process_biounits(
    pdb_path: Path, biounit_paths: tp.List[Path], pdb_model: PdbModel
) -> tp.List[BiolUnitModel]:
    deposition_structure = create_entry.create_biounit_entry(
        pdb_path, 0, pdb_model, is_deposited_pdb=True, preferred_biol_unit=None
    )
    biounits = [deposition_structure]
    for path in biounit_paths:
        biounit_number_search = re.search(r"pdb(\d+)\.gz$", str(path))
        if biounit_number_search:
            biounit_number = int(biounit_number_search.group(1))
            assert biounit_number > 0, (
                f"Biological unit number is expected to be a positive "
                f"integer but I got `{biounit_number}` for `{path}`."
            )
        else:
            raise ValueError(
                f"Expected biological unit path to have the form "
                f"[pdb_code].pdb[biounit_number].gz, but I got `{path}`"
            )
        biounits.append(
            create_entry.create_biounit_entry(
                path,
                biounit_number,
                pdb_model,
                is_deposited_pdb=False,
                preferred_biol_unit=1,
            )
        )
    return biounits
