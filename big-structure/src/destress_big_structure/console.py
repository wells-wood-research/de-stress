from datetime import datetime
import gzip as gz
import os
import multiprocessing as mp
from pathlib import Path
import random
import re
import typing as tp

import click
import bs4
import ampal

from destress_big_structure import app
from destress_big_structure.big_structure_models import (
    big_structure_engine,
    big_structure_db_session,
    BigStructureBase,
    PdbModel,
    BiolUnitModel,
)
import destress_big_structure.create_entry as create_entry

ProcPdbResult = tp.Union[tp.Tuple[str, PdbModel], tp.Tuple[str, str]]

BATCH_SIZE = 100  # files will be processed in batches of 100


def dev_run():
    app.run()


@click.command()
@click.argument("path_to_data", type=click.Path(exists=True))
@click.option(
    "--take", default=-1, help="Number of entries to process (to nearest 100)."
)
@click.option(
    "--shuffle/--no-shuffle",
    default=False,
    help=(
        "Shuffles the order of the input paths, useful with the `--take` flag "
        "to make testing databases."
    ),
)
@click.option(
    "--processes",
    default=1,
    help=("Sets the number of processes used to process structure files."),
)
@click.option(
    "--first-bio-unit-only/--all-bio-units",
    default=True,
    help=("Restricts the database to only contain the first biological unit."),
)
@click.option(
    "--pdb-list",
    type=click.Path(exists=True),
    help=(
        "A path to a file containing a list of white-space separated pdb codes. "
        "This list will be used to filter files defined in `path_to_data`."
    ),
)
def dbs_db_from_scratch(
    path_to_data: str,
    take: int,
    shuffle: bool,
    processes: int,
    first_bio_unit_only: bool,
    pdb_list: tp.Optional[str],
):
    """Creates the full database for the DeStrES Big Structure application."""

    data_dir = Path(path_to_data).resolve()
    pdb_data = data_dir / "pdb"
    assert pdb_data.exists(), f"Can't find `pdb` folder in `{data_dir}`."
    biounit_data = data_dir / "biounit"
    assert biounit_data.exists(), f"Can't find `biounit` folder in `{data_dir}`."
    xml_data = data_dir / "XML"
    assert xml_data.exists(), f"Can't find `XML` folder in `{data_dir}`."
    all_pdb_paths = list(pdb_data.glob("**/*.gz"))

    # Filter pdb files to be processed
    if pdb_list:
        with open(pdb_list, "r") as inf:
            pdb_white_list = [pdb_code.strip() for pdb_code in inf.read().split()]
        pdb_paths = [path for path in all_pdb_paths if path.name[3:7] in pdb_white_list]
        print(f"Excluded {len(all_pdb_paths)-len(pdb_paths)} pdb files.")
        print(f"Processing {len(pdb_paths)} pdb files...")
    else:
        pdb_paths = all_pdb_paths

    if shuffle:
        random.shuffle(pdb_paths)

    # Create the database tables
    BigStructureBase.metadata.create_all(bind=big_structure_engine)

    taken = 0
    failed: tp.Dict[str, str] = {}
    with mp.Pool(processes=processes) as process_pool:
        batches = [
            pdb_paths[x : x + BATCH_SIZE] for x in range(0, len(pdb_paths), BATCH_SIZE)
        ]
        for batch_number, path_batch in enumerate(batches):
            print(f"Processing batch {batch_number+1}/{len(batches)}...")
            batch_results = process_pool.map(
                process_pdb,
                [
                    (pdb_path, biounit_data, xml_data, first_bio_unit_only)
                    for pdb_path in path_batch
                ],
            )
            pdb_models = []
            for result in batch_results:
                if isinstance(result[1], PdbModel):
                    pdb_models.append(result)
                else:
                    failed[result[0]] = result[1]
            big_structure_db_session.add_all(pdb_model[1] for pdb_model in pdb_models)
            big_structure_db_session.commit()
            for added_path, _ in pdb_models:
                taken += 1
                print(f"Added {added_path}.")
            if taken == take:
                break
            print(f"Finished processing batch {batch_number+1}/{len(batches)}")
    for (k, v) in failed.items():
        print(f"The following files failed to run:")
        print(f"---- {k} ----\n{v}")
    print(f"Added {taken} files to database.")
    print("Exiting.")


def process_pdb(input_arguments: tp.Tuple[Path, Path, Path, bool]) -> ProcPdbResult:
    pdb_path, biounit_data, xml_data, first_bio_unit_only = input_arguments
    try:
        print(f"\tProcessing {pdb_path}...")
        pdb_code = pdb_path.name[3:7]
        if first_bio_unit_only:
            biounit_paths = sorted(
                list((biounit_data / pdb_code[1:3]).glob(f"{pdb_code}.pdb*.gz"))
            )[:1]
        else:
            biounit_paths = list(
                (biounit_data / pdb_code[1:3]).glob(f"{pdb_code}.pdb*.gz")
            )

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
        print(f"\tFinished processing {pdb_path}")
        return (str(pdb_path), pdb_model)
    except Exception as e:
        return (str(pdb_path), str(e))


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
        print(f"\t\tProcessing {path}...")
        biounit_number_search = re.search(r"pdb(\d+)\.gz$", str(path))
        if biounit_number_search:
            biounit_number = int(biounit_number_search.group(1))
            assert biounit_number > 0, (
                f"Biological unit number is expected to be a positive "
                f"integer but I got `{biounit_number}` for `{path}`."
            )
            print(f"\t\tFinished processing {path}")
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
