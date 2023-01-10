from dataclasses import dataclass
from datetime import datetime
import gzip as gz
import os
import time
import multiprocessing as mp
from pathlib import Path
import random
import re
import typing as tp
import csv
import math
import click
import bs4
import ampal
import logging

from destress_big_structure import app
from destress_big_structure.big_structure_models import (
    big_structure_engine,
    big_structure_db_session,
    BigStructureBase,
    PdbModel,
    BiolUnitModel,
)
from typing import Dict
from destress_big_structure.elm_types import (
    DesignMetrics,
)
from destress_big_structure import analysis
import destress_big_structure.create_entry as create_entry
from .elm_types import DesignMetricsOutputRow

ProcPdbResult = tp.Union[tp.Tuple[str, PdbModel], tp.Tuple[str, str]]

BATCH_SIZE = 1000  # files will be processed in batches of 1000

from destress_big_structure.settings import (
    HEADLESS_DESTRESS_WORKERS,
    HEADLESS_DESTRESS_BATCH_SIZE,
)


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
            pdb_white_list = [
                pdb_code.strip().lower() for pdb_code in inf.read().split()
            ]
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


# Defining a function to unpack the composition metrics results
# and change the one letter amino acid codes to three letter
# amino acid codes
def unpacking_comp_metrics(design_metrics: DesignMetrics) -> Dict:

    # Defining a dict to convert between the
    # one letter and three letter aa codes
    aa_dict = {
        "ALA": "A",
        "CYS": "C",
        "ASP": "D",
        "GLU": "E",
        "PHE": "F",
        "GLY": "G",
        "HIS": "H",
        "ILE": "I",
        "LYS": "K",
        "LEU": "L",
        "MET": "M",
        "ASN": "N",
        "PRO": "P",
        "GLN": "Q",
        "ARG": "R",
        "SER": "S",
        "THR": "T",
        "VAL": "V",
        "TRP": "W",
        "UNK": "X",
        "TYR": "Y",
    }

    # Setting all the comp metrics to 0
    comp_metrics = dict(zip(aa_dict.keys(), [0] * len(aa_dict.keys())))

    # Selecting the aa one letter keys from the design metrics for this PDB
    design_aa_one_letter_list = design_metrics.composition.keys()

    # Listing out the aa three and one letter codes from aa dict
    aa_three_letter_list = list(aa_dict.keys())
    aa_one_letter_list = list(aa_dict.values())

    # Now for each of the one letter codes from the design metrics, we
    # update the composition metric values
    for aa_one_letter_code in design_aa_one_letter_list:

        # Finding position of this one letter code in the list from the dictionary
        position = aa_one_letter_list.index(aa_one_letter_code)
        aa_three_letter_code = aa_three_letter_list[position]

        # Updating the comp metrics dictionary
        comp_metrics[aa_three_letter_code] = design_metrics.composition[
            aa_one_letter_code
        ]

    return comp_metrics


def headless_destress(pdb_file: str) -> DesignMetricsOutputRow:

    """Running DE-STRESS in headless mode (using CLI rather than
    DE-STRESS user interface) for a single pdb file.

    Defining a function to run the analysis.create_metrics_from_pdb()
    on a single pdb file. Firstly the pdb file is converted to an
    ampal assembly object and only the ATOM residues are kept (some
    of the other residues can cause issues for some of the DE-STRESS
    metrics). A list of the DE-STRESS metrics is returned as data_row
    from this function.

    Parameters
    ----------
    pdb_file: str
        This is the input pdb file.

    Returns
    -------
    design_metrics_output_row: DesignMetricsOutputRow
        This is the DE-STRESS metrics that have been calculated
        for the input pdb file.

    """

    # Extracting the design name for the pdb file and the file name
    design_name = os.path.splitext(os.path.basename(pdb_file))[0]
    file_name = pdb_file

    # Firstly defining a list of fields in the output
    design_field_list = [
        "composition_ALA",
        "composition_CYS",
        "composition_ASP",
        "composition_GLU",
        "composition_PHE",
        "composition_GLY",
        "composition_HIS",
        "composition_ILE",
        "composition_LYS",
        "composition_LEU",
        "composition_MET",
        "composition_ASN",
        "composition_PRO",
        "composition_GLN",
        "composition_ARG",
        "composition_SER",
        "composition_THR",
        "composition_VAL",
        "composition_TRP",
        "composition_UNK",
        "composition_TYR",
        "hydrophobic_fitness",
        "isoelectric_point",
        "mass",
        "num_residues",
        "packing_density",
        "budeff_total",
        "budeff_steric",
        "budeff_desolvation",
        "budeff_charge",
        "evoef2_total",
        "evoef2_ref_total",
        "evoef2_intraR_total",
        "evoef2_interS_total",
        "evoef2_interD_total",
        "dfire2_total",
        "rosetta_total",
        "rosetta_fa_atr",
        "rosetta_fa_rep",
        "rosetta_fa_intra_rep",
        "rosetta_fa_elec",
        "rosetta_fa_sol",
        "rosetta_lk_ball_wtd",
        "rosetta_fa_intra_sol_xover4",
        "rosetta_hbond_lr_bb",
        "rosetta_hbond_sr_bb",
        "rosetta_hbond_bb_sc",
        "rosetta_hbond_sc",
        "rosetta_dslf_fa13",
        "rosetta_rama_prepro",
        "rosetta_p_aa_pp",
        "rosetta_fa_dun",
        "rosetta_omega",
        "rosetta_pro_close",
        "rosetta_yhh_planarity",
        "aggrescan3d_total_value",
        "aggrescan3d_avg_value",
        "aggrescan3d_min_value",
        "aggrescan3d_max_value",
    ]

    # Loading in the PDB file and converting it to an ampal assembly
    try:
        ampal_assembly = ampal.load_pdb(str(pdb_file), path=True)

    except ValueError as e:
        logging.debug(f"PDB file could not be loaded due to a ValueError:\n {e}")
        ampal_assembly = None

    if ampal_assembly is None:

        # Setting all the design metrics to None
        design_metrics_output = dict(
            zip(design_field_list, [None] * len(design_field_list))
        )

        # Creating the design metrics output row
        design_metrics_output_row = DesignMetricsOutputRow(
            design_name=design_name,
            file_name=file_name,
            **design_metrics_output,
        )
    else:

        # Only selecting ATOM residues and removing the other residues.
        # This is because some of these other residues can cause issues
        # for the DE-STRESS metric calculations.
        pdb_lines = ampal_assembly.pdb.splitlines()
        pdb_lines_filtered = [line for line in pdb_lines if line.startswith("ATOM")]
        pdb_string_filtered = "\n".join(pdb_lines_filtered)
        num_atom_records_removed = len(pdb_lines) - len(pdb_lines_filtered)

        logging.warning(
            f"{num_atom_records_removed} non ATOM records removed from the PDB file {file_name}."
        )

        try:

            # Running the DE-STRESS metrics for the pdb file
            design_metrics = analysis.create_metrics_from_pdb(pdb_string_filtered)

            # Unpacking the compisition metrics
            comp_metrics = unpacking_comp_metrics(design_metrics)

            # Creating a dictionary of all the design metrics
            design_metrics_output = dict(
                zip(
                    design_field_list,
                    [
                        comp_metrics["ALA"],
                        comp_metrics["CYS"],
                        comp_metrics["ASP"],
                        comp_metrics["GLU"],
                        comp_metrics["PHE"],
                        comp_metrics["GLY"],
                        comp_metrics["HIS"],
                        comp_metrics["ILE"],
                        comp_metrics["LYS"],
                        comp_metrics["LEU"],
                        comp_metrics["MET"],
                        comp_metrics["ASN"],
                        comp_metrics["PRO"],
                        comp_metrics["GLN"],
                        comp_metrics["ARG"],
                        comp_metrics["SER"],
                        comp_metrics["THR"],
                        comp_metrics["VAL"],
                        comp_metrics["TRP"],
                        comp_metrics["UNK"],
                        comp_metrics["TYR"],
                        design_metrics.hydrophobic_fitness,
                        design_metrics.isoelectric_point,
                        design_metrics.mass,
                        design_metrics.num_of_residues,
                        design_metrics.packing_density,
                        design_metrics.budeFF_results.total_energy,
                        design_metrics.budeFF_results.steric,
                        design_metrics.budeFF_results.desolvation,
                        design_metrics.budeFF_results.charge,
                        design_metrics.evoEF2_results.total,
                        design_metrics.evoEF2_results.ref_total,
                        design_metrics.evoEF2_results.intraR_total,
                        design_metrics.evoEF2_results.interS_total,
                        design_metrics.evoEF2_results.interD_total,
                        design_metrics.dfire2_results.total,
                        design_metrics.rosetta_results.total_score,
                        design_metrics.rosetta_results.fa_atr,
                        design_metrics.rosetta_results.fa_rep,
                        design_metrics.rosetta_results.fa_intra_rep,
                        design_metrics.rosetta_results.fa_elec,
                        design_metrics.rosetta_results.fa_sol,
                        design_metrics.rosetta_results.lk_ball_wtd,
                        design_metrics.rosetta_results.fa_intra_sol_xover4,
                        design_metrics.rosetta_results.hbond_lr_bb,
                        design_metrics.rosetta_results.hbond_sr_bb,
                        design_metrics.rosetta_results.hbond_bb_sc,
                        design_metrics.rosetta_results.hbond_sc,
                        design_metrics.rosetta_results.dslf_fa13,
                        design_metrics.rosetta_results.rama_prepro,
                        design_metrics.rosetta_results.p_aa_pp,
                        design_metrics.rosetta_results.fa_dun,
                        design_metrics.rosetta_results.omega,
                        design_metrics.rosetta_results.pro_close,
                        design_metrics.rosetta_results.yhh_planarity,
                        design_metrics.aggrescan3d_results.total_value,
                        design_metrics.aggrescan3d_results.avg_value,
                        design_metrics.aggrescan3d_results.min_value,
                        design_metrics.aggrescan3d_results.max_value,
                    ],
                )
            )

            # Creating the design metrics output row
            design_metrics_output_row = DesignMetricsOutputRow(
                design_name=design_name,
                file_name=file_name,
                **design_metrics_output,
            )

        except Exception as e:
            logging.debug(
                f"Error encountered when calculating the DE-STRESS metrics for PDB file {file_name}. Therefore, this file will have None values for all the metrics. :\n {e}"
            )

            # Setting all the design metrics to None
            design_metrics_output = dict(
                zip(design_field_list, [None] * len(design_field_list))
            )

            # Creating the design metrics output row
            design_metrics_output_row = DesignMetricsOutputRow(
                design_name=design_name,
                file_name=file_name,
                **design_metrics_output,
            )

    return design_metrics_output_row


@click.command()
@click.argument("input_path", type=click.Path(exists=True))
def headless_destress_batch(input_path: str) -> None:
    """Running DE-STRESS in headless mode (using CLI rather than
    DE-STRESS user interface) for a set of pdb files.

    Defining a function to run the function headless_destress()
    on a set of pdb files. This function uses multiprocessing and
    splits the set of pdb files into batches which are run one
    by one. Two parameters are used in this function:
    HEADLESS_DESTRESS_WORKERS which is the number of workers to use
    for the multiprocessing and HEADLESS_DESTRESS_BATCH_SIZE
    which is the number of pdb files to have in each batch. Both of
    these can be set by the user in the .env-headless file. All
    the data_row lists are collected from the results of
    run_headless_destress() function and outputted as a csv file
    in the input path directory. This csv file is created for the
    first batch and then the results from the other batches are
    inserted into the same csv file.

    Parameters
    ----------
    input_path: str
        This is the input path to a set of pdb files to be ran through
        headless DE-STRESS.

    Returns
    -------
    None
    """

    # Start time
    tic = time.time()

    # Resolving the input path that has been provided
    input_path = Path(input_path).resolve()

    # Changing directory to the input path
    os.chdir(input_path)

    logging.basicConfig(filename="logging.txt", level=logging.DEBUG, filemode="w")

    # Getting a list of all the pdb files in the input path
    pdb_file_list = list(input_path.glob("*.pdb"))
    pdb_file_list = pdb_file_list + list(input_path.glob("*.ent"))

    # Checking that the list of PDB files is not empty.
    assert pdb_file_list, logging.debug("There are no PDB files in the input path.")

    # Checking if HEADLESS_DESTRESS_WORKERS has
    # been specified in the .env-headless file
    assert HEADLESS_DESTRESS_WORKERS, logging.debug(
        "The number of HEADLESS_DESTRESS_WORKERS is not set in a .env file."
    )

    # Checking if HEADLESS_DESTRESS_BATCH_SIZE has
    # been specified in the .env-headless file
    assert HEADLESS_DESTRESS_BATCH_SIZE, logging.debug(
        "The number of HEADLESS_DESTRESS_BATCH_SIZE is not set in a .env file."
    )

    # Converting from strings to integers.
    NUM_HEADLESS_DESTRESS_WORKERS = int(HEADLESS_DESTRESS_WORKERS)
    NUM_HEADLESS_DESTRESS_BATCH_SIZE = int(HEADLESS_DESTRESS_BATCH_SIZE)

    # Calculating the total number of PDB files in the list
    num_pdb_files = len(pdb_file_list)

    # Printing info to the user
    print(
        "Hello user :) I hope you're having a great day! Just to let you know that headless DE-STRESS will run on "
        + str(num_pdb_files)
        + " PDB files in "
        + str(int(math.ceil(num_pdb_files / NUM_HEADLESS_DESTRESS_BATCH_SIZE)))
        + " batches."
    )

    logging.info(
        "DE-STRESS will run on "
        + str(num_pdb_files)
        + " PDB files in "
        + str(int(math.ceil(num_pdb_files / NUM_HEADLESS_DESTRESS_BATCH_SIZE)))
        + " batches."
    )

    print(
        "The estimated run time with >= 20 cores will be roughly "
        + str(round(num_pdb_files / 60))
        + " minutes. So relax, get a coffee and the results will be ready for you soon!"
    )

    # Initialising the mprocess pool and number of workers
    with mp.Pool(processes=NUM_HEADLESS_DESTRESS_WORKERS) as process_pool:

        # Splitting the PDB file list up into batches based on the
        # HEADLESS_DESTRESS_BATCH_SIZE set by the user
        batches = [
            pdb_file_list[x : x + NUM_HEADLESS_DESTRESS_BATCH_SIZE]
            for x in range(0, len(pdb_file_list), NUM_HEADLESS_DESTRESS_BATCH_SIZE)
        ]

        # Looping over the different batches
        for batch_number, batch_file_list in enumerate(batches):

            # Printing the batch number to the user
            print(f"Processing batch {batch_number+1}/{len(batches)}...")

            logging.info(f"Processing batch {batch_number+1}/{len(batches)}...")

            # Applying process pool to the batch of PDB files
            batch_results = process_pool.map(headless_destress, batch_file_list)

            # If this is the first batch then it creates the csv file
            # but for all other batches it inserts into the csv file
            if batch_number == 0:

                # Extracting the dictionary keys as headers for the CSV file
                headers = batch_results[0].__dict__.keys()

                # Opening in "write" mode
                with open("design_data.csv", "w", encoding="UTF8") as f:
                    writer = csv.writer(f)

                    # Writing the header to the csv file
                    writer.writerow(headers)

                    # Looping through the data rows and
                    # writing them into the data set
                    for i in range(0, len(batch_results)):
                        writer.writerow(batch_results[i].__dict__.values())

            else:

                # Opening in "append" mode
                with open("design_data.csv", "a", encoding="UTF8") as f:
                    writer = csv.writer(f)

                    # Looping through the data rows and
                    # writing them into the data set
                    for i in range(0, len(batch_results)):
                        writer.writerow(batch_results[i].__dict__.values())

    # End time
    toc = time.time()

    # Printing the total time it took for the script to run in minutes.
    print("Completed in {:.2f} minutes.".format((toc - tic) / 60))

    logging.info("Completed in {:.2f} minutes.".format((toc - tic) / 60))
