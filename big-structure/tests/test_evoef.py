import numpy as np
import json
import pathlib
import typing as t
import graphene
import re

from destress_big_structure.analysis import run_evoef2
from destress_big_structure.settings import EVOEF2_BINARY_PATH
from destress_big_structure.elm_types import EvoEF2Output
from destress_big_structure.schema import Query
from destress_big_structure.big_structure_models import big_structure_db_session


def test_check_run_evoef_executes():
    test_path = pathlib.Path("/app/tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    evoef_results = run_evoef2(
        pdb_string=pdb_string, evoef2_binary_path=EVOEF2_BINARY_PATH
    )

    # Testing the results encoding
    results_json_str = evoef_results.to_json()
    assert isinstance(results_json_str, str)

    # Testing that the decoded results have the correct number of fields
    decoded_results = json.loads(results_json_str)
    assert len(decoded_results) == 70

    # EvoEF2 results obtained from running the binary file
    # directly in the command line on the pdb file 1aac.pdb
    test_evoEF2_results = EvoEF2Output(
        log_info="""\n                                    EvoEF2                                                  \n  A framework for macromolecular modeling, e.g.,protein design, protein side-chain packing, \nprotein structure energy minimization, add and optimize hydrogen bonds, build mutant model, \ncalculate protein folding stability, calculate protein-protein binding free energy, etc     \n\n\n  Copyright (c) Xiaoqiang Huang (xiaoqiah@umich.edu; tommyhuangthu@foxmail.com)\n  Dept. of Computational Medicine & Bioinformatics\n  Medical School\n  University of Michigan\n############################################################################################\ncommand ComputeStability works\n\n""",
        error_info="",
        return_code=0,
        reference_ALA=-5.30,
        reference_CYS=-0.11,
        reference_ASP=-4.01,
        reference_GLU=-11.03,
        reference_PHE=2.72,
        reference_GLY=-14.65,
        reference_HIS=-1.47,
        reference_ILE=9.32,
        reference_LYS=-10.00,
        reference_LEU=6.45,
        reference_MET=3.79,
        reference_ASN=-4.31,
        reference_PRO=-4.53,
        reference_GLN=-1.94,
        reference_ARG=-2.64,
        reference_SER=-5.93,
        reference_THR=-3.33,
        reference_VAL=20.40,
        reference_TRP=2.00,
        reference_TYR=2.80,
        intraR_vdwatt=-19.25,
        intraR_vdwrep=3.66,
        intraR_electr=-0.26,
        intraR_deslvP=0.00,
        intraR_deslvH=-1.92,
        intraR_hbscbb_dis=-0.27,
        intraR_hbscbb_the=-0.00,
        intraR_hbscbb_phi=-0.00,
        aapropensity=-16.89,
        ramachandran=224.95,
        dunbrack=37.88,
        interS_vdwatt=-610.82,
        interS_vdwrep=40.41,
        interS_electr=-30.09,
        interS_deslvP=351.86,
        interS_deslvH=-255.43,
        interS_ssbond=0.00,
        interS_hbbbbb_dis=-48.90,
        interS_hbbbbb_the=-34.68,
        interS_hbbbbb_phi=-51.45,
        interS_hbscbb_dis=-9.61,
        interS_hbscbb_the=-7.40,
        interS_hbscbb_phi=-1.51,
        interS_hbscsc_dis=-8.93,
        interS_hbscsc_the=-3.47,
        interS_hbscsc_phi=-0.00,
        interD_vdwatt=0.00,
        interD_vdwrep=0.00,
        interD_electr=0.00,
        interD_deslvP=0.00,
        interD_deslvH=0.00,
        interD_ssbond=0.00,
        interD_hbbbbb_dis=0.00,
        interD_hbbbbb_the=0.00,
        interD_hbbbbb_phi=0.00,
        interD_hbscbb_dis=0.00,
        interD_hbscbb_the=0.00,
        interD_hbscbb_phi=0.00,
        interD_hbscsc_dis=0.00,
        interD_hbscsc_the=0.00,
        interD_hbscsc_phi=0.00,
        total=-463.90,
        time_spent=2.9,
    )

    # Comparing the returned EvoEF2 results from the results obtained from directly running
    # the binary file in the command line (test_evoEF2_results)
    assert evoef_results.__eq__(test_evoEF2_results)

    # Calculating the total of all the EvoEF2 reference energy fields
    evoef_ref_total_calc = (
        evoef_results.reference_ALA
        + evoef_results.reference_CYS
        + evoef_results.reference_ASP
        + evoef_results.reference_GLU
        + evoef_results.reference_PHE
        + evoef_results.reference_GLY
        + evoef_results.reference_HIS
        + evoef_results.reference_ILE
        + evoef_results.reference_LYS
        + evoef_results.reference_LEU
        + evoef_results.reference_MET
        + evoef_results.reference_ASN
        + evoef_results.reference_PRO
        + evoef_results.reference_GLN
        + evoef_results.reference_ARG
        + evoef_results.reference_SER
        + evoef_results.reference_THR
        + evoef_results.reference_VAL
        + evoef_results.reference_TRP
        + evoef_results.reference_TYR
    )

    # Calculating the total of all the EvoEF2 intraR energy fields
    evoef_intraR_total_calc = (
        evoef_results.intraR_vdwatt
        + evoef_results.intraR_vdwrep
        + evoef_results.intraR_electr
        + evoef_results.intraR_deslvP
        + evoef_results.intraR_deslvH
        + evoef_results.intraR_hbscbb_dis
        + evoef_results.intraR_hbscbb_the
        + evoef_results.intraR_hbscbb_phi
    )

    # Calculating the total of all the EvoEF2 interS energy fields
    evoef_interS_total_calc = (
        evoef_results.interS_vdwatt
        + evoef_results.interS_vdwrep
        + evoef_results.interS_electr
        + evoef_results.interS_deslvP
        + evoef_results.interS_deslvH
        + evoef_results.interS_ssbond
        + evoef_results.interS_hbbbbb_dis
        + evoef_results.interS_hbbbbb_the
        + evoef_results.interS_hbbbbb_phi
        + evoef_results.interS_hbscbb_dis
        + evoef_results.interS_hbscbb_the
        + evoef_results.interS_hbscbb_phi
        + evoef_results.interS_hbscsc_dis
        + evoef_results.interS_hbscsc_the
        + evoef_results.interS_hbscsc_phi
    )

    # Calculating the total of all the EvoEF2 interD energy fields
    evoef_interD_total_calc = (
        evoef_results.interD_vdwatt
        + evoef_results.interD_vdwrep
        + evoef_results.interD_electr
        + evoef_results.interD_deslvP
        + evoef_results.interD_deslvH
        + evoef_results.interD_ssbond
        + evoef_results.interD_hbbbbb_dis
        + evoef_results.interD_hbbbbb_the
        + evoef_results.interD_hbbbbb_phi
        + evoef_results.interD_hbscbb_dis
        + evoef_results.interD_hbscbb_the
        + evoef_results.interD_hbscbb_phi
        + evoef_results.interD_hbscsc_dis
        + evoef_results.interD_hbscsc_the
        + evoef_results.interD_hbscsc_phi
    )

    # Calculating the total of all the EvoEF2 energy fields
    evoef_total_calc = (
        evoef_ref_total_calc
        + evoef_intraR_total_calc
        + evoef_results.aapropensity
        + evoef_results.ramachandran
        + evoef_results.dunbrack
        + evoef_interS_total_calc
        + evoef_interD_total_calc
    )

    # Testing that the EvoEF2 ref total energy field corresponds to
    # summing all the energy fields (rounded to 1 decimal place)
    np.testing.assert_almost_equal(
        evoef_results.ref_total, evoef_ref_total_calc, decimal=1
    )

    # Testing that the EvoEF2 intraR total energy field corresponds to
    # summing the energy fields (rounded to 1 decimal place)
    np.testing.assert_almost_equal(
        evoef_results.intraR_total, evoef_intraR_total_calc, decimal=1
    )

    # Testing that the EvoEF2 interS total energy field corresponds to
    # summing the energy fields (rounded to 1 decimal place)
    np.testing.assert_almost_equal(
        evoef_results.interS_total, evoef_interS_total_calc, decimal=1
    )

    # Testing that the EvoEF2 interD energy field corresponds to
    # summing the energy fields (rounded to 1 decimal place)
    np.testing.assert_almost_equal(
        evoef_results.interD_total, evoef_interD_total_calc, decimal=1
    )

    # Testing that the EvoEF2 total energy field corresponds to
    # summing all the energy fields (rounded to 1 decimal place)
    np.testing.assert_almost_equal(evoef_results.total, evoef_total_calc, decimal=1)

    # Querying the data base to get the field names in the
    # EvoEF2Results table
    schema = graphene.Schema(query=Query)
    result = schema.execute(
        """{  __type(name: "EvoEF2Results") {name fields { name } } }"""
    )

    # Extracting the results of this query and converting from camel case
    # to snake case
    db_column_list = []
    for i in result.data["__type"]["fields"]:
        db_column_list.append(re.sub(r"(?<!^)(?=[A-Z])", "_", i["name"]).lower())

    # Removing some extra fields (id, state, state_id)
    db_column_list = [a for a in db_column_list if a not in ["id", "state", "state_id"]]

    # Checking that the fields from the evoef_results object
    # are the same as the fields in the EvoEF2Results table
    # in the data base
    assert set(evoef_results.__dict__.keys()) & set(db_column_list)
