import numpy as np
import json
import pathlib
import typing as t

from destress_big_structure.analysis import run_evoef2
from destress_big_structure.settings import EVOEF2_BINARY_PATH


def test_check_run_evoef_executes():
    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    evoef_results = run_evoef2(
        pdb_string=pdb_string, evoef2_binary_path=EVOEF2_BINARY_PATH
    )

    # Tests results encoding
    results_json_str = evoef_results.to_json()
    assert isinstance(results_json_str, str)

    # Test decoded results have the correct number of fields
    decoded_results = json.loads(results_json_str)
    assert len(decoded_results) == 64

    # Manually calculating the total of all the
    # EvoEF2 reference energy fields
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

    # Manually calculating the total of all the
    # EvoEF2 intraR energy fields
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

    # Manually calculating the total of all the
    # EvoEF2 interS energy fields
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

    # Manually calculating the total of all the
    # EvoEF2 interD energy fields
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

    # Manually calculating the total of all the
    # EvoEF2 energy fields
    evoef_total_calc = (
        evoef_ref_total_calc
        + evoef_intraR_total_calc
        + evoef_results.aapropensity
        + evoef_results.ramachandran
        + evoef_results.dunbrack
        + evoef_interS_total_calc
        + evoef_interD_total_calc
    )

    # Testing that the EvoEF2 total energy field
    # corresponds to summing all the energy fields
    # (rounded to 1 decimal place)
    np.testing.assert_almost_equal(
        evoef_results.ref_energy_total, evoef_ref_total_calc, decimal=1
    )
    np.testing.assert_almost_equal(
        evoef_results.intraR_energy_total, evoef_intraR_total_calc, decimal=1
    )
    np.testing.assert_almost_equal(
        evoef_results.interS_energy_total, evoef_interS_total_calc, decimal=1
    )
    np.testing.assert_almost_equal(
        evoef_results.interD_energy_total, evoef_interD_total_calc, decimal=1
    )
    np.testing.assert_almost_equal(evoef_results.total, evoef_total_calc, decimal=1)

    # print(evoef_results.ref_energy_total)
    # print(evoef_ref_total_calc)

    # print(evoef_results.intraR_energy_total)
    # print(evoef_intraR_total_calc)

    # print(evoef_results.interS_energy_total)
    # print(evoef_interS_total_calc)

    # print(evoef_results.interD_energy_total)
    # print(evoef_interD_total_calc)

    # print(evoef_results.total)
    # print(evoef_total_calc)

    # assert False
