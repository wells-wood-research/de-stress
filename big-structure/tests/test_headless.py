from pathlib import Path
import subprocess
import os
from dotenv import load_dotenv
from destress_big_structure.elm_types import (
    DesignMetricsOutputRow,
)

HEADLESS_DESTRESS_WORKERS = os.getenv("HEADLESS_DESTRESS_WORKERS")

# Testing the consistency of DE-STRESS headless with DE-STRESS UI
def test_check_headless_ui_consistency():

    test_path = "tests/testing_files/test_headless/"

    # DE-STRESS UI results for 1aac.pdb test file
    destress_ui_results = DesignMetricsOutputRow(
        design_name="1aac",
        file_name="1aac.pdb",
        composition_ALA=0.12381,
        composition_CYS=0.00952,
        composition_ASP=0.04762,
        composition_GLU=0.08571,
        composition_PHE=0.0381,
        composition_GLY=0.06667,
        composition_HIS=0.04762,
        composition_ILE=0.0381,
        composition_LYS=0.07619,
        composition_LEU=0.0381,
        composition_MET=0.04762,
        composition_ASN=0.01905,
        composition_PRO=0.06667,
        composition_GLN=0.00952,
        composition_ARG=0.01905,
        composition_SER=0.02857,
        composition_THR=0.07619,
        composition_VAL=0.11429,
        composition_TRP=0.00952,
        composition_UNK=0.0,
        composition_TYR=0.0381,
        hydrophobic_fitness=-25.83079,
        isoelectric_point=5.7,
        mass=11490.05638,
        num_residues=105.0,
        packing_density=56.84263,
        budeff_total=-1399.0744,
        budeff_steric=64.614,
        budeff_desolvation=-961.71459,
        budeff_charge=-501.9738,
        evoef2_total=-463.9,
        evoef2_ref_total=-21.77,
        evoef2_intraR_total=227.9,
        evoef2_interS_total=-670.02,
        evoef2_interD_total=0.0,
        dfire2_total=-161.6,
        rosetta_total=-211.37703,
        rosetta_fa_atr=-578.00646,
        rosetta_fa_rep=58.79006,
        rosetta_fa_intra_rep=1.03391,
        rosetta_fa_elec=-140.09581,
        rosetta_fa_sol=310.69503,
        rosetta_lk_ball_wtd=-10.06316,
        rosetta_fa_intra_sol_xover4=17.85988,
        rosetta_hbond_lr_bb=-43.38526,
        rosetta_hbond_sr_bb=-7.63386,
        rosetta_hbond_bb_sc=-12.73563,
        rosetta_hbond_sc=-9.88622,
        rosetta_dslf_fa13=0.0,
        rosetta_rama_prepro=4.56823,
        rosetta_p_aa_pp=-19.29385,
        rosetta_fa_dun=156.64923,
        rosetta_omega=11.43853,
        rosetta_pro_close=7.25051,
        rosetta_yhh_planarity=0.0,
        aggrescan3d_total_value=-90.2641,
        aggrescan3d_avg_value=-0.8597,
        aggrescan3d_min_value=-2.953,
        aggrescan3d_max_value=1.0632,
    )

    # Creating bash command to run headless destress
    headless_destress_run_cmd = ["./run_headless_with_env.sh"]

    # Using subprocess to run this command
    subprocess.run(
        headless_destress_run_cmd,
    )

    # Opening csv to insert into
    with open(test_path + "design_data.csv", "r") as f:
        lines = f.readlines()

    # Extracting the results from the csv output
    design_data_csv_results = lines[1].replace("\n", "").split(",")

    # Converting this output to a dictionary
    destress_headless_results_dict = dict(
        zip(destress_ui_results.__dict__.keys(), design_data_csv_results)
    )

    # Changing the values of every metric (expect design_name
    # and file_name) to float and rounding to 5 places
    for key in destress_headless_results_dict:
        if key not in ["design_name", "file_name"]:
            destress_headless_results_dict[key] = float(
                destress_headless_results_dict[key]
            )
            destress_headless_results_dict[key] = round(
                destress_headless_results_dict[key], 5
            )

    # Creating a DesignMetricsOutputRow object with these results
    destress_headless_results = DesignMetricsOutputRow(**destress_headless_results_dict)

    # Comparing the destress headless results with the destress ui results for
    # 1aac.pdb
    assert destress_ui_results.__eq__(destress_headless_results)
