import numpy as np
import json
import pathlib
import pytest
import typing as t
import graphene
import re

from destress_big_structure.analysis import run_rosetta
from destress_big_structure.settings import ROSETTA_BINARY_PATH
from destress_big_structure.elm_types import RosettaOutput
from destress_big_structure.schema import Query
from destress_big_structure.big_structure_models import big_structure_db_session


@pytest.mark.rosetta
def test_check_run_rosetta_executes():

    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    rosetta_results = run_rosetta(
        pdb_string=pdb_string, rosetta_binary_path=ROSETTA_BINARY_PATH
    )

    # Testing the results encoding
    results_json_str = rosetta_results.to_json()
    assert isinstance(results_json_str, str)

    # Testing that the decoded results have the correct number of fields
    decoded_results = json.loads(results_json_str)
    assert len(decoded_results) == 27

    # Rosetta results obtained from running the binary file
    # directly in the command line on the pdb file 1aac.pdb
    test_rosetta_results = RosettaOutput(
        log_info="",
        error_info="",
        return_code=0,
        dslf_fa13=0.0,
        fa_atr=-578.006456782364,
        fa_dun=156.649225256911,
        fa_elec=-140.0958080040151,
        fa_intra_rep=1.033905024330054,
        fa_intra_sol_xover4=17.85988440186921,
        fa_rep=58.79005766457982,
        fa_sol=310.6950288284573,
        hbond_bb_sc=-12.73563165584133,
        hbond_lr_bb=-43.38525607201246,
        hbond_sc=-9.886223057096258,
        hbond_sr_bb=-7.633857750166618,
        linear_chainbreak=0.0,
        lk_ball_wtd=-10.06315720368977,
        omega=11.43852503549201,
        overlap_chainbreak=0.0,
        p_aa_pp=-19.29384733591506,
        pro_close=7.250513283535823,
        rama_prepro=4.568231085182551,
        ref=41.43784000000002,
        score=-211.3770294189453,
        time=0.0,
        total_score=-211.3770272807427,
        yhh_planarity=0.0,
    )

    # Comparing the returned Rosetta results from the results obtained from directly running
    # the binary file in the command line (test_rosetta_results)
    assert rosetta_results.__eq__(test_rosetta_results)

    # Querying the data base to get the field names in the
    # RosettaResults table
    schema = graphene.Schema(query=Query)
    result = schema.execute(
        """{  __type(name: "RosettaResults") {name fields { name } } }"""
    )

    # Extracting the results of this query and converting from camel case
    # to snake case
    db_column_list = []
    for i in result.data["__type"]["fields"]:
        db_column_list.append(re.sub(r"(?<!^)(?=[A-Z])", "_", i["name"]).lower())

    # Removing some extra fields (id, state, state_id)
    db_column_list = [a for a in db_column_list if a not in ["id", "state", "state_id"]]

    # Checking that the fields from the rosetta_results object
    # are the same as the fields in the RosettaResults table
    # in the data base
    assert set(rosetta_results.__dict__.keys()) & set(db_column_list)
