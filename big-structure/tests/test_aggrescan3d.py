import numpy as np
import json
import pathlib
import pytest
import typing as t
import graphene
import re

from destress_big_structure.analysis import run_aggrescan3d
from destress_big_structure.settings import AGGRESCAN3D_SCRIPT_PATH
from destress_big_structure.elm_types import Aggrescan3DOutput
from destress_big_structure.schema import Query
from destress_big_structure.big_structure_models import big_structure_db_session


def test_check_run_aggrescan3D_executes():

    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    aggrescan3d_results = run_aggrescan3d(
        pdb_string=pdb_string, aggrescan3d_script_path=AGGRESCAN3D_SCRIPT_PATH
    )

    # Testing the results encoding
    results_json_str = aggrescan3d_results.to_json()
    assert isinstance(results_json_str, str)

    # Testing that the decoded results have the correct number of fields
    decoded_results = json.loads(results_json_str)
    assert len(decoded_results) == 12

    # Testing results
    test_residue_name_list = [
        "D",
        "K",
        "A",
        "T",
        "I",
        "P",
        "S",
        "E",
        "S",
        "P",
        "F",
        "A",
        "A",
        "A",
        "E",
        "V",
        "A",
        "D",
        "G",
        "A",
        "I",
        "V",
        "V",
        "D",
        "I",
        "A",
        "K",
        "M",
        "K",
        "Y",
        "E",
        "T",
        "P",
        "E",
        "L",
        "H",
        "V",
        "K",
        "V",
        "G",
        "D",
        "T",
        "V",
        "T",
        "W",
        "I",
        "N",
        "R",
        "E",
        "A",
        "M",
        "P",
        "H",
        "N",
        "V",
        "H",
        "F",
        "V",
        "A",
        "G",
        "V",
        "L",
        "G",
        "E",
        "A",
        "A",
        "L",
        "K",
        "G",
        "P",
        "M",
        "M",
        "K",
        "K",
        "E",
        "Q",
        "A",
        "Y",
        "S",
        "L",
        "T",
        "F",
        "T",
        "E",
        "A",
        "G",
        "T",
        "Y",
        "D",
        "Y",
        "H",
        "C",
        "T",
        "P",
        "H",
        "P",
        "F",
        "M",
        "R",
        "G",
        "K",
        "V",
        "V",
        "V",
        "E",
    ]

    test_residue_score_list = [
        "-2.6269",
        "-1.8741",
        "0.0",
        "-0.5537",
        "-0.0538",
        "-0.4515",
        "-0.9085",
        "-1.8392",
        "-1.0009",
        "-0.7549",
        "-0.7229",
        "-1.061",
        "-1.023",
        "-0.818",
        "-1.8304",
        "-0.8613",
        "-1.1069",
        "-2.0852",
        "-1.0478",
        "-0.2595",
        "0.5824",
        "0.6944",
        "0.0",
        "-1.3383",
        "0.0",
        "0.0",
        "-2.3843",
        "-1.324",
        "-2.7955",
        "0.0",
        "-2.8198",
        "-1.7957",
        "-2.5332",
        "-2.3591",
        "-1.0628",
        "-1.8161",
        "0.0",
        "-2.2393",
        "-0.664",
        "-0.8022",
        "-0.6702",
        "-0.0716",
        "0.0",
        "0.0",
        "0.0",
        "0.0",
        "0.0",
        "-2.1546",
        "-1.7817",
        "-1.183",
        "-0.1894",
        "-0.7938",
        "0.0",
        "0.0",
        "0.0",
        "-0.9297",
        "0.0",
        "0.7566",
        "-0.0202",
        "-0.7669",
        "-0.4293",
        "0.0",
        "-1.6058",
        "-2.0905",
        "-0.9407",
        "-0.1058",
        "-0.4858",
        "-1.3168",
        "0.0",
        "-0.8448",
        "-0.4346",
        "0.0",
        "-2.6002",
        "-2.953",
        "-2.3369",
        "-1.8861",
        "0.0",
        "0.0",
        "0.0",
        "0.0",
        "-0.33",
        "0.0",
        "-1.1197",
        "-1.8889",
        "-1.3342",
        "-1.0746",
        "-1.1789",
        "0.0",
        "-2.3114",
        "0.0",
        "-1.2705",
        "0.0",
        "-0.4863",
        "-0.0604",
        "-0.1077",
        "0.1907",
        "1.0632",
        "0.0",
        "-2.3941",
        "-2.4412",
        "-2.2709",
        "0.0",
        "-1.3621",
        "0.0",
        "-2.5153",
    ]

    # Aggrescan3d results obtained from running the webserver
    # on the pdb file 1aac.pdb and extracting info manually
    # from the server
    test_aggrescan3d_results = Aggrescan3DOutput(
        log_info="",
        error_info="",
        return_code=0,
        protein_list=";".join(["folded"] * 105),
        chain_list=";".join(["A"] * 105),
        residue_number_list=";".join(list(map(str, list(range(1, 106))))),
        residue_name_list=";".join(test_residue_name_list),
        residue_score_list=";".join(test_residue_score_list),
        max_value=1.0632,
        avg_value=-0.8597,
        min_value=-2.953,
        total_value=-90.2641,
    )

    # Comparing the returned Aggrescan3D results from the results obtained from
    # the webserver (test_aggrescan3d_results)
    assert aggrescan3d_results.__eq__(test_aggrescan3d_results)

    # Querying the data base to get the field names in the
    # Aggrescan3DResults table
    schema = graphene.Schema(query=Query)
    result = schema.execute(
        """{  __type(name: "Aggrescan3DResults") {name fields { name } } }"""
    )

    # Extracting the results of this query and converting from camel case
    # to snake case
    db_column_list = []
    for i in result.data["__type"]["fields"]:
        db_column_list.append(re.sub(r"(?<!^)(?=[A-Z])", "_", i["name"]).lower())

    # Removing some extra fields (id, state, state_id)
    db_column_list = [a for a in db_column_list if a not in ["id", "state", "state_id"]]

    # Checking that the fields from the aggrescan3d_results object
    # are the same as the fields in the Aggrescan3DResults table
    # in the data base
    assert set(aggrescan3d_results.__dict__.keys()) & set(db_column_list)
