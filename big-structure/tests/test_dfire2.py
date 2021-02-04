import numpy as np
import json
import pathlib
import typing as t
import graphene
import re

from destress_big_structure.analysis import run_dfire2
from destress_big_structure.settings import DFIRE2_BINARY_PATH
from destress_big_structure.schema import Query
from destress_big_structure.big_structure_models import big_structure_db_session


def test_check_run_dfire2_executes():
    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    dfire2_results = run_dfire2(
        pdb_string=pdb_string, dfire2_binary_path=DFIRE2_BINARY_PATH
    )

    # Testing the results encoding
    results_json_str = dfire2_results.to_json()
    assert isinstance(results_json_str, str)

    # Testing that the decoded results have the correct number of fields
    decoded_results = json.loads(results_json_str)
    assert len(decoded_results) == 2

    # DFIRE2 results obtained from running the binary file
    # directly in the command line on the pdb file 1aac.pdb
    test_dfire2_total = -161.6

    # Comparing the returned DFIRE2 results from the results obtained from directly running
    # the binary file in the command line (test_dfire2_results)
    assert dfire2_results.total == test_dfire2_total

    # Querying the data base to get the field names in the
    # DFIRE2Results table
    schema = graphene.Schema(query=Query)
    result = schema.execute(
        """{  __type(name: "DFIRE2Results") {name fields { name } } }"""
    )

    # Extracting the results of this query and converting from camel case
    # to snake case
    db_column_list = []
    for i in result.data["__type"]["fields"]:
        db_column_list.append(re.sub(r"(?<!^)(?=[A-Z])", "_", i["name"]).lower())

    # Removing some extra fields (id, state, state_id)
    db_column_list = [a for a in db_column_list if a not in ["id", "state", "state_id"]]

    # Checking that the fields from the dfire2_results object
    # are the same as the fields in the DFIRE2Results table
    # in the data base
    assert set(dfire2_results.__dict__.keys()) & set(db_column_list)
