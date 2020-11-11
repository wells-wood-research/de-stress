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

