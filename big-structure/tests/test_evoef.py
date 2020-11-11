import pathlib
import typing as t

from destress_big_structure.analysis import run_evoef2
from destress_big_structure.settings import EVOEF2_BINARY_PATH


def test_check_run_evoef_executes():
    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    run_evoef2(pdb_string=pdb_string, evoef2_binary_path=EVOEF2_BINARY_PATH)

