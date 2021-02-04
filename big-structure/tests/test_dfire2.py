import numpy as np
import json
import pathlib
import typing as t
import graphene
import re

from destress_big_structure.analysis import run_dfire2
from destress_big_structure.settings import DFIRE2_BINARY_PATH

# from destress_big_structure.elm_types import EvoEF2Output
from destress_big_structure.schema import Query
from destress_big_structure.big_structure_models import big_structure_db_session


def test_check_run_dfire2_executes():
    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        pdb_string = inf.read()

    dfire2_results = run_dfire2(
        pdb_string=pdb_string, dfire2_binary_path=DFIRE2_BINARY_PATH
    )

    print(dfire2_results)

    assert False
