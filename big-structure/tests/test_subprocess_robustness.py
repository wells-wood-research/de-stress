"""Tests for the robustness of the external tool wrappers in `analysis.py`.

These cover three failure modes that are not exercised by the other test
modules, as those all use full sized PDB structures and correctly installed
dependencies.
"""

import pathlib
import subprocess

from destress_big_structure.analysis import run_dfire2, run_evoef2, run_rosetta
from destress_big_structure.settings import DFIRE2_FOLDER_PATH, EVOEF2_BINARY_PATH


def small_pdb_string() -> str:
    """Returns a PDB string that is smaller than the default 8192 byte buffer."""

    test_path = pathlib.Path("tests/testing_files/1aac.pdb")

    with open(test_path) as inf:
        lines = [
            line
            for line in inf
            if line.startswith("ATOM") and int(line[22:26]) <= 12
        ]

    pdb_string = "".join(lines) + "TER\nEND\n"

    # The buffering bug this guards against only appears below the buffer size
    assert len(pdb_string) < 8192

    return pdb_string


def test_small_structures_are_written_to_disk_in_full():
    """Structures smaller than the write buffer must still reach the subprocess.

    `NamedTemporaryFile` is buffered, so a PDB string shorter than the buffer
    stays in memory unless it is flushed, and the external tool is handed an
    empty file. DFIRE2 reports an energy of 0.0 for an empty file and exits
    successfully, so the failure is silent rather than raised.
    """

    dfire2_results = run_dfire2(
        pdb_string=small_pdb_string(), dfire2_folder_path=DFIRE2_FOLDER_PATH
    )

    assert dfire2_results.return_code == 0
    assert dfire2_results.total is not None
    assert dfire2_results.total != 0.0


def test_missing_binary_does_not_raise():
    """A dependency that is not installed should produce an empty result row.

    Rosetta requires a licence, so it is legitimately absent on some
    installations. That must not abort the analysis of the whole design.
    """

    rosetta_results = run_rosetta(
        pdb_string=small_pdb_string(), rosetta_binary_path="/nonexistent/score_jd2"
    )

    assert rosetta_results.total_score is None
    assert "FileNotFoundError" in rosetta_results.error_info


def test_timeout_does_not_raise(monkeypatch):
    """A timed out run should produce an empty result row rather than raise.

    `subprocess.run` raises before binding a `CompletedProcess` when it times
    out, so the handler cannot read the return code from one.
    """

    def always_timeout(*args, **kwargs):
        raise subprocess.TimeoutExpired(cmd="stub", timeout=0.001)

    monkeypatch.setattr(subprocess, "run", always_timeout)

    evoef2_results = run_evoef2(
        pdb_string=small_pdb_string(), evoef2_binary_path=EVOEF2_BINARY_PATH
    )

    assert evoef2_results.total is None
    assert evoef2_results.return_code is None
