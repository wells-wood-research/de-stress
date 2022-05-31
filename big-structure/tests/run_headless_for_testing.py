import subprocess
import sys

# Setting the argument that is passed in
test_pdb_path = "tests/testing_files/test_headless/"

# Creating bash command to run headless destress
headless_destress_run_cmd = [
    "poetry",
    "run",
    "headless_destress",
    test_pdb_path,
]

# Using subprocess to run this command
subprocess.run(
    headless_destress_run_cmd,
)
