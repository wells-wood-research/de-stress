import subprocess
import sys

# Setting the argument that is passed in
test_path = sys.argv[1]

# Creating bash command to run headless destress
headless_destress_run_cmd = [
    "poetry",
    "run",
    "headless_destress",
    test_path,
]

# Using subprocess to run this command
subprocess.run(
    headless_destress_run_cmd,
)
