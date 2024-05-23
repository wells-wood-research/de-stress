import subprocess
import sys

# Setting the argument that is passed in
pdb_file_path = sys.argv[1]

# Constructing the aggrescan command
cmd = ["aggrescan", "-i", pdb_file_path, "-w", "output", "-v", "4"]

# Using subprocess to run this command and capturing the output
aggrescan3d_stdout = subprocess.call(cmd)
