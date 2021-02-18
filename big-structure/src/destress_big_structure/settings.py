import os

import dotenv

dotenv.load_dotenv()

EVOEF2_BINARY_PATH = os.getenv("EVOEF2_BINARY_PATH")
DFIRE2_BINARY_PATH = os.getenv("DFIRE2_BINARY_PATH")
ROSETTA_BINARY_PATH = os.getenv("ROSETTA_BINARY_PATH")
