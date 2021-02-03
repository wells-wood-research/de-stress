import os

import dotenv

dotenv.load_dotenv()

EVOEF2_BINARY_PATH = os.getenv("EVOEF2_BINARY_PATH")
EVOEF2_BINARY_PATH_TEST = os.getenv("EVOEF2_BINARY_PATH_TEST")