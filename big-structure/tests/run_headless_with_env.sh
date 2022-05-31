#!/usr/bin/env bash

set -a
source tests/.env
set +a

python3 tests/run_headless_for_testing.py
