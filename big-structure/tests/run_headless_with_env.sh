#!/usr/bin/env bash

set -a
source .env
set +a

python3 run_headless_for_testing.py
