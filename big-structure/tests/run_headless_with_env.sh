#!/usr/bin/env bash

set -a
source tests/.env
set +a

poetry run headless_destress "tests/testing_files/test_headless/"
