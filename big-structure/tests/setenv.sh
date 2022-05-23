#!/usr/bin/env bash

# Show env vars
grep -v '^#' tests/.env-headless-test

# Export env vars
export $(grep -v '^#' tests/.env-headless-test | xargs)
