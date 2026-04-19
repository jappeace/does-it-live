#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

mkdir -p output

docker run --rm \
  -v "$(pwd)/output:/output" \
  does-it-live:latest \
  -o /output/results.csv "$@"
