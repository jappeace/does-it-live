#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

if ! docker image inspect does-it-live:latest &>/dev/null; then
  echo "Image not found, building..."
  ./scripts/build-docker.sh
fi

mkdir -p output

docker run --rm \
  -v "$(pwd)/output:/output" \
  does-it-live:latest \
  -o /output/results.csv "$@"
