#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building Docker image..."
nix-build nix/docker.nix

echo "Loading image into Docker..."
docker load -i result

echo "Done. Image: does-it-live:latest"
