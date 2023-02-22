#!/bin/bash

set -euo pipefail

GITROOT=$(git rev-parse --show-toplevel)

tie -o "$GITROOT/public-api" \
  --module-name TsplayPublic \
  --package-name tsplay-public \
  "$GITROOT/public-api.yml"
