#!/bin/bash

set -euo pipefail

GITROOT=$(git rev-parse --show-toplevel)

fourmolu -i "$GITROOT/public-api/"
fourmolu -i "$GITROOT/server/src/"
