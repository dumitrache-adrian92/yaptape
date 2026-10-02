#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "${BASH_SOURCE[0]}")/prepare-test-db.sh"

stack test --test-arguments='--match Unit'
