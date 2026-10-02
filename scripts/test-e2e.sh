#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "${BASH_SOURCE[0]}")/prepare-test-db.sh"

export PORT="${PORT:-3000}"
export YAPTAPE_URL="${YAPTAPE_URL:-http://127.0.0.1:${PORT}}"

app_pid=""
cleanup() {
  if [[ -n "$app_pid" ]]; then
    kill "$app_pid" 2>/dev/null || true
    wait "$app_pid" 2>/dev/null || true
  fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

if ! curl --silent --fail "$YAPTAPE_URL/health" >/dev/null; then
  stack run > .stack-work/test-app.log 2>&1 &
  app_pid=$!

  for attempt in $(seq 1 60); do
    if curl --silent --fail "$YAPTAPE_URL/health" >/dev/null; then
      break
    fi
    if ! kill -0 "$app_pid" 2>/dev/null; then
      cat .stack-work/test-app.log
      echo "The Yaptape app exited before becoming healthy." >&2
      exit 1
    fi
    if [[ "$attempt" -eq 60 ]]; then
      cat .stack-work/test-app.log
      echo "Timed out waiting for the Yaptape app at $YAPTAPE_URL." >&2
      exit 1
    fi
    sleep 1
  done
fi

npx playwright test
