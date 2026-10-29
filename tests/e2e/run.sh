#!/usr/bin/env bash
# End-to-end test of apps/tlgs-app: copies the demo fixture to a scratch folder, starts the app on it, runs Playwright, stops it.
# Usage: bash tests/e2e/run.sh [playwright args]   Env: TLGS_FIXTURE (default ~/workbench/fixtures/00-flow-cytometry-demo), E2E_PORT (3912)
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
FIXTURE="${TLGS_FIXTURE:-$HOME/workbench/fixtures/00-flow-cytometry-demo}"
PORT="${E2E_PORT:-3912}"
[ -d "$FIXTURE/00-drop-folder" ] || { echo "no fixture at $FIXTURE" >&2; exit 1; }
WORK=$(mktemp -d)
cp -R "$FIXTURE" "$WORK/data"
DATA_DIR="$WORK/data" R --quiet -e "shiny::runApp('$ROOT/apps/tlgs-app', host = '127.0.0.1', port = $PORT, launch.browser = FALSE)" > "$WORK/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null; wait $APP 2>/dev/null; rm -rf "${WORK:?}"' EXIT
for _ in $(seq 1 120); do curl -sf "http://127.0.0.1:$PORT/data/manifest.json" > /dev/null && break; sleep 1; done
curl -sf "http://127.0.0.1:$PORT/data/manifest.json" > /dev/null || { echo "app did not start:"; cat "$WORK/app.log"; exit 1; } >&2
cd "$ROOT/tests/e2e"
[ -d node_modules ] || npm ci
npx playwright install chromium > /dev/null
E2E_BASE_URL="http://127.0.0.1:$PORT" E2E_DATA_DIR="$WORK/data" npx playwright test "$@"
