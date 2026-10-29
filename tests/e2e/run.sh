#!/usr/bin/env bash
# End-to-end test of apps/tlgs-portal: unpacks Kwame's demo DB zip into a scratch folder, starts the app on it, runs Playwright, stops it.
# Usage: bash tests/e2e/run.sh [playwright args]   Env: TLGS_DEMO_ZIP (default ~/workbench/fixtures/_archive/2026-10-28_00-flow-cytometry-demo.zip), E2E_PORT (3912)
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
ZIP="${TLGS_DEMO_ZIP:-$HOME/workbench/fixtures/_archive/2026-10-28_00-flow-cytometry-demo.zip}"
PORT="${E2E_PORT:-3912}"
[ -f "$ZIP" ] || { echo "no demo zip at $ZIP" >&2; exit 1; }
WORK=$(mktemp -d)
unzip -q "$ZIP" -d "$WORK" -x '__MACOSX/*' '*/.DS_Store'
mv "$WORK/00-flow-cytometry-demo" "$WORK/data"
DATA_DIR="$WORK/data" R --quiet -e "shiny::runApp('$ROOT/apps/tlgs-portal', host = '127.0.0.1', port = $PORT, launch.browser = FALSE)" > "$WORK/app.log" 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null; wait $APP 2>/dev/null; rm -rf "${WORK:?}"' EXIT
for _ in $(seq 1 120); do curl -sf "http://127.0.0.1:$PORT/data/manifest.json" > /dev/null && break; sleep 1; done
curl -sf "http://127.0.0.1:$PORT/data/manifest.json" > /dev/null || { echo "app did not start:"; cat "$WORK/app.log"; exit 1; } >&2
cd "$ROOT/tests/e2e"
[ -d node_modules ] || npm ci
npx playwright install chromium > /dev/null
E2E_BASE_URL="http://127.0.0.1:$PORT" E2E_DATA_DIR="$WORK/data" npx playwright test "$@"
