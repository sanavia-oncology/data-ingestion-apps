#!/usr/bin/env bash
# Flow-cytometry S3 sync: sync each registered folder, sleep, repeat.
# Started by apps/upload-app/scripts/fc_sync_start.command.
#
# The publish manifest sits inside the synced folder, so it goes up with
# everything else - there is no separate upload step for it.
#
# The sleep sits between passes, so passes never overlap.
#
# Reads ~/.env_data_ingestion_apps:
#
#   UPLOAD_S3_BUCKET       destination bucket. Must match the creds file below:
#                          each laptop has a separate IAM user per environment
#                          and each can only see its own bucket. Crossing them
#                          gives "AccessDenied on ListBucket".
#   UPLOAD_S3_PREFIX       key prefix, e.g. "flow-cytometry/". The synced folder
#                          adds no segment of its own, so <group>/<project>/
#                          inside it lands at <prefix><group>/<project>/.
#   UPLOAD_AWS_CREDS_FILE  sh-style KEY=value file, e.g. ~/.checkr/aws-creds.
#                          Skipped if AWS_PROFILE is set; otherwise falls back
#                          to the default AWS credential chain.
#   UPLOAD_SYNC_INTERVAL   seconds to sleep between passes (default 30).
#   UPLOAD_MANIFEST_NAME   publish-manifest filename (default
#                          upload-manifest.csv), written at the top of the
#                          synced folder so this sync carries it.
#   AWS_DEFAULT_REGION     default us-east-1.
#
# Folders to upload live in ~/.upload-app/folders.txt, one absolute path per
# line, written by fc_sync_start.command.

set -uo pipefail
export PATH="/opt/homebrew/bin:/usr/local/bin:$PATH"

ENV_FILE="${DATA_INGESTION_ENV_FILE:-$HOME/.env_data_ingestion_apps}"
STATE="${UPLOAD_STATE_DIR:-$HOME/.upload-app}"
LOGS="${UPLOAD_LOG_DIR:-$HOME/Library/Logs/upload-app}"
LOG="$LOGS/fc_sync.log"

mkdir -p "$STATE" "$LOGS"
log() { printf '%s  %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" >> "$LOG"; }

# Junk that must never reach the bucket. aws matches each pattern against the
# whole path, so every name needs a */-prefixed twin to match below the root.
EXCLUDES=(
    # Raw FCS stays on the laptop; only the gating results go up.
    --exclude "*.fcs"             --exclude "*.FCS"
    --exclude "*.DS_Store"
    --exclude "._*"               --exclude "*/._*"
    --exclude "Icon*"             --exclude "*/Icon*"
    --exclude "Thumbs.db"         --exclude "*/Thumbs.db"
    --exclude ".Rhistory"         --exclude "*/.Rhistory"
    --exclude ".checkr-sync.json" --exclude "*/.checkr-sync.json"
    --exclude ".git/*"            --exclude "*/.git/*"
)

CHILD=""
trap 'kill -KILL "$CHILD" 2>/dev/null; rm -f "$STATE/fc_sync.pid"; log "fc_sync stopped"; exit 0' INT TERM
echo $$ > "$STATE/fc_sync.pid"

log "--- fc_sync start (pid $$)"

while true; do
    # Re-read every pass, so editing the env file or the folder list takes
    # effect without a restart.
    if [[ -f "$ENV_FILE" ]]; then
        set -a; . "$ENV_FILE"; set +a
    else
        log "ERROR no env file at $ENV_FILE"; sleep 60; continue
    fi

    BUCKET="${UPLOAD_S3_BUCKET:-}"
    PREFIX="${UPLOAD_S3_PREFIX:-}"
    INTERVAL="${UPLOAD_SYNC_INTERVAL:-30}"
    case "$INTERVAL" in ''|*[!0-9]*) INTERVAL=30 ;; esac
    export AWS_DEFAULT_REGION="${AWS_DEFAULT_REGION:-us-east-1}"

    # The creds file must match the bucket: the -dev key only works on -dev.
    CREDS="${UPLOAD_AWS_CREDS_FILE:-}"; CREDS="${CREDS/#\~/$HOME}"
    if [[ -z "${AWS_PROFILE:-}" && -n "$CREDS" && -f "$CREDS" ]]; then
        set -a; . "$CREDS"; set +a
        unset AWS_PROFILE
    fi

    if [[ -z "$BUCKET" ]]; then
        log "ERROR UPLOAD_S3_BUCKET is not set"
    else
        while IFS= read -r root || [[ -n "$root" ]]; do
            [[ -z "$root" || "$root" == \#* ]] && continue
            root="${root/#\~/$HOME}"
            [[ -d "$root" ]] || { log "ERROR missing folder: $root"; continue; }

            # The root adds no path segment: <group>/<project>/ under it lands
            # at <prefix><group>/<project>/.
            # Backgrounded and waited on, so a signal reaches us mid-transfer
            # and the child dies with us instead of uploading on alone.
            aws s3 sync "$root" "s3://$BUCKET/$PREFIX" \
                --no-progress --only-show-errors "${EXCLUDES[@]}" \
                --cli-connect-timeout 10 --cli-read-timeout 120 >>"$LOG" 2>&1 &
            CHILD=$!
            if wait "$CHILD"; then log "ok $root"; else log "ERROR sync failed: $root"; fi
            CHILD=""
        done < "$STATE/folders.txt" 2>/dev/null
    fi

    # Trim rather than rotate, so the log never grows unbounded.
    if [[ $(stat -f%z "$LOG" 2>/dev/null || echo 0) -gt 1048576 ]]; then
        tail -n 500 "$LOG" > "$LOG.tmp" && mv "$LOG.tmp" "$LOG"
    fi

    sleep "$INTERVAL" &     # backgrounded so a signal lands immediately
    wait $! || break
done
