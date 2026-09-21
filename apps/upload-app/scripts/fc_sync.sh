#!/bin/bash
# Flow-cytometry S3 uploader. Runs in the background, comes back at every login.
#
#     bash fc_sync.sh check     show what would be uploaded, install nothing
#     bash fc_sync.sh start     install and start; run once, from anywhere
#     bash fc_sync.sh stop      stop and uninstall
#     bash fc_sync.sh status    is it running, and what has it done lately
#
# Save the aws-creds key next to this script before start; it is moved to
# ~/.upload-app/aws-creds. Reads DATA_DIR (the folder above the projects),
# and optionally UPLOAD_SYNC_INTERVAL and AWS_DEFAULT_REGION, from
# ~/.env_data_ingestion_apps. Destination is fixed to the prod bucket.

set -u
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"

LABEL="com.sanavia.upload-app"
ENV_FILE="${DATA_INGESTION_ENV_FILE:-$HOME/.env_data_ingestion_apps}"
STATE="${UPLOAD_STATE_DIR:-$HOME/.upload-app}"
LOG_DIR="${UPLOAD_LOG_DIR:-$HOME/Library/Logs/upload-app}"
LOG="$LOG_DIR/fc_sync.log"
COPY="$STATE/fc_sync.sh"
PIDFILE="$STATE/fc_sync.pid"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
BUCKET_FIXED="sanavia-experiment-raw-data"
PREFIX_FIXED="flow-cytometry/"

die() { echo "ERROR: $*" >&2; exit 1; }
log() { printf '%s  %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" >> "$LOG"; }

load_config() {
    [[ -f "$ENV_FILE" ]] && { set -a; . "$ENV_FILE"; set +a; }
    ROOT="${UPLOAD_DIR:-${DATA_DIR:-}}";  ROOT="${ROOT/#\~/$HOME}"; ROOT="${ROOT%/}"
    CREDS="$STATE/aws-creds"
    INTERVAL="${UPLOAD_SYNC_INTERVAL:-30}"
    case "$INTERVAL" in ''|*[!0-9]*) INTERVAL=30 ;; esac
    export AWS_DEFAULT_REGION="${AWS_DEFAULT_REGION:-us-east-1}"
    unset AWS_PROFILE AWS_SESSION_TOKEN UPLOAD_S3_BUCKET UPLOAD_S3_PREFIX UPLOAD_AWS_CREDS_FILE
    BUCKET="$BUCKET_FIXED"; PREFIX="$PREFIX_FIXED"
    if [[ -f "$CREDS" ]]; then
        set -a; . "$CREDS"; set +a
        [[ -n "${UPLOAD_S3_BUCKET:-}" ]] && BUCKET="$UPLOAD_S3_BUCKET"
        [[ -n "${UPLOAD_S3_PREFIX:-}" ]] && PREFIX="$UPLOAD_S3_PREFIX"
    fi
}

check_aws() {
    command -v aws >/dev/null 2>&1 || die "aws not found - run: brew install awscli"
    local out; out=$(aws --version 2>&1) && return 0
    die "aws is installed at $(command -v aws) but cannot run: ${out##*: }
       fix, either:  softwareupdate --install-rosetta --agree-to-license
                or:  sudo rm $(command -v aws) $(dirname "$(command -v aws)")/aws_completer; brew install awscli"
}

preflight() {
    check_aws
    [[ -f "$ENV_FILE" ]]           || die "no $ENV_FILE - see the header of this script for what goes in it"
    load_config
    [[ -n "$ROOT" ]]  || die "DATA_DIR is not set in $ENV_FILE"
    [[ -d "$ROOT" ]]  || die "DATA_DIR is not a folder: $ROOT"
    [[ -f "$CREDS" ]] || die "no key file at $CREDS - save aws-creds next to this script and run start again"
    [[ -n "${AWS_ACCESS_KEY_ID:-}" && -n "${AWS_SECRET_ACCESS_KEY:-}" ]] || die "$CREDS has no AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY"
    check_layout
    case "$ROOT" in
        "$HOME/Documents"*|"$HOME/Desktop"*|"$HOME/Downloads"*|"$HOME/Library/CloudStorage"*|"$HOME/Library/Mobile Documents"*|/Volumes/*)
            echo "WARNING: $ROOT is in a folder macOS hides from background programs."
            echo "         The sync will run but see NO files until /bin/bash has Full Disk Access:"
            echo "         System Settings > Privacy & Security > Full Disk Access > + > Cmd-Shift-G > /bin/bash > Open"
            echo "         (status will say so if it is still blocked)"; echo ;;
    esac
}

MARKERS="plate_information_sheets|assay_data|qc_report|gating_results"

check_layout() {
    for m in plate_information_sheets assay_data qc_report gating_results; do
        [[ -d "$ROOT/$m" ]] && die "DATA_DIR is a single project ($ROOT holds $m/). It must be the folder ABOVE the projects - one level up - or the upload lands flat in the bucket root, where nothing can be deleted."
    done
    PROJECTS=$(find "$ROOT" -mindepth 2 -maxdepth 4 -type d 2>/dev/null \
        | grep -E "/($MARKERS)$" | sed -E "s#/($MARKERS)\$##; s#^$ROOT/##" | sort -u)
    [[ -n "$PROJECTS" ]] || die "no projects found under $ROOT (nothing holds $MARKERS one or more levels down)"
}

print_plan() {
    local n; n=$(echo "$PROJECTS" | grep -c .)
    echo "  folder:  $ROOT"
    echo "      to:  s3://$BUCKET/$PREFIX"
    echo "     key:  $CREDS  (${AWS_ACCESS_KEY_ID:-?})"
    echo "projects:  $n found; they land at"
    echo "$PROJECTS" | head -6 | sed "s#^#           s3://$BUCKET/$PREFIX#; s#\$#/#"
    (( n > 6 )) && echo "           ... and $((n-6)) more"
}

do_check() { preflight; print_plan; echo; echo "nothing installed. to install:  bash $0 start"; }

run_loop() {
    mkdir -p "$STATE" "$LOG_DIR"
    EXCLUDES=(
        --exclude "*.DS_Store"
        --exclude "._*"               --exclude "*/._*"
        --exclude "Icon*"             --exclude "*/Icon*"
        --exclude "Thumbs.db"         --exclude "*/Thumbs.db"
        --exclude ".Rhistory"         --exclude "*/.Rhistory"
        --exclude ".checkr-sync.json" --exclude "*/.checkr-sync.json"
        --exclude ".git/*"            --exclude "*/.git/*"
    )
    CHILD=""
    trap 'kill "$CHILD" 2>/dev/null; rm -f "$PIDFILE"; log "stopped"; exit 0' INT TERM
    echo $$ > "$PIDFILE"
    log "--- start (pid $$)"

    while true; do
        load_config
        if ! aws --version >/dev/null 2>&1; then
            log "ERROR aws cannot run ($(command -v aws || echo not installed)) - see: bash $COPY check"
        elif [[ ! -f "$ENV_FILE" ]]; then
            log "ERROR no env file at $ENV_FILE"
        elif [[ -z "$ROOT" ]]; then
            log "ERROR DATA_DIR is not set in $ENV_FILE"
        elif [[ ! -d "$ROOT" ]]; then
            log "ERROR missing folder: $ROOT"
        elif [[ ! -f "$CREDS" ]]; then
            log "ERROR no key file at $CREDS"
        elif ! ls "$ROOT" >/dev/null 2>"$STATE/.lserr" && grep -q "not permitted" "$STATE/.lserr"; then   # Documents/Desktop/Downloads are hidden from background jobs
            log "ERROR macOS blocks background access to $ROOT. Fix: System Settings > Privacy & Security > Full Disk Access > + > press Cmd-Shift-G, type /bin/bash, Open. Then: bash $COPY stop; bash $COPY start"
        elif [[ -z "$(find "$ROOT" -type f 2>/dev/null | head -1)" ]]; then
            log "ERROR no readable files under $ROOT"
        else
            nfiles=$(find "$ROOT" -type f 2>/dev/null | wc -l | tr -d ' ')
            args=(s3 sync "$ROOT" "s3://$BUCKET/$PREFIX"
                  --no-progress --only-show-errors "${EXCLUDES[@]}"
                  --cli-connect-timeout 10 --cli-read-timeout 120)
            if printf '%s\n' "${args[@]}" | grep -qx -- "--delete"; then
                log "REFUSING: --delete present in sync args"
            else
                aws "${args[@]}" >>"$LOG" 2>&1 &
                CHILD=$!
                if wait "$CHILD"; then log "ok $nfiles local files checked, $ROOT -> s3://$BUCKET/$PREFIX"; else log "ERROR sync failed: $ROOT"; fi
                CHILD=""
            fi
        fi
        if [[ $(stat -f%z "$LOG" 2>/dev/null || echo 0) -gt 1048576 ]]; then
            tail -n 500 "$LOG" > "$LOG.tmp" && mv "$LOG.tmp" "$LOG"
        fi
        sleep "$INTERVAL" &
        wait $! || break
    done
}

adopt_key() {
    load_config
    here="$(cd "$(dirname "$0")" && pwd)"
    for f in "$here/aws-creds" "$here/aws-creds.txt"; do
        [[ -f "$f" && "$f" != "$CREDS" ]] || continue
        if [[ -f "$CREDS" ]]; then echo "replacing key at $CREDS"; else echo "installing key to $CREDS"; fi
        mkdir -p "$(dirname "$CREDS")"
        mv -f "$f" "$CREDS" && chmod 600 "$CREDS"
        break
    done
}

do_start() {
    mkdir -p "$STATE" "$LOG_DIR" "$HOME/Library/LaunchAgents"
    adopt_key
    preflight
    self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
    [[ "$self" == "$COPY" ]] || cp "$self" "$COPY"
    chmod 755 "$COPY"

    cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>$LABEL</string>
    <key>ProgramArguments</key>
    <array><string>/bin/bash</string><string>$COPY</string><string>run</string></array>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>
    <key>ThrottleInterval</key><integer>10</integer>
    <key>EnvironmentVariables</key>
    <dict>
        <key>PATH</key><string>/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin</string>
        <key>HOME</key><string>$HOME</string>
        <key>DATA_INGESTION_ENV_FILE</key><string>$ENV_FILE</string>
        <key>UPLOAD_STATE_DIR</key><string>$STATE</string>
        <key>UPLOAD_LOG_DIR</key><string>$LOG_DIR</string>
    </dict>
    <key>StandardOutPath</key><string>$LOG_DIR/launchd.out.log</string>
    <key>StandardErrorPath</key><string>$LOG_DIR/launchd.err.log</string>
    <key>ProcessType</key><string>Background</string>
    <key>LowPriorityIO</key><true/>
</dict>
</plist>
PLIST
    plutil -lint "$PLIST" >/dev/null || { rm -f "$PLIST"; die "bad plist at $PLIST"; }

    launchctl bootout "gui/$UID/$LABEL" 2>/dev/null
    for _ in $(seq 40); do launchctl print "gui/$UID/$LABEL" >/dev/null 2>&1 || break; sleep 0.25; done
    launchctl bootstrap "gui/$UID" "$PLIST" || die "launchctl bootstrap failed"
    launchctl enable "gui/$UID/$LABEL" 2>/dev/null
    launchctl kickstart -k "gui/$UID/$LABEL" 2>/dev/null

    echo "started - runs in the background and at every login"
    print_plan
    echo "     log:  $LOG"
    echo
    echo "check:   bash $COPY status"
    echo "stop:    bash $COPY stop"
}

do_stop() {
    pid=$(cat "$PIDFILE" 2>/dev/null)
    running=0
    [[ -n "$pid" ]] && ps -p "$pid" -o command= 2>/dev/null | grep -q fc_sync && running=1
    launchctl bootout "gui/$UID/$LABEL" 2>/dev/null
    rm -f "$PLIST"
    if (( running )); then
        for _ in $(seq 20); do kill -0 "$pid" 2>/dev/null || break; sleep 0.25; done
        kill -0 "$pid" 2>/dev/null && { kill -TERM "$pid" 2>/dev/null; sleep 1; }
        kill -0 "$pid" 2>/dev/null && kill -KILL -- "-$pid" 2>/dev/null
        echo "stopped (pid $pid)"
    else
        echo "was not running"
    fi
    rm -f "$PIDFILE"
}

do_status() {
    if launchctl print "gui/$UID/$LABEL" >/dev/null 2>&1; then
        pid=$(launchctl print "gui/$UID/$LABEL" 2>/dev/null | awk '/^\tpid = /{print $3}')
        if [[ -n "$pid" ]]; then echo "installed: yes   running: yes (pid $pid)"; else echo "installed: yes   running: no"; fi
    else
        echo "installed: no"
    fi
    load_config
    if [[ -f "$CREDS" ]]; then echo "key: $CREDS  (${AWS_ACCESS_KEY_ID:-no AWS_ACCESS_KEY_ID inside})"; else echo "key: MISSING at $CREDS"; fi
    [[ -n "$ROOT" ]] && echo "folder: $ROOT$( [[ -d "$ROOT" ]] || echo '  (MISSING)')" || echo "folder: DATA_DIR not set in $ENV_FILE"
    echo "log: $LOG"
    [[ -f "$LOG" ]] && { echo "--- last 8 lines:"; tail -n 8 "$LOG"; } || echo "(no log yet - never ran)"
}

case "${1:-}" in
    run)    run_loop ;;
    check)  do_check ;;
    start)  do_start ;;
    stop)   do_stop ;;
    status) do_status ;;
    *)      echo "usage: bash $0 check | start | stop | status" >&2; exit 2 ;;
esac
