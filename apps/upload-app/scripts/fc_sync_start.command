#!/usr/bin/env zsh
# Pick the folder to upload, then start the uploader. Run once: launchd brings
# it back at every login (which runs fc_sync.sh directly, with no picker).
set -eo pipefail
export PATH="/opt/homebrew/bin:/usr/local/bin:$PATH"   # a Finder launch doesn't inherit a Terminal PATH

SCRIPT="${0:A:h}/fc_sync.sh"
LABEL="com.sanavia.upload-app"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
LOGS="$HOME/Library/Logs/upload-app"
STATE="${UPLOAD_STATE_DIR:-$HOME/.upload-app}"
FOLDERS="$STATE/folders.txt"

[[ -f "$SCRIPT" ]] || { print -u2 "no fc_sync.sh at $SCRIPT"; exit 1; }
[[ -f "$HOME/.env_data_ingestion_apps" ]] || { print -u2 "no ~/.env_data_ingestion_apps — see the header of fc_sync.sh for the keys it needs"; exit 1; }

mkdir -p "$STATE" "$LOGS" "$HOME/Library/LaunchAgents"
touch "$FOLDERS"

folder=$(osascript <<'APPLESCRIPT'
try
    set chosen to choose folder with prompt "Pick the folder to sync to AWS"
    POSIX path of chosen
on error number -128
    return ""
end try
APPLESCRIPT
)
folder="${folder%/}"

if [[ -n "$folder" && -d "$folder" ]]; then
  grep -qxF "$folder" "$FOLDERS" || print "$folder" >> "$FOLDERS"
elif [[ ! -s "$FOLDERS" ]]; then
  print -u2 "no folder chosen and none registered — nothing to sync"
  exit 1
fi

# KeepAlive, not StartInterval: fc_sync.sh loops on its own, so a timer would
# fire on top of a pass that is still running.
cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>$LABEL</string>
    <key>ProgramArguments</key>
    <array><string>/bin/bash</string><string>$SCRIPT</string></array>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>
    <key>ThrottleInterval</key><integer>10</integer>
    <key>EnvironmentVariables</key>
    <dict>
        <key>PATH</key><string>/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin</string>
        <key>HOME</key><string>$HOME</string>
    </dict>
    <key>StandardOutPath</key><string>$LOGS/launchd.out.log</string>
    <key>StandardErrorPath</key><string>$LOGS/launchd.err.log</string>
    <key>ProcessType</key><string>Background</string>
    <key>LowPriorityIO</key><true/>
</dict>
</plist>
PLIST

plutil -lint "$PLIST" >/dev/null || { print -u2 "bad plist at $PLIST"; rm -f "$PLIST"; exit 1; }

# bootout first, or a changed plist is not picked up.
launchctl bootout "gui/$UID/$LABEL" 2>/dev/null || true
launchctl bootstrap "gui/$UID" "$PLIST"
launchctl enable "gui/$UID/$LABEL" 2>/dev/null || true   # a past `disable` sticks
# bootstrap registers it for future logins but does not start it now, so kick
# it once. Without this the first sync waits until the next login.
launchctl kickstart -k "gui/$UID/$LABEL" 2>/dev/null || true

print "syncing:"
sed 's/^/  /' "$FOLDERS"
print "\nlog:  $LOGS/fc_sync.log"
print "stop: fc_sync_stop.command"
