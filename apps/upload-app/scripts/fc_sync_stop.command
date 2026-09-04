#!/usr/bin/env zsh
LABEL="com.sanavia.upload-app"
PIDFILE="${UPLOAD_STATE_DIR:-$HOME/.upload-app}/fc_sync.pid"

# bootout first: with KeepAlive set, killing the pid alone just restarts it.
launchctl bootout "gui/$UID/$LABEL" 2>/dev/null
rm -f "$HOME/Library/LaunchAgents/$LABEL.plist"

# A hard reboot skips the trap that removes the pid file, so the pid left
# behind is likely to have been reused by something unrelated. Check what the
# process actually is before signalling it.
pid=$(cat "$PIDFILE" 2>/dev/null)
if [[ -n "$pid" ]] && ps -p "$pid" -o command= 2>/dev/null | grep -q fc_sync; then
  kill -TERM "$pid" 2>/dev/null
  for _ in {1..20}; do kill -0 "$pid" 2>/dev/null || break; sleep 0.25; done
  # SIGKILL can't be trapped, so take the group or the aws child keeps going.
  kill -0 "$pid" 2>/dev/null && kill -KILL -- "-$pid" 2>/dev/null
  print "stopped fc_sync (pid $pid)"
else
  print "fc_sync was not running"
fi
rm -f "$PIDFILE"
