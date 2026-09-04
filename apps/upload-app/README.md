# upload-app

Uploads folders to S3 in the background, and records which projects inside them
are cleared to appear on the web.

Needs macOS, R with `shiny`, `bslib`, `DT`, `jsonlite`, and AWS CLI v2 on `PATH`.

## Setup

Fill in `~/.env_data_ingestion_apps` — the keys are listed in the header of
`scripts/fc_sync.sh`. Then run once:

```
scripts/fc_sync_start.command    pick the folder, start uploading
scripts/fc_sync_stop.command     stop
```

`fc_sync_start.command` opens the folder picker, records the choice in
`~/.upload-app/folders.txt`, and registers a launchd agent that restarts at
every login. At login it runs `fc_sync.sh` directly, so there is no picker
prompt. Cancelling the picker keeps the folder already registered; cancelling
with none registered refuses to start.

## The sync

`scripts/fc_sync.sh` runs `aws s3 sync` over each registered folder, sleeps
`UPLOAD_SYNC_INTERVAL` (30s), repeats. The sleep sits between passes, so passes
cannot overlap.

Raw `.fcs` is excluded; gating results go up. A folder holding
`<group>/<project>/` lands at `s3://<bucket>/<prefix><group>/<project>/` — the
folder itself adds no path segment.

`UPLOAD_S3_BUCKET` and `UPLOAD_AWS_CREDS_FILE` are a pair: each laptop has a
separate IAM user per environment and each sees only its own bucket, so
crossing them gives `AccessDenied on ListBucket`. The policy grants no
`s3:DeleteObject`, so never add `--delete`.

## The manifest

One CSV written at the top of the synced folder, so the same sync carries it to
`s3://<bucket>/<prefix>upload-manifest.csv`. The web app reads that one object
to decide what to show.

Columns: `project_group`, `project_name`, `s3_uri`, `display` (`yes`/`no`),
`updated_at`, `updated_by`.

The app lists every project found under the registered folders. Select rows and
press **Add** or **Remove** to set `display`. Status is **Waiting** until
decided, then **Added** or **Removed** — Remove writes a row rather than
deleting one, so a withdrawn project is distinguishable from one never
submitted.

## Troubleshooting

```bash
tail -f "$HOME/Library/Logs/upload-app/fc_sync.log"
launchctl print gui/$(id -u)/com.sanavia.upload-app | head -20
```

The log self-truncates to its last 500 lines past 1 MB.

If a launcher does nothing on a Finder double-click, `aws` is not on its `PATH`
— Finder starts with a bare `PATH`, and the scripts prepend the Homebrew
directories only.
