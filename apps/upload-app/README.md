# upload-app

Uploads folders to S3 in the background, and records which projects inside them
are cleared to appear on the web.

Needs macOS, R with `shiny`, `bslib`, `DT`, `jsonlite`, and AWS CLI v2 on `PATH`.

## Setup

Two files go to the scientist: `scripts/fc_sync.sh` (over Slack, nothing
secret) and their `aws-creds` key (1Password only). Both in the same folder,
Downloads is fine. Then, in Terminal:

```
bash ~/Downloads/fc_sync.sh start
```

Needs AWS CLI v2 (`brew install awscli`). The folder comes from `DATA_DIR` in
`~/.env_data_ingestion_apps` — the same line the other apps already read, so a
Mac that runs them needs nothing added. `UPLOAD_DIR` overrides it if the upload
root differs.

`start` moves the key to `~/.upload-app/aws-creds`, copies the script to
`~/.upload-app/fc_sync.sh`, and registers a launchd agent that runs the copy in
the background and at every login. It checks everything first and refuses with
a reason rather than installing something broken. A new key later: same two
steps, it replaces the old one.

```
bash ~/.upload-app/fc_sync.sh status    installed? running? which key? last log lines
bash ~/.upload-app/fc_sync.sh stop      stop and uninstall
```

No double-click, no folder picker: a `.command` opened from Finder hits
Gatekeeper on downloaded files, and a picker dialog can open behind other
windows. `bash <file>` in Terminal sidesteps both and shows every message.

## The sync

`fc_sync.sh run` is the loop launchd runs: `aws s3 sync` of `DATA_DIR`,
sleep `UPLOAD_SYNC_INTERVAL` (30s), repeat. Passes cannot overlap. Env file
and key are re-read every pass, so edits apply without a restart; a bad config
is logged and retried, never fatal.

Everything in the folder goes up, raw `.fcs` included. A folder holding
`<group>/<project>/` lands at `s3://<bucket>/<prefix><group>/<project>/` — the
folder itself adds no path segment.

The **sync** hardcodes the destination — `sanavia-experiment-raw-data` and
`flow-cytometry/`. A background uploader on a scientist's laptop always goes to
prod, and a stale env-file line cannot redirect it. Only the key file may name
another bucket, because a key and its bucket are a matched pair (each laptop
has a separate IAM user per environment; crossing them gives `AccessDenied on
ListBucket`).

The **app** still reads `UPLOAD_S3_BUCKET` and `UPLOAD_S3_PREFIX` from the env
file, so local development can point it at the test bucket. It only builds the
`s3_uri` strings in the manifest; it never uploads.

Deletes are refused twice over: the IAM policy grants no `s3:DeleteObject`, and
the bucket policy (`scripts/aws/bucket-no-delete-policy.json`) denies it to
everyone, admins included. Never add `--delete`.

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
bash ~/.upload-app/fc_sync.sh status
tail -f "$HOME/Library/Logs/upload-app/fc_sync.log"
```

The log self-truncates to its last 500 lines past 1 MB. From the AWS side, the
laptop's key last-used time (`aws iam get-access-key-last-used`) moves on every
pass — the sync lists the prefix even when there is nothing to upload — though
IAM reports it with some minutes of lag.
