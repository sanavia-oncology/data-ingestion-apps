# Read ~/.env_data_ingestion_apps and resolve every path the upload app uses.
#
# The other three apps call dotenv::load_dot_env() on the same file. This one
# parses it directly instead, for two reasons: the launchers `source` the file
# with the shell, so values legitimately contain $HOME / $DATA_DIR references
# that dotenv leaves unexpanded, and it drops a package dependency from an app
# whose whole job is to shell out.

APP_ENV_FILE = "~/.env_data_ingestion_apps"

read_env_file = function(path) {
    path = path.expand(path)
    if (!file.exists(path)) return(stats::setNames(character(0), character(0)))

    lines = trimws(readLines(path, warn = FALSE))
    lines = lines[nzchar(lines) & !startsWith(lines, "#")]
    lines = sub("^export[[:space:]]+", "", lines)

    m = regmatches(lines, regexec("^([A-Za-z_][A-Za-z0-9_]*)=(.*)$", lines))
    keys = vapply(m, function(x) if (length(x) == 3L) x[2] else NA_character_, character(1))
    vals = vapply(m, function(x) if (length(x) == 3L) x[3] else NA_character_, character(1))
    ok = !is.na(keys)
    keys = keys[ok]
    vals = trimws(vals[ok])
    vals = sub('^"(.*)"$', "\\1", vals)
    vals = sub("^'(.*)'$", "\\1", vals)

    # Expand $VAR / ${VAR} against earlier keys in the same file, then the
    # process environment — the same order the shell would resolve them in.
    out = stats::setNames(character(0), character(0))
    for (i in seq_along(keys)) {
        out[[keys[i]]] = expand_env_refs(vals[i], out)
    }
    out
}

expand_env_refs = function(value, resolved) {
    if (!nzchar(value)) return(value)
    repeat {
        m = regexpr("\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?", value)
        if (m == -1L) break
        token = regmatches(value, m)
        name = gsub("[${}]", "", token)
        repl = if (name %in% names(resolved)) resolved[[name]] else Sys.getenv(name, "")
        value = paste0(substr(value, 1, m - 1),
                       repl,
                       substr(value, m + attr(m, "match.length"), nchar(value)))
    }
    path.expand(value)
}

# Everything the app needs, resolved once per session. Defaults here mirror
# fc_sync.sh's header so a partially-filled env file still starts the app —
# the UI reports what is missing rather than failing at load.
build_cfg = function() {
    conf = read_env_file(Sys.getenv("DATA_INGESTION_ENV_FILE", APP_ENV_FILE))

    getv = function(key, default = "") {
        if (key %in% names(conf) && nzchar(conf[[key]])) return(unname(conf[[key]]))
        v = Sys.getenv(key, "")
        if (nzchar(v)) v else default
    }

    state_dir = path.expand(Sys.getenv("UPLOAD_STATE_DIR", "~/.upload-app"))

    # A filename, not a path: the manifest sits at the top of the synced
    # folder, so the same `aws s3 sync` carries it and it lands at
    # s3://<bucket>/<prefix>upload-manifest.csv. No second upload step.
    manifest_name = getv("UPLOAD_MANIFEST_NAME", "upload-manifest.csv")


    list(
        folders_file = file.path(state_dir, "folders.txt"),
        manifest_name = manifest_name,
        bucket       = getv("UPLOAD_S3_BUCKET"),
        prefix       = getv("UPLOAD_S3_PREFIX")
    )
}

# s3://bucket/<prefix><path under the sync root>/
#
# The root itself contributes no path segment: a root holding
# pHrodo/2026-04-30_pHrodo-pKT-E-T-AT02 lands at
# flow-cytometry/pHrodo/2026-04-30_pHrodo-pKT-E-T-AT02/, matching the
# <assay>/<group>/<project> layout already in the bucket.
s3_uri_for = function(cfg, root, rel_path = "") {
    if (!nzchar(cfg$bucket)) return("")
    base = paste0("s3://", cfg$bucket, "/", cfg$prefix)
    if (nzchar(rel_path)) paste0(base, rel_path, "/") else base
}

# Which required settings are still blank. The sidebar shows these instead of
# letting a sync fail deep inside the AWS CLI.
# DATA_DIR is deliberately not checked: it belongs to the gating apps, and
# this one is perfectly usable pointed at a folder DATA_DIR knows nothing
# about. What it does need is a bucket and a working CLI.
cfg_problems = function(cfg) {
    msgs = character(0)
    if (!nzchar(cfg$bucket)) msgs = c(msgs, "UPLOAD_S3_BUCKET is not set")
    if (!nzchar(Sys.which("aws"))) msgs = c(msgs, "the aws CLI is not on PATH")
    msgs
}
