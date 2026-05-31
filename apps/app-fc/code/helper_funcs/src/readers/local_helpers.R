# Local-mode project discovery.
#
# /data is bind-mounted into the app-fc container at launch, holding the
# user's parent project folder. Discovery is driven entirely by what is on
# disk in each project — no DB, no `latest.json`, no API round-trip. A
# project is "ready for gating" iff QC has produced its primary output:
#
#     <project>/qc_report/<date>-merged-data.csv
#
# That CSV is written by checker for both FCS and FRD assays at finalize
# time. If the API also wrote `qc_report/qc_manifest-*.json` alongside, we
# layer that in for richer table fields (real submission_id, qc_author).
# If the manifest is missing — e.g. legacy QC runs from before the manifest
# existed — the project still surfaces; missing fields fall back to
# filesystem inspection so the user can still gate it.
#
# Container restarts cannot lose state because none of this lives inside a
# container.

# Prune list mirrors apps/checker/server.R's project picker so a parent like
# ~/workbench loaded with node_modules / .git / venv trees doesn't blow up
# the walk. Keep these in sync.
.PRUNE_DIRS = c(".*", "node_modules", ".git", ".venv", "venv",
                "__pycache__", "renv", "target", "dist", "build")

# Depth budget: checker's picker uses -maxdepth 6 for the
# plate_information_sheets directory; the merged-data.csv sits inside
# qc_report/, so 7 covers the same project-nesting reach.
.QC_MAXDEPTH = 7

# Returns absolute paths to project directories that have a finished QC,
# detected by the presence of `qc_report/*-merged-data.csv` (the universal
# QC output for both FCS and FRD assays).
.find_qc_projects = function(data_root) {
    if (!dir.exists(data_root)) return(character(0))
    LP = shQuote("(")
    RP = shQuote(")")
    prune = character(0)
    for (i in seq_along(.PRUNE_DIRS)) {
        prune = c(prune, "-name", shQuote(.PRUNE_DIRS[i]))
        if (i < length(.PRUNE_DIRS)) prune = c(prune, "-o")
    }
    cmd_args = c(
        shQuote(data_root),
        "-maxdepth", as.character(.QC_MAXDEPTH),
        LP, "-type", "d", LP, prune, RP, "-prune", RP,
        "-o",
        LP, "-type", "f",
        "-name", shQuote("*-merged-data.csv"),
        "-path", shQuote("*/qc_report/*"),
        "-print", RP
    )
    paths = tryCatch(
        system2("find", cmd_args, stdout = TRUE, stderr = FALSE),
        error = function(e) character(0)
    )
    # qc_report/<date>-merged-data.csv  →  project dir (parent of qc_report)
    unique(vapply(paths, function(p) dirname(dirname(p)), character(1)))
}

.coalesce = function(x, default) {
    if (is.null(x) || length(x) == 0) default else x
}

# Newest qc_manifest in qc_report/, or NULL if none. Filenames are ISO-Z
# prefixed → lexicographic max == chronological max.
.read_latest_manifest = function(qc_dir) {
    matches = Sys.glob(file.path(qc_dir, "qc_manifest-*.json"))
    if (length(matches) == 0) return(NULL)
    latest = sort(matches, decreasing = TRUE)[1]
    tryCatch(
        jsonlite::fromJSON(latest, simplifyVector = TRUE),
        error = function(e) NULL
    )
}

# Build one row of the projects table from a project directory. Manifest
# fields take priority; filesystem-derived defaults fill the gaps so legacy
# projects (QC done before the manifest existed) still appear.
.build_project_row = function(project_dir, data_root) {
    qc_dir = file.path(project_dir, "qc_report")
    m = .read_latest_manifest(qc_dir)

    project_relpath = sub(paste0("^", data_root, "/?"), "", project_dir)
    if (!nzchar(project_relpath)) project_relpath = basename(project_dir)

    # Assay: FCS is detected by the mfi-channel.csv that checker writes for
    # FCS uploads only. FRD has no MFI.
    mfi_csv = Sys.glob(file.path(qc_dir, "*-mfi-channel.csv"))
    is_fcs = length(mfi_csv) > 0
    fs_assay = if (is_fcs) "fcs" else ""
    fs_mfi = ""
    if (is_fcs) {
        chan = tryCatch(
            utils::read.csv(mfi_csv[1], stringsAsFactors = FALSE),
            error = function(e) NULL
        )
        if (!is.null(chan) && "mfi_channel" %in% names(chan) && nrow(chan) > 0) {
            fs_mfi = as.character(chan$mfi_channel[1])
        }
    }

    sid = .coalesce(m[["submission_id"]], "")
    project_name = .coalesce(m[["project_name"]], basename(project_dir))
    qc_author = .coalesce(m[["qc_author"]][["display_name"]], "")
    assay_type = .coalesce(m[["assay_type"]], fs_assay)
    mfi_channel = .coalesce(m[["mfi_channel"]], fs_mfi)

    merged = Sys.glob(file.path(qc_dir, "*-merged-data.csv"))

    # Creator from the plate info sheet (lab user who ran the experiment) —
    # distinct from qc_author (who ran QC). Read just the Creator column with
    # colClasses="NULL" elsewhere so we don't pull the whole merged CSV into
    # memory. Multiple plates can have different Creators → unique + joined.
    creator = ""
    if (length(merged) > 0) {
        hdr = tryCatch(
            utils::read.csv(merged[1], nrows = 1, stringsAsFactors = FALSE),
            error = function(e) NULL
        )
        if (!is.null(hdr) && "Creator" %in% names(hdr)) {
            cls = rep("NULL", length(names(hdr)))
            cls[match("Creator", names(hdr))] = "character"
            md = tryCatch(
                utils::read.csv(merged[1], colClasses = cls,
                                stringsAsFactors = FALSE),
                error = function(e) NULL
            )
            if (!is.null(md) && "Creator" %in% names(md) && nrow(md) > 0) {
                vals = unique(md$Creator[!is.na(md$Creator) & nzchar(md$Creator)])
                creator = paste(vals, collapse = ", ")
            }
        }
    }

    # Timestamp: manifest submitted_at when present, else the merged-data
    # CSV's mtime (the file is the QC artifact). Held as POSIXct in UTC so
    # sort is time-precise; rendered to "YYYY-MM-DD HH:MM" for the table
    # (ISO format → lexicographic sort matches chronological).
    ts = as.POSIXct(NA, tz = "UTC")
    if (!is.null(m[["submitted_at"]]) && nzchar(m[["submitted_at"]])) {
        ts = suppressWarnings(as.POSIXct(m[["submitted_at"]],
                                          format = "%Y-%m-%dT%H:%M:%OS",
                                          tz = "UTC"))
    }
    if (is.na(ts) && length(merged) > 0) {
        ts = as.POSIXct(file.info(merged[1])$mtime, tz = "UTC")
    }
    date_display = if (!is.na(ts)) format(ts, "%Y-%m-%d %H:%M", tz = "UTC") else ""
    date_str = if (!is.na(ts)) format(ts, "%Y-%m-%d", tz = "UTC") else "????-??-??"

    id_display = if (nzchar(sid)) {
        paste(date_str, substr(sid, 1, 8))
    } else {
        paste(date_str, "(legacy)")
    }

    # Gating done iff a gated CSV exists. The analysis manifest is
    # provenance, not the readiness indicator.
    has_analysis = length(Sys.glob(file.path(project_dir, "gating_results",
                                              "*-gated-results.csv"))) > 0

    n_fcs = length(list.files(project_dir, pattern = "\\.fcs$",
                              recursive = TRUE, ignore.case = TRUE))

    data.frame(
        Date = date_display,
        ID = id_display,
        `Project Name` = project_name,
        Creator = creator,
        qc_author = qc_author,
        `FCS files` = n_fcs,
        submission_id = sid,
        project_relpath = project_relpath,
        assay_type = assay_type,
        mfi_channel = mfi_channel,
        has_analysis = has_analysis,
        check.names = FALSE, stringsAsFactors = FALSE
    )
}

# Returns the projects data.frame the FC table consumes:
#   Date, ID, Project Name, Creator, qc_author, FCS files, submission_id,
#   project_relpath, assay_type, mfi_channel, has_analysis
# Empty result is a valid no-projects state, not an error.
fetch_submissions_local = function(data_root = "/data") {
    empty = data.frame(
        Date = character(0),
        ID = character(0),
        `Project Name` = character(0),
        Creator = character(0),
        qc_author = character(0),
        `FCS files` = integer(0),
        submission_id = character(0),
        project_relpath = character(0),
        assay_type = character(0),
        mfi_channel = character(0),
        has_analysis = logical(0),
        check.names = FALSE, stringsAsFactors = FALSE
    )

    project_dirs = .find_qc_projects(data_root)
    if (length(project_dirs) == 0) return(empty)

    rows = lapply(project_dirs, .build_project_row, data_root = data_root)
    rows = Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(empty)
    out = do.call(rbind, rows)
    out[order(out$Date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
