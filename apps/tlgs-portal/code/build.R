# TLGs data build: runs Kwame's preprocess on the gated-results sheets, writes versioned study CSVs + manifest.json.

fingerprint = function(src, dropdir) {
    rel = list.files(src, "gated-results.csv$", recursive = TRUE)
    i = file.info(c(file.path(src, rel), file.path(dropdir, "00-drop-folder", "to-drop-wells_cummulative.csv")))
    key = sort(paste(c(rel, "drop"), i$size, as.numeric(i$mtime)))
    f = tempfile()
    on.exit(unlink(f))
    writeLines(key, f)
    unname(tools::md5sum(f))
}

read_manifest = function(out) {
    f = file.path(out, "manifest.json")
    if (file.exists(f)) jsonlite::fromJSON(f, simplifyVector = FALSE)
}

build_sana = function(src, dropdir, out) {
    fp = fingerprint(src, dropdir)
    drop = file.path(dropdir, "00-drop-folder", "to-drop-wells_cummulative.csv")
    res = results_summary_table(paste0(sub("/+$", "", src), "/"))
    repeat {
        now = Sys.time()
        version = format(now, "%Y-%m-%d_%H-%M-%S", tz = "UTC")
        if (!dir.exists(file.path(out, version))) break
        Sys.sleep(1)
    }
    date = substr(version, 1, 10)
    studies = setNames(list(), character(0))
    summary = NULL
    projects = wells_in = dropped = 0
    excluded = character(0)
    dir.create(out, showWarnings = FALSE, recursive = TRUE)
    if (is.list(res)) {
        key = vapply(res$sheets, function(s) paste0(s$project_group[1], "/", s$project_name[1]), "")
        tab = res$summary_table
        mtime = format(file.mtime(names(key)), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        tab$uploaded_at = mtime[match(paste0(tab$project_group, "/", tab$project_name), key)]
        need = c("Platename", "Plate Position", "Probe ID", "probe_name", "target_spec_name",
                 "experiment_type", "dosing_schedule", "probe_conc", "gMFI", "sana_project")
        has = vapply(res$sheets, function(s) all(need %in% names(s)), TRUE)
        excluded = unname(key[!has])
        cols = Reduce(union, lapply(res$sheets[has], names))
        ok = lapply(res$sheets[has], function(s) { s[setdiff(cols, names(s))] = NA; s[cols] })
        wells = unlist(lapply(ok, function(s) paste0(s$Platename, "|", s$`Plate Position`)))
        wells_in = length(wells)
        dropped = sum(wells %in% if (file.exists(drop)) with(read.csv(drop), paste0(Platename, "|", well)))
        proj_dbs = list()
        if (length(ok)) proj_dbs = get_sana_projects(ok, path_to_drop = if (file.exists(drop)) drop else NULL)
        if (!dir.create(file.path(out, version))) stop("cannot create ", file.path(out, version))
        for (p in names(proj_dbs)) {
            f = file.path(version, paste0(p, "_cumulative-v", date, ".csv"))
            write.csv(proj_dbs[[p]], file = file.path(out, f), row.names = FALSE)
            studies[[p]] = list(file = f, wells = nrow(proj_dbs[[p]]))
        }
        summary = file.path(version, paste0("summary-table_cumulative-v", date, ".csv"))
        write.csv(tab, file = file.path(out, summary), row.names = FALSE)
        projects = nrow(tab)
    }
    m = list(version = version, generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
             status = if (is.list(res)) "ok" else "empty", studies = studies, summary = summary,
             projects = projects, wells_in = wells_in, dropped = dropped, drop_file = file.exists(drop),
             excluded = I(excluded), fingerprint = fp)
    mf = file.path(out, "manifest.json")
    prev = read_manifest(out)$version
    jsonlite::write_json(m, paste0(mf, ".tmp"), auto_unbox = TRUE, pretty = TRUE, null = "null")
    if (!file.rename(paste0(mf, ".tmp"), mf)) stop("cannot write ", mf)
    old = setdiff(list.files(out, "^\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}$"), c(version, prev))
    unlink(file.path(out, old), recursive = TRUE)
    invisible(m)
}
