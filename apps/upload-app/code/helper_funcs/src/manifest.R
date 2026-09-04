# The publish manifest - the state file the web app reads.
#
# One CSV, one row per project, written at the top of the synced folder so the
# same `aws s3 sync` that carries the data carries it too. It lands at
# s3://<bucket>/<prefix>upload-manifest.csv - one object, one GET for the
# reader, and no second upload step here.
#
# A project with no row is treated as display=no. Nothing is published until
# somebody presses Add, and Remove flips the flag rather than deleting the
# row, so the web app can tell "withdrawn" from "never submitted".

MANIFEST_COLS = c("project_group", "project_name", "s3_uri",
                  "display", "updated_at", "updated_by")

empty_manifest = function() {
    as.data.frame(
        stats::setNames(replicate(length(MANIFEST_COLS), character(0), simplify = FALSE),
                        MANIFEST_COLS),
        stringsAsFactors = FALSE
    )
}

# The manifest lives in the first registered folder; with none, there is
# nowhere to put it and nothing to publish.
manifest_path = function(cfg, folders) {
    if (length(folders) == 0) return("")
    file.path(folders[1], cfg$manifest_name)
}

read_manifest = function(path) {
    if (!nzchar(path) || !file.exists(path)) return(empty_manifest())
    df = tryCatch(
        utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
        error = function(e) empty_manifest()
    )
    for (col in setdiff(MANIFEST_COLS, names(df))) df[[col]] = character(nrow(df))
    df[, MANIFEST_COLS, drop = FALSE]
}

# Atomic: the launchd sync may read this file at any moment, and a half
# written manifest published to S3 would hide live projects from the web.
write_manifest = function(path, df) {
    if (!nzchar(path)) stop("no folder chosen, so there is nowhere to write the manifest")
    tmp = paste0(path, ".tmp")
    utils::write.csv(df[, MANIFEST_COLS, drop = FALSE], tmp, row.names = FALSE)
    if (!file.rename(tmp, path)) {
        unlink(tmp)
        stop("could not replace ", path)
    }
    invisible(df)
}

manifest_key = function(group, name) paste(group, name, sep = "\r")

# Upsert the given projects to display=yes / display=no.
set_display = function(cfg, path, projects, display) {
    stopifnot(display %in% c("yes", "no"))
    if (is.null(projects) || nrow(projects) == 0) return(read_manifest(path))

    df = read_manifest(path)
    now = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    who = Sys.info()[["user"]]

    for (i in seq_len(nrow(projects))) {
        p = projects[i, ]
        # substring, not sub(): real project folders carry spaces, "+" and
        # trailing whitespace, so treating the root as a regex would mangle
        # or silently fail to strip the prefix.
        rel = substring(p[["path"]], nchar(p[["root"]]) + 2L)
        row = data.frame(
            project_group = p[["Project Group"]],
            project_name  = p[["Project Name"]],
            s3_uri        = s3_uri_for(cfg, p[["root"]], rel),
            display       = display,
            updated_at    = now,
            updated_by    = who,
            stringsAsFactors = FALSE
        )
        hit = manifest_key(df$project_group, df$project_name) ==
              manifest_key(row$project_group, row$project_name)
        if (any(hit)) df[which(hit)[1], ] = row else df = rbind(df, row)
    }

    df = df[order(df$project_group, df$project_name), , drop = FALSE]
    rownames(df) = NULL
    write_manifest(path, df)
    df
}

# The Status column. Three states, and the distinction matters: a project
# nobody has decided on yet is Waiting, not Removed - reading "Removed" for
# something that was never submitted is simply wrong.
#
#   no row in the manifest -> Waiting
#   display = yes          -> Added
#   display = no           -> Removed   (submitted, then withdrawn)
#
# The CSV itself still stores display=yes/no, which is the question the web
# app actually asks: show this or not. Waiting and Removed both mean no.
status_flags = function(manifest, projects) {
    if (nrow(projects) == 0) return(character(0))
    idx = match(manifest_key(projects[["Project Group"]], projects[["Project Name"]]),
                manifest_key(manifest$project_group, manifest$project_name))
    flags = manifest$display[idx]
    ifelse(is.na(flags), "Waiting", ifelse(flags == "yes", "Added", "Removed"))
}
