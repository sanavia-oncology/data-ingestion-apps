# Find the projects inside each registered sync root.
#
# A project is any directory holding at least one of the four folders the
# ingestion apps write. Unlike the sibling apps' get_paths_by_project2(), this
# one does not require a QC report: the upload app has to show a project that
# was picked up but never QC'd, otherwise it looks like the folder was
# silently dropped.
#
# The columns match front_page_table2() in app-fc / transfer-qc so the table
# reads the same as the front page of every other app.

PROJECT_MARKER_DIRS = c("plate_information_sheets", "assay_data",
                        "qc_report", "gating_results")

# What the table shows, in order. path/root ride along for the server.
# Project Group still rides along in the data - the manifest keys on it, so
# two groups can hold a project of the same name - but it is not shown.
PROJECT_TABLE_COLS = c("Project Name", "Status")

discover_projects = function(roots) {
    empty = data.frame(
        "Date Created" = as.Date(character(0)),
        "Project Group" = character(0),
        "Project Name" = character(0),
        path = character(0), root = character(0),
        check.names = FALSE, stringsAsFactors = FALSE
    )
    roots = roots[dir.exists(roots)]
    if (length(roots) == 0) return(empty)

    rows = list()
    for (root in roots) {
        root = sub("/$", "", normalizePath(root))
        # One pass over the tree, as the sibling apps do; every column below is
        # derived from this vector rather than re-walking the disk.
        all_files = list.files(root, recursive = TRUE, full.names = TRUE)
        if (length(all_files) == 0) next

        rel = substring(all_files, nchar(root) + 2)
        parts = strsplit(rel, "/", fixed = TRUE)

        # Where the marker folder sits in the relative path tells us where the
        # project directory ends.
        marker_at = vapply(parts, function(p) {
            hit = which(p %in% PROJECT_MARKER_DIRS)
            if (length(hit) == 0) 0L else hit[1]
        }, integer(1))

        keep = marker_at > 1L
        if (!any(keep)) next

        idx = which(keep)
        proj_rel = vapply(idx, function(i) {
            paste(parts[[i]][seq_len(marker_at[i] - 1L)], collapse = "/")
        }, character(1))
        files_kept = all_files[idx]

        for (pr in unique(proj_rel)) {
            proj_files = files_kept[proj_rel == pr]
            segs = strsplit(pr, "/", fixed = TRUE)[[1]]

            # A project directly under the root has no group folder of its
            # own; the root's own name is the group in that case.
            group = if (length(segs) > 1) paste(segs[-length(segs)], collapse = "/") else basename(root)
            name = segs[length(segs)]

            mtimes = file.info(proj_files)$mtime

            rows[[paste(root, pr, sep = "::")]] = data.frame(
                "Date Created"  = as.Date(min(mtimes, na.rm = TRUE)),
                "Project Group" = group,
                "Project Name"  = name,
                path            = file.path(root, pr),
                root            = root,
                check.names = FALSE, stringsAsFactors = FALSE
            )
        }
    }

    if (length(rows) == 0) return(empty)
    out = do.call(rbind, rows)
    rownames(out) = NULL
    out = out[order(out[["Date Created"]], decreasing = TRUE), , drop = FALSE]
    rownames(out) = NULL
    out
}
