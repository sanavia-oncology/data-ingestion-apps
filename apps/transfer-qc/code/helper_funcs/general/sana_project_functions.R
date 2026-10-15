SANA_PROJECTS = c("SANA01" = "SANA01", "SANA03" = "SANA03", "SANA04" = "SANA04")

probe_table = function(pinfo_sheets) {
    cols = c("Probe ID", "Probe Type", "Platename")
    d = do.call(rbind, lapply(pinfo_sheets, function(x) x[, cols]))
    agg = function(v) paste(sort(unique(v[!is.na(v) & v != ""])), collapse = ", ")
    ids = unique(d[["Probe ID"]])
    by_id = function(col) unname(sapply(ids, function(i) agg(d[d[["Probe ID"]] == i, col])))
    data.frame(
        "Probe ID"     = ids,
        "Probe Type"   = by_id("Probe Type"),
        "Platename"    = by_id("Platename"),
        "Sana project" = "",
        check.names = FALSE, stringsAsFactors = FALSE)
}

add_sana_tag = function(tags, rows, tag) {
    cur = strsplit(tags[rows], "|", fixed = TRUE)
    tags[rows] = sapply(cur, function(x) paste(sort(unique(c(x[x != ""], tag))), collapse = "|"))
    tags
}

sana_project_column = function(merged_data, choice, other = "", probe_tags = NULL) {
    n = nrow(merged_data)
    if (choice == "multiple") return(unname(probe_tags[merged_data[["Probe ID"]]]))
    if (choice == "other") return(rep(trimws(other), n))
    rep(choice, n)
}
