SANA_PROJECTS = c("SANA01" = "SANA01", "SANA03" = "SANA03", "SANA04" = "SANA04")

well_table = function(pinfo_sheets) {
    cols = c("Platename", "Plate Position", "Target Spec ID", "Probe ID",
             "Probe Quant Type", "Probe Quant Value", "Primary Role")
    d = do.call(rbind, lapply(pinfo_sheets, function(x) x[, cols]))
    rownames(d) = NULL
    d[["Sana Project"]] = ""
    d
}

well_key = function(d) paste(d[["Platename"]], d[["Plate Position"]], sep = "|")

well_constants = function(d) {
    cols = setdiff(names(d), c("Plate Position", "Sana Project"))
    cols = cols[vapply(cols, function(k) length(unique(d[[k]])) == 1, logical(1))]
    vapply(cols, function(k) as.character(d[[k]][1]), character(1))
}

well_display = function(d) d[, setdiff(names(d), names(well_constants(d))), drop = FALSE]

add_sana_tag = function(tags, rows, tag) {
    cur = strsplit(tags[rows], "|", fixed = TRUE)
    tags[rows] = sapply(cur, function(x) paste(sort(unique(c(x[x != ""], tag))), collapse = "|"))
    tags
}

sana_project_column = function(merged_data, choice, other = "", well_tags = NULL) {
    n = nrow(merged_data)
    if (choice == "multiple") return(unname(well_tags[well_key(merged_data)]))
    if (choice == "other") return(rep(trimws(other), n))
    rep(choice, n)
}
