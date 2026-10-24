read_dict = function(path) {
    d = read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    kind = names(Filter(function(k) k$name_col %in% names(d), NAME_KINDS))
    if (length(kind) != 1 || !"colloquial_id" %in% names(d)) {
        stop("The dictionary needs colloquial_id and one of ",
             paste(vapply(NAME_KINDS, `[[`, "", "name_col"), collapse = " or "))
    }
    d = data.frame(id = trimws(d[["colloquial_id"]]), alias = trimws(d[[NAME_KINDS[[kind]]$name_col]]),
                   stringsAsFactors = FALSE)
    list(kind = kind, dict = d[nzchar(d$id) & nzchar(d$alias) & !duplicated(d$id), ])
}

NAME_KINDS = list(
    probe = list(column = "Probe ID", name = "Probe name", title = "Match probe names",
                 name_col = "probe_name", dict = "probe dictionary"),
    target = list(column = "Target Spec ID", name = "Target spec name", title = "Match target specimen names",
                  name_col = "target_spec_name", dict = "target specimen dictionary")
)

name_table = function(ids, dict) {
    ids = trimws(ids)
    u = sort(unique(ids))
    alias = unname(setNames(dict$alias, dict$id)[u])
    d = data.frame(id = u, wells = tabulate(match(ids, u), length(u)),
                   name = ifelse(is.na(alias), "", alias), auto = !is.na(alias),
                   stringsAsFactors = FALSE)
    d = d[order(d$auto, d$id), ]
    rownames(d) = NULL
    d
}

name_display = function(tbl, kind) {
    setNames(tbl[, c("id", "wells", "name")], c(kind$column, "Wells", kind$name))
}

assembly_type = function(alias) sub("\\|.*$", "", alias)

dict_choices = function(dict) setNames(dict$id, dict$alias)

DICT_RENDER = I('{
    option: function(item, escape) {
        var sub = item.value === item.label ? "" :
            "<div class=\\"text-secondary\\" style=\\"font-size: 12px;\\">" + escape(item.value) + "</div>";
        return "<div class=\\"option\\">" + escape(item.label) + sub + "</div>";
    },
    item: function(item, escape) { return "<div class=\\"item\\">" + escape(item.label) + "</div>"; }
}')
