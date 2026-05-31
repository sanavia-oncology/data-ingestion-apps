# Kwame Okrah
# 2026-03-08

make_results_table = function(gres_list, pinfos, author_gating = "unknown") {
    hold = list()
    for (k in names(gres_list)) {
        gres = gres_list[[k]]
        hold[[k]] = gating_summary(gres)
    }
    stat_summ = do.call(rbind, hold)

    rn = rownames(stat_summ)
    rnl = strsplit(rn, "\\|")

    PLATENAME = sapply(rnl, "[[", 1)
    PLATE_POSITION = sapply(rnl, "[[", 2)

    assay_results_sheet = data.frame("Platename"=PLATENAME,
                                     "Plate Position"=PLATE_POSITION,
                                     "author_gating"=author_gating,
                                     "date_gating"=Sys.Date(),
                                     check.names = FALSE)

    res = cbind(assay_results_sheet, stat_summ)
    res = res[rownames(pinfos),]
    res = cbind(pinfos, res)

    return(res)
}