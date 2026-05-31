# Author: Kwame Okrah
# Date: 2026-03-07

gating_summary = function(gres) {
    if (length(gres) == 1) {
        if (is.na(gres)) {
            res = rep(NA, 7)
            return(res)
        }else{
            msg = "length(gres) is 1 check gating results file (K.Okrah)\n"
            cat(msg)
            stop()
        }
    }

    imat = gres[["imat"]]
    count_events = colSums(imat)

    ab_ch = gres[["ch"]]["ab+", "x_ch"]

    sel = imat[,"/intact/singlet/viable"]
    x = gres[["mat"]][,ab_ch][sel]
    x = xform(x)

    drop = x < 0
    log10_mfi = mean(x[!drop], na.rm=TRUE)
    log10_mfi_med = median(x[!drop], na.rm=TRUE)
    std_dev = sd(x[!drop], na.rm=TRUE)
    mfi_ch_neg = mean(drop)

    res = c("gMFI" = 10**log10_mfi,
            "gMFI_med" = 10**log10_mfi_med,
            "Log10 MFI" = log10_mfi,
            "Log10 MFI_med" = log10_mfi_med,
            "Std Dev" = std_dev,
            "Cells Neg" = mfi_ch_neg,
            count_events)

    return(res)
}
