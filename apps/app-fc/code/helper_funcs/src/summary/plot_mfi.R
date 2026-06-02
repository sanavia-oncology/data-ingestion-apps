# Kwame Okrah
# 2026-03-14

plot_mfi = function(results_table, fig_path=NULL) {
    csel = "Log10 MFI"

    pinfos_by_platename = split(results_table, results_table$Platename)
    tmax = max(results_table[,csel], na.rm=TRUE)
    tmin = min(results_table[,csel], na.rm=TRUE)
    
    if (!is.null(fig_path)) {
        pdf(fig_path, height = 2.85, width = 6.85)
    }
    
    op = par(mar=c(3.25, 3.25, 1.25, 0.75), 
             mgp=c(2, 0.75, 0),
             mfrow=c(1, 1))
    
    for (pl in names(pinfos_by_platename)) {
        plate_df = pinfos_by_platename[[pl]]
        
        y = plate_df[,csel]

        x = plate_df$"Plate Position"
        
        nr = 8; nc = 12
        lvls = paste0(rep(LETTERS[1:nr], each=nc), 1:nc)
        x = factor(x, levels = lvls)

        names(y) = x

        y = y[levels(x)]
        names(y) = levels(x)

        ylim = c(tmin, tmax)
        
        plot(0, 0, pch="", xlim=c(0, 97), ylim=ylim,
             xaxs="i", xaxt="n", xlab="Well ID", 
             ylab=csel,
             main=pl)
        axis(1, at=1:length(y), names(y), cex.axis=0.9)
        abline(v=seq(12, 84, by=12) + 0.5, col="gray80")
        points(y, pch=19, col="steelblue", cex=0.65)
        
    }
    
    par(op)
    
    if (!is.null(fig_path)) {
        dev.off()
    }
}
