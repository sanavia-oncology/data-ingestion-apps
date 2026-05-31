# Author: Kwame Okrah
# Date: 2026-03-07

plot_gated_smpl = function(gres, ch = "intact") {
    
    colramp = colorRampPalette(c("#00007F", "blue", "#007FFF", 
                                 "cyan", "#7FFF7F", "yellow", 
                                 "#FF7F00", "red", "#7F0000"))
    
    x_ch = gres[["ch"]][ch,"x_ch"]
    y_ch = gres[["ch"]][ch,"y_ch"]
    
    if (ch=="ab+") {
        sel = gres[["imat"]][,"/intact/singlet/viable"]
        x = gres[["mat"]][,x_ch][sel]
        y = gres[["mat"]][,y_ch][sel]
        x = xform(x)
        
        drop = x < 0
        log10_mfi = round(mean(x[!drop], na.rm=TRUE), 2)
        
        main = paste0(ch, " (N = ", sum(sel), ")\n",
                      "Log10 MFI = ", log10_mfi)
        
        plot(x, y, col=densCols(x, y, colramp=colramp), 
             pch=19, cex=0.35, main=main,
             xlab = x_ch, ylab = y_ch,
        )
        abline(v=0:6, lty=3, col="gray")
        abline(h=(1:10)*100000, lty=3, col="gray")
        
        
    }else{
        if (ch=="intact") {
            ch_long = "/intact"
            parent_sel = gres[["imat"]][,"root"]
        }
        
        if (ch=="singlet") {
            ch_long = "/intact/singlet"  
            parent_sel = gres[["imat"]][,"/intact"]
        }
        
        if (ch=="viable") {
            ch_long = "/intact/singlet/viable"
            parent_sel = gres[["imat"]][,"/intact/singlet"]
        }
        
        x = gres[["mat"]][,x_ch][parent_sel]
        y = gres[["mat"]][,y_ch][parent_sel]
        
        if (x_ch %in% c("VL1-A", "VL1-H")) {
            x = xform(x)
        }
        
        
        ylim=c(min(y), max(y))
        xlim = c(min(x), max(x))
        
        if (ylim[1] < 0) ylim[1] = 0
        if (xlim[1] < 0) xlim[1] = 0
        
        verts = gres[["verts"]][[ch]]
        
        main = paste0(ch_long, "\nN = ",
                      sum(gres[["imat"]][,ch_long]))
        
        plot(x, y, col=densCols(x, y, colramp=colramp), 
             pch=19, cex=0.35, main=main,
             xlab = x_ch, ylab = y_ch, xlim=xlim, ylim=ylim)
        
        abline(h=(1:10)*100000, lty=3, col="gray")
        
        if (ch=="viable") {
            abline(v=0:6, lty=3, col="gray")
            abline(v=verts["B","x"], col="red", lwd=1.5)
        }else{
            abline(v=(1:10)*100000, lty=3, col="gray")
            polygon(verts, border="red", lwd=1.5)    
        }
        
    }
    
}

ab_plus_density = function(gres) {
    ch = "ab+"
    x_ch = gres[["ch"]][ch,"x_ch"]
    
    sel = gres[["imat"]][,"/intact/singlet/viable"]
    x = gres[["mat"]][,x_ch][sel]
    x = xform(x)
    
    main = paste0("Log10_MFI = ", sprintf("%0.3f", mean(x[x > 0], na.rm=T)), 
                  " | SD = ", round(sd(x[x > 0]), 2),
                  "\nN = ", sum(sel))
    
    plot(density(x), main=main, xlim=c(0, 6), xlab=x_ch, 
         ylab="Density")
}

