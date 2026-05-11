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
        
        neg = x < 0
        main = paste0(ch, " (N = ", sum(sel), ")\n",
                      "Neg = ", sum(neg), " | Pos = ", sum(!neg))
        
        plot(x, y, col=densCols(x, y, colramp=colramp), 
             pch=19, cex=0.35, main=main,
             xlab = x_ch, ylab = y_ch, 
            )
        abline(v=0, lty=2)
        
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
        
        if (x_ch=="VL1-A") {
            x = xform(x)
        }
        
        minx = min(x)
        minx[minx < 0] = 0
        maxx = max(x)
        xlim = c(minx, maxx)
        
        ylim = range(y)
        ylim[ylim < 0] = 0
        
        verts = gres[["verts"]][[ch]]
        
        vmax = max(verts[,2])
        if (ylim[2] < vmax) {
            ylim[2] = vmax
        }
        
        vmin = max(verts[,1])
        if (ylim[1] > vmin) {
            ylim[1] = vmin
        }

        main = paste0(ch_long, "\nN = ",
                      sum(gres[["imat"]][,ch_long]))
        
        plot(x, y, col=densCols(x, y, colramp=colramp), 
             pch=19, cex=0.35, main=main,
             xlab = x_ch, ylab = y_ch, xlim=xlim, ylim=ylim)
        
        polygon(verts, border="red", lwd=1.5)
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

