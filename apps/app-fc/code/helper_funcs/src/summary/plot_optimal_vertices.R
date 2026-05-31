# Kwame Okrah
# 2026-03-14

plot_optimal_vertices = function(optimal_vertices_res, 
                                 alpha.f=0.25, 
                                 fig_path=NULL) {
    
    colramp = colorRampPalette(c("#00007F", "blue", "#007FFF", 
                                 "cyan", "#7FFF7F", "yellow", 
                                 "#FF7F00", "red", "#7F0000"))
    
    optim_verts_list = optimal_vertices_res$optim_verts_list
    default_ch = optimal_vertices_res$default_ch
    verts_hold = optimal_vertices_res$verts_hold
    omat = optimal_vertices_res$omat
    
    if (!is.null(fig_path)) {
        pdf(fig_path, height = 3, width = 9)
    }
    
    op = par(mfrow=c(1, 3), cex.axis=1.2, cex.lab=1.2, cex.main=2)
    
    for (pop in c("intact", "singlet", "viable")) {
        
        xp = sapply(verts_hold, function(x) x[[pop]][,"x"])
        yp = sapply(verts_hold, function(x) x[[pop]][,"y"])

        x = omat[,default_ch[pop,"x_ch"]]
        y = omat[,default_ch[pop,"y_ch"]]
        
        if (default_ch[pop,"x_ch"] %in% c("VL1-A", 
                                          "RL1-A", 
                                          "VL1-H", 
                                          "RL1-H")) {
            x = xform(x)    
        }

        xlim = range(x)
        ylim = range(y)
        
        if (pop %in% c("viable", "intact")) {
            xlim[1] = 0
            ylim[1] = 0
        }

        plot(x, y, 
             pch=19, cex=0.35,
             col=densCols(x, y, colramp=colramp),
             xlab = default_ch[pop,"x_ch"],
             ylab = default_ch[pop,"y_ch"],
             main = paste0(pop, " (not gated)"),
             xlim=xlim, ylim=ylim,
             font.lab  = 2,   # bold axis labels
             font.main = 2,   # bold title
             font.axis = 2    # bold axis tick labels  
        )
        
        for (i in 1:length(verts_hold)) {
            polygon(verts_hold[[i]][[pop]], 
                    border=adjustcolor("red", alpha.f = alpha.f))    
        }
        
        optim_verts = optim_verts_list[[pop]]
        polygon(optim_verts, border = "red", lwd=2)
    }
    
    par(op)
    
    if (!is.null(fig_path)) {
        dev.off()
    }
}