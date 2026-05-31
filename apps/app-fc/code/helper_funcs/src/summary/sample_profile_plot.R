# Kwame Okrah
# 2026-03-16

sample_profile_plot = function(k, gres_list, show_ab=TRUE) {
    gres = gres_list[[k]]
    
    if (show_ab) {
        mfrow = c(1, 4)    
    }else{
        mfrow = c(1, 3)
    }
    
    op = par(mfrow     = mfrow, 
             mar       = c(4, 4, 3, 0.5), 
             cex.axis  = 1.2,
             cex.lab   = 1.2,
             cex.main  = 1.5,
             font.lab  = 2,   # bold axis labels
             font.main = 2,   # bold title
             font.axis = 2)   # bold axis tick labels  

    plot_gated_smpl(gres, "intact")
    plot_gated_smpl(gres, "singlet")
    plot_gated_smpl(gres, "viable")
    if (show_ab) {
        plot_gated_smpl(gres, "ab+")    
    }
    
    par(op)
}
