# Kwame Okrah
# 2026-03-16

sample_profile_plot = function(k, gres_list) {
    gres = gres_list[[k]]
    
    op = par(mfrow     = c(1, 4), 
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
    plot_gated_smpl(gres, "ab+")

    par(op)
}
