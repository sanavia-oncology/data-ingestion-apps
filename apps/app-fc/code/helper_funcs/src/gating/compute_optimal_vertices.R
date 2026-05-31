# Kwame Okrah
# 2026-03-14

compute_optimal_vertices = function(fcs_files, 
                                    default_ch,
                                    verbose = FALSE) {
    if (verbose) {
        cat("computing vertices...\n")
    }
    
    verts_hold = list()
    mat_hold = list()

    for (k in names(fcs_files)) {
        fcs = fcs_files[[k]]
        mat = flowCore::exprs(fcs)
        auto_gres = auto_gate2_viability(mat, default_ch)
        verts_hold[[k]] = auto_gres[["verts"]]
        mat_hold[[k]] = mat
    }
    
    optim_verts_list = list()
        
    for (pop in c("intact", "singlet", "viable")) {
        xp = sapply(verts_hold, function(x) x[[pop]][,"x"])
        yp = sapply(verts_hold, function(x) x[[pop]][,"y"])
        
        if (pop=="intact") {
            A = c(quantile(xp["A",], 0.75), quantile(yp["A",], 0.75))
            B = c(quantile(xp["B",], 0.99), A[2])
            if (A[1] > 2e+05) {
                A[1] = 2e+05
            }
            if (A[2] > 1e+05) {
                A[2] = 1e+05
                B[2] = A[2]
            }
            if (B[1] < 8e+05) {
                B[1] = 8e+05
            }
            C = c(B[1], quantile(yp["C",], 0.99))
            if (C[2] < 8e+05) {
                C[2] = 8e+05
            }
            D = c(A[1], C[2])
        }

        if (pop=="singlet") {
            A = c(quantile(xp["A",], 0.0), quantile(yp["A",], 0.0))
            B = c(quantile(xp["B",], 0.95), quantile(yp["B",], 0.05))
            C = c(B[1], quantile(yp["C",], 0.99))
            D = c(A[1], quantile(yp["D",], 0.99))
        }

        if (pop=="viable") {
            A = c(min(xp["A",]), 0)
            B = c(quantile(xp["B",], 0.25), A[2])
            if (B[1] > 3.5) {
                B[1] = 3.5
            }
            C = c(B[1], 8e+05)
            D = c(A[1], C[2])   
        }
        
        optim_verts = rbind(A, B, C, D)
        colnames(optim_verts) = c("x", "y")
        
        optim_verts_list[[pop]] = optim_verts
    }
    
    omat = do.call(rbind, mat_hold)
    rownames(omat) = NULL

    nr = nrow(omat)
    if (nr > 10000) {
        nsel = sample(1:nr, 10000, replace = FALSE)
        omat = omat[nsel,]
    }

    res = list(optim_verts_list = optim_verts_list,
               verts_hold = verts_hold,
               default_ch = default_ch,
               omat = omat)
    
    if (verbose) {
        cat("done\n")
    }
    
    return(res)
}