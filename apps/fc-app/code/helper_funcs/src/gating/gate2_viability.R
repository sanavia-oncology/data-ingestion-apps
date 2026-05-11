# Author: Kwame Okrah
# Date: 2026-03-07

gate2_viability = function(mat, ch, verts) {
    
    imat = data.frame("root"=rep(TRUE, nrow(mat)))
    rownames(imat) = paste0("imat_", 1:nrow(mat))
    rownames(mat) = rownames(imat)
    
    # 1. intact
    x_ch = ch["intact", "x_ch"]
    y_ch = ch["intact", "y_ch"]
    
    x = mat[,x_ch]
    y = mat[,y_ch]
    pts = cbind(x, y) 
    
    intact_verts = verts[["intact"]]
    sel = points_in_polygon(intact_verts, pts)
    
    imat = cbind(imat, "/intact"=sel)
    
    rm(list=c("x_ch", "y_ch", "x", "y", "sel", "pts"))
    
    # 2. singlet
    x_ch = ch["singlet", "x_ch"]
    y_ch = ch["singlet", "y_ch"]
    
    x = mat[,x_ch][imat[["/intact"]]]
    y = mat[,y_ch][imat[["/intact"]]]
    pts = cbind(x, y) 
    
    singlet_verts = verts[["singlet"]]
    sel = points_in_polygon(singlet_verts, pts)
    
    sel = sel[rownames(imat)]
    sel[is.na(sel)] = FALSE
    
    imat = cbind(imat, "/intact/singlet"=sel)
    
    rm(list=c("x_ch", "y_ch", "x", "y", "sel", "pts"))
    
    # 3. viable
    x_ch = ch["viable", "x_ch"]
    y_ch = ch["viable", "y_ch"]
    
    x = mat[,x_ch][imat[["/intact/singlet"]]]
    x = xform(x)
    y = mat[,y_ch][imat[["/intact/singlet"]]]
    
    pts = cbind(x, y) 
    
    viable_verts = verts[["viable"]]
    sel = points_in_polygon(viable_verts, pts)
    
    sel = sel[rownames(imat)]
    sel[is.na(sel)] = FALSE
    
    imat = cbind(imat, "/intact/singlet/viable"=sel)
    
    rm(list=c("x_ch", "y_ch", "x", "y", "sel", "pts"))
    
    # return imat and verts
    rownames(imat) = NULL
    
    verts = list(intact  = intact_verts,
                 singlet = singlet_verts,
                 viable  = viable_verts)
    
    res = list(mat=mat, imat=imat, verts=verts, ch=ch)
    
    return(res)
}


