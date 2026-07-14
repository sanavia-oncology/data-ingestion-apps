library(grid)

clean_dat = function(dat, probe_dict, target_spec_dict) {
    probe_alias = probe_dict[["probe_alias"]]
    names(probe_alias) = probe_dict[["probe_id"]]
    probe_alias_xpand = probe_alias[dat[["Probe ID"]]]
    sel = probe_alias_xpand=="no_alias"
    probe_alias_xpand[sel] = dat[["Probe ID"]][sel]
    
    target_spec_alias = target_spec_dict[["target_spec_alias"]]
    names(target_spec_alias) = target_spec_dict[["target_spec_id"]]
    target_spec_alias_xpand = target_spec_alias[dat[["Target Spec ID"]]]
    sel = target_spec_alias_xpand=="no_alias"
    target_spec_alias_xpand[sel] = dat[["Target Spec ID"]][sel]
    
    dat$probe_name = probe_alias_xpand
    dat$target_spec_name = target_spec_alias_xpand
    
    to_keep = grep("^Keep", dat[["to_drop"]])
    dat = dat[to_keep,,drop=F]
    
    rownames(dat) = NULL
    
    return(dat)
}

prep_single_dose_data = function(project_data) {
    res = split(project_data, project_data$"Probe Quant Value")
    return(res)
}

prep_multi_dose_data  = function(dat) {
    res = split(project_data, project_data$"target_spec_name")
    return(res)
}

ncut_ti = 40

titration_hmap = function(dat, 
                          cell="cell", 
                          cluster_rows=FALSE, 
                          ncut=ncut_ti) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    max_mfi = 100000; N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    # data
    mfi = dat[["gMFI"]]
    probe_id = dat[["probe_name"]]
    probe_conc = dat[["Probe Quant Value"]]
    MAT = tapply(mfi, list(probe_id, probe_conc), mean, na.rm=T)
    
    # split heatmap matrix
    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    for (i in 1:lm_) {
        main = paste0(cell,
                      "\nProbe x Conc (ug/ml) Table (Results: MFI)")
        
        mat = mat_list[[i]]
        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           angle_col=0,
                           breaks=breaks,
                           color=colors,
                           border_color=border_color,
                           display_numbers=round(mat),
                           number_color=number_color,
                           main=main,
                           legend=legend)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }

}

titration_sample_size = function(dat, 
                                 cell="cell", 
                                 cluster_rows=FALSE, 
                                 ncut=ncut_ti) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"

    # data
    mfi = dat[["gMFI"]]
    probe_id = dat[["probe_name"]]
    probe_conc = dat[["Probe Quant Value"]]
    MAT = tapply(mfi, list(probe_id, probe_conc), length)

    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    
    for (i in 1:lm_) {
        main = paste0(cell,
                      "\nProbe x Conc (ug/ml) Table (Results: Sample Size)")
        
        mat = mat_list[[i]]
        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           angle_col=0,
                           breaks=c(1, 2),
                           color=c("aliceblue"),
                           border_color=border_color,
                           display_numbers=round(mat),
                           number_color=number_color,
                           main=main,
                           legend=FALSE)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
        
        msg = "Values of cells w/ sample size > 1 are averaged."
        grid.text(msg, x=0.5, y=0.02, gp=gpar(fontsize=10))
    }
    
}

ncut_sd = 38

single_dose_hmap1 = function(dat,
                             cluster_rows=FALSE, 
                             ncut=ncut_sd) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    max_mfi = 100000; N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["gMFI"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), mean, na.rm=TRUE)
    
    probe_conc = dat[["Probe Quant Value"]]             
    
    # split heatmap matrix
    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    for (i in 1:lm_) {
        main = paste0("Probe Conc: ", probe_conc[1], " (ug/ml)",
                      "\nProbe x Target Spec (Results: MFI)")
        
        mat = mat_list[[i]]
        
        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           breaks=breaks,
                           color=colors,
                           border_color=border_color,
                           display_numbers=round(mat),
                           number_color=number_color,
                           main=main,
                           legend=legend)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }
    
}

single_dose_sample_size1 = function(dat,
                                    cluster_rows=FALSE, 
                                    ncut=ncut_sd) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["gMFI"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), length)
    
    probe_conc = dat[["Probe Quant Value"]]             
    
    # split heatmap matrix
    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    
    for (i in 1:lm_) {
        main = paste0("Probe Conc: ", probe_conc[1], " (ug/ml)",
                      "\nProbe x Target Spec (Results: Sample Size)")
        
        mat = mat_list[[i]]
        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           breaks=c(1, 2),
                           color=c("aliceblue"),
                           border_color=border_color,
                           display_numbers=round(mat),
                           number_color=number_color,
                           main=main,
                           legend=FALSE)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
        
        msg = "Values of cells w/ sample size > 1 are averaged."
        grid.text(msg, x=0.5, y=0.02, gp=gpar(fontsize=10))
    }
    
}

single_dose_hmap2 = function(dat,
                             cluster_rows=FALSE, 
                             ncut=ncut_sd) {
    # graph params
    cellheight = 15
    cellwidth = 19
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    max_mfi = 100000; N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["gMFI"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), mean, na.rm=TRUE)
    
    probe_conc = dat[["Probe Quant Value"]]             
    
    # split heatmap matrix
    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    for (i in 1:lm_) {
        main = paste0("Probe Conc: ", probe_conc[1], " (ug/ml)",
                      "\nProbe x Target Spec (Results: MFI/1000)")
        
        mat = mat_list[[i]]
        
        pheatmap::pheatmap(t(mat),
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           breaks=breaks,
                           color=colors,
                           border_color=border_color,
                           display_numbers=round(t(mat / 1000), 1),
                           number_color=number_color,
                           main=main,
                           legend=legend)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }
    
}

single_dose_sample_size2 = function(dat,
                                    cluster_rows=FALSE, 
                                    ncut=ncut_sd) {
    # graph params
    cellheight = 15
    cellwidth = 19
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["gMFI"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), length)
    
    probe_conc = dat[["Probe Quant Value"]]             
    
    # split heatmap matrix
    nr = nrow(MAT)
    s = floor(nr / ncut) + 1
    s_ = rep(1:s, each=ncut)[1:nr]
    ls_ = length(s_)
    if (ls_ > 1) {
        if (sum(s_==s_[ls_]) == 1) s_[ls_] = s_[ls_ - 1]
    }
    
    mat_list = split(as.data.frame(MAT), s_)
    lm_ = length(mat_list)
    
    for (i in 1:lm_) {
        main = paste0("Probe Conc: ", probe_conc[1], " (ug/ml)",
                      "\nProbe x Target Spec (Results: Sample Size)")
        
        mat = mat_list[[i]]
        pheatmap::pheatmap(t(mat),
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           breaks=c(1, 2),
                           color=c("aliceblue"),
                           border_color=border_color,
                           display_numbers=round(t(mat)),
                           number_color=number_color,
                           main=main,
                           legend=FALSE)
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
        
        msg = "Values of cells w/ sample size > 1 are averaged."
        grid.text(msg, x=0.5, y=0.02, gp=gpar(fontsize=10))
    }
    
}


titration_tlgs = function(res_t) {
    ncells = length(res_t)
    nsmpls = sum(sapply(res_t, nrow))
    
    # heatmaps
    op = par(mar=c(3, 3, 5, 3))
    plot(0, 0, axes=F, xlab="", ylab="", pch="", main="Titration TLGs",
         cex.main=1.25)
    msg = paste0("Titration TLGs\nNumber of cell lines: ", ncells,
                 "\nNumber of total samples: ", nsmpls)
    text(0, 0, msg, cex=1.5)
    par(op)
    
    for (cell in names(res_t)) {
        titration_hmap(res_t[[cell]], cell)    
    }
    
    # sample size
    op = par(mar=c(3, 3, 3, 3))
    plot(0, 0, axes=F, xlab="", ylab="", pch="")
    text(0, 0, "Appendix\nSample Size Matrix", cex=1.5)
    par(op)
    
    for (cell in names(res_t)) {
        titration_sample_size(res_t[[cell]], cell)    
    }
}

single_dose_tlgs = function(res_s, v=1) {
    ncells = length(unique(sapply(res_s, function(x) unique(x[["target_spec_name"]]))))
    nsmpls = sum(sapply(res_s, nrow))
    
    # heatmaps
    op = par(mar=c(3, 3, 5, 3))
    plot(0, 0, axes=F, xlab="", ylab="", pch="", main="Single Dose TLGs",
         cex.main=1.25)
    msg = paste0("Single Dose TLGs\nNumber of cell lines: ", ncells,
                 "\nNumber of total samples: ", nsmpls)
    text(0, 0, msg, cex=1.5)
    par(op)
    
    if (v==1) {
        for (k in names(res_s)) {
            print(k)
            single_dose_hmap1(res_s[[k]])
        }
    }

    if (v==2) {
        for (k in names(res_s)) {
            print(k)
            single_dose_hmap2(res_s[[k]])
        }
    }
    
    # sample size
    op = par(mar=c(3, 3, 3, 3))
    plot(0, 0, axes=F, xlab="", ylab="", pch="")
    text(0, 0, "Appendix\nSample Size Matrix", cex=1.5)
    par(op)
    
    if (v==1) {
        for (k in names(res_s)) {
            single_dose_sample_size1(res_s[[k]])
        }
    }
    
    if (v==2) {
        for (k in names(res_s)) {
            single_dose_sample_size2(res_s[[k]])
        }
    }

    
}
