library(grid)

ref_cols = c("None"="aliceblue",
             "Negative Reference"="black",
             "Positive Reference"="seagreen2",
             "Benchmark"="red3")

ncut_ti = 40
ncut_sd = 38

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

prep_multi_dose_data  = function(project_data) {
    res = split(project_data, project_data$"target_spec_name")
    return(res)
}

get_probe_annotation = function(project_data) {
    annotation_list = split(project_data$annotation, project_data$probe_name)
    f = function(x) {
        check_vec = x %in% "None"
        if (all(check_vec)) {
            return("None")
        }else{
            return(unique(x[!check_vec]))
        }
    }
    sapply(annotation_list, f)
}

titration_hmap = function(dat, 
                          cell="cell", 
                          cluster_rows=FALSE, 
                          ncut=ncut_ti,
                          pa=NULL,
                          max_mfi=NULL) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    if (!is.null(pa)) {
        annotation_colors = list("annotation"=c("None"=ref_cols["None"],
                                              "Negative Reference"=ref_cols["Negative Reference"],
                                             "Positive Reference"=ref_cols["Positive Reference"],
                                             "Benchmark"=ref_cols["Benchmark"]))
    }

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
        txt = round(mat)
        txt[is.na(txt)] = "."
        
        if (!is.null(pa)) {
            annotation_row = data.frame(A=pa[rownames(mat)])
            
            annotation_ = annotation_colors$annotation
            names(annotation_) = sapply(strsplit(names(annotation_), "\\."), "[[", 1)
            annotation_ = annotation_[names(annotation_) %in% annotation_row$A]
            annotation_colors$A = annotation_
            
            pheatmap::pheatmap(mat,
                               cluster_rows=cluster_rows,
                               cluster_cols=FALSE,
                               cellwidth=cellwidth,
                               cellheight=cellheight,
                               angle_col=0,
                               breaks=breaks,
                               color=colors,
                               border_color=border_color,
                               display_numbers=txt,
                               number_color=number_color,
                               main=main,
                               na_col="gray90",
                               legend=legend,
                               annotation_legend=FALSE,
                               annotation_colors=annotation_colors,
                               annotation_row=annotation_row)
        }else{
            
            pheatmap::pheatmap(mat,
                               cluster_rows=cluster_rows,
                               cluster_cols=FALSE,
                               cellwidth=cellwidth,
                               cellheight=cellheight,
                               angle_col=0,
                               breaks=breaks,
                               color=colors,
                               border_color=border_color,
                               display_numbers=txt,
                               number_color=number_color,
                               main=main,
                               legend=legend)
        }

        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }

    return(MAT)
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
        txt = round(mat)
        txt[is.na(txt)] = "."

        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           angle_col=0,
                           breaks=c(1, 2),
                           color=c("aliceblue"),
                           border_color=border_color,
                           display_numbers=txt,
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

titration_tlgs = function(res_t, 
                          pa=NULL, 
                          sample_size=FALSE, 
                          max_mfi=NULL) {
    ncells = length(res_t)
    nsmpls = sum(sapply(res_t, nrow))
    
    if (is.null(max_mfi)) {
        p_max = max(sapply(res_t, function(x) max(x[["mfi"]], na.rm = TRUE)), 
                    na.rm=T)    
    }
    
    if (!sample_size) {
        op = par(mar=c(3, 3, 5, 3))
        
        plot(0, 0, axes=F, xlab="", ylab="", pch="", main="Titration TLGs",
             cex.main=1.25)
        msg = paste0("Titration TLGs\nNumber of cell lines: ", ncells,
                     "\nNumber of total samples: ", nsmpls)
        text(0, 0, msg, cex=1.5)
        
        if (!is.null(pa)) {
            plot(0, 0, axes=F, xlab="", ylab="", pch="", 
                 main="Titration TLGs\nReference Key",
                 cex.main=1.25)
            
            nr_ = names(pa)[pa == "Negative Reference"]
            pr_ = names(pa)[pa == "Positive Reference"]
            bm_ = names(pa)[pa == "Benchmark"]
            
            if (length(nr_) > 0) {
                nr_ = paste0("Negative Reference | ", nr_)    
            }else{
                nr_ = paste0("Negative Reference | Not applicable")  
            }
            
            if (length(pr_) > 0) {
                pr_ = paste0("Positive Reference | ", nr_)    
            }else{
                pr_ = paste0("Positive Reference | Not applicable")  
            }
            
            if (length(bm_) > 0) {
                bm_ = paste0("Benchmark | ", bm_)    
            }else{
                bm_ = paste0("Benchmark | Not applicable")  
            }
            
            legend("center", 
                   legend = c(bm_, 
                              nr_, 
                              pr_), 
                   pch=19, 
                   col=c(ref_cols["Benchmark"],
                         ref_cols["Negative Reference"],
                         ref_cols["Positive Reference"]))
        }
        
        par(op)
        
        hold = list()
        for (cell in names(res_t)) {
            res = titration_hmap(res_t[[cell]], cell, pa=pa, max_mfi=p_max)
            colnames(res) = paste0(colnames(res), " (ug/ml)")
            df = data.frame("target_spec"=rep(cell, nrow(res)),
                            probe_name=rownames(res),
                            check.names = F)
            df = cbind(df, round(res))
            hold[[cell]] = df  
        }
        hold = do.call(rbind, hold)
        rownames(hold) = NULL
        
    }else{
        # sample size
        op = par(mar=c(3, 3, 3, 3))
        plot(0, 0, axes=F, xlab="", ylab="", pch="")
        text(0, 0, "Appendix\nSample Size Matrix", cex=1.5)
        par(op)
        
        for (cell in names(res_t)) {
            titration_sample_size(res_t[[cell]], cell)    
        }
        
        hold = NULL
    }
    
    return(hold)
}

single_dose_hmap = function(dat,
                            cluster_rows=FALSE, 
                            ncut=ncut_sd,
                            pa=NULL,
                            max_mfi=NULL) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    if (!is.null(pa)) {
        annotation_colors = list("annotation"=c("None"=ref_cols["None"],
                                                "Negative Reference"=ref_cols["Negative Reference"],
                                                "Positive Reference"=ref_cols["Positive Reference"],
                                                "Benchmark"=ref_cols["Benchmark"]))
    }
    
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
        txt = round(mat)
        txt[is.na(txt)] = "."
        
        if (!is.null(pa)) {
            annotation_row = data.frame(annotation=pa[rownames(mat)])
            
            annotation_ = annotation_colors$annotation
            names(annotation_) = sapply(strsplit(names(annotation_), "\\."), "[[", 1)
            annotation_ = annotation_[names(annotation_) %in% annotation_row$annotation]
            annotation_colors$annotation = annotation_
            
            pheatmap::pheatmap(mat,
                               cluster_rows=cluster_rows,
                               cluster_cols=FALSE,
                               cellwidth=cellwidth,
                               cellheight=cellheight,
                               breaks=breaks,
                               color=colors,
                               border_color=border_color,
                               display_numbers=txt,
                               number_color=number_color,
                               main=main,
                               na_col="gray90",
                               legend=legend,
                               annotation_legend=FALSE,
                               annotation_colors=annotation_colors,
                               annotation_row=annotation_row)
        }else{
            pheatmap::pheatmap(mat,
                               cluster_rows=cluster_rows,
                               cluster_cols=FALSE,
                               cellwidth=cellwidth,
                               cellheight=cellheight,
                               breaks=breaks,
                               color=colors,
                               border_color=border_color,
                               display_numbers=txt,
                               number_color=number_color,
                               main=main,
                               legend=legend)
        }

        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }
    
    return(MAT)
}

single_dose_sample_size = function(dat,
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
        txt = round(mat)
        txt[is.na(txt)] = "."
        pheatmap::pheatmap(mat,
                           cluster_rows=cluster_rows,
                           cluster_cols=FALSE,
                           cellwidth=cellwidth,
                           cellheight=cellheight,
                           breaks=c(1, 2),
                           color=c("aliceblue"),
                           border_color=border_color,
                           display_numbers=txt,
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

single_dose_tlgs = function(res_s, 
                            pa=NULL, 
                            sample_size=FALSE,
                            max_mfi=NULL) {
    ncells = length(unique(sapply(res_s, function(x) unique(x[["target_spec_name"]]))))
    nsmpls = sum(sapply(res_s, nrow))
    
    if (is.null(max_mfi)) {
        p_max = max(sapply(res_s, function(x) max(x[["mfi"]], na.rm = TRUE)), 
                    na.rm=T)    
    }
    
    if (!sample_size) {
        op = par(mar=c(3, 3, 5, 3))
        plot(0, 0, axes=F, xlab="", ylab="", pch="", main="Single Dose TLGs",
             cex.main=1.25)
        msg = paste0("Single Dose TLGs\nNumber of cell lines: ", ncells,
                     "\nNumber of total samples: ", nsmpls)
        text(0, 0, msg, cex=1.5)
        par(op)
        
        if (!is.null(pa)) {
            
            plot(0, 0, axes=F, xlab="", ylab="", pch="", 
                 main="Titration TLGs\nReference Key",
                 cex.main=1.25)
            
            nr_ = names(pa)[pa == "Negative Reference"]
            pr_ = names(pa)[pa == "Positive Reference"]
            bm_ = names(pa)[pa == "Benchmark"]
            
            if (length(nr_) > 0) {
                nr_ = paste0("Negative Reference |", nr_)    
            }else{
                nr_ = paste0("Negative Reference | Not applicable")  
            }
            
            if (length(pr_) > 0) {
                pr_ = paste0("Positive Reference | ", nr_)    
            }else{
                pr_ = paste0("Positive Reference | Not applicable")  
            }
            
            if (length(bm_) > 0) {
                bm_ = paste0("Benchmark | ", bm_)    
            }else{
                bm_ = paste0("Benchmark | Not applicable")  
            }
            
            legend("center", 
                   legend = c(bm_, 
                              nr_, 
                              pr_), 
                   pch=19, 
                   col=c(ref_cols["Benchmark"],
                         ref_cols["Negative Reference"],
                         ref_cols["Positive Reference"]))
        }
        
        hold1 = list()
        for (k in names(res_s)) {
            res = single_dose_hmap(res_s[[k]], pa=pa, max_mfi=p_max)
            df = data.frame("probe_conc (ug/ml)"=rep(k, nrow(res)),
                            probe_name=rownames(res),
                            check.names = F)
            df = cbind(df, round(res))
            hold1[[k]] = df
        }
        hold1 = do.call(rbind, hold1)
        rownames(hold1) = NULL
        
    }else{
        # sample size
        op = par(mar=c(3, 3, 3, 3))
        plot(0, 0, axes=F, xlab="", ylab="", pch="")
        text(0, 0, "Appendix\nSample Size Matrix", cex=1.5)
        par(op)
        
        for (k in names(res_s)) {
            single_dose_sample_size(res_s[[k]])
        }
        
        hold1 = NULL
    }

    return(hold1)
}
