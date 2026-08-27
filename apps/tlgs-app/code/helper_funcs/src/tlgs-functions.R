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
                          max_mfi=NULL,
                          cluster_rows=FALSE, 
                          ncut=40) {

    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    # data
    mfi = dat[["mfi"]]
    probe_id = dat[["probe_name"]]
    probe_conc = dat[["conc (ug/ml)"]]
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
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }

    return(MAT)
}

titration_sample_size = function(dat, 
                                 cell="cell", 
                                 cluster_rows=FALSE, 
                                 ncut=40) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"

    # data
    mfi = dat[["mfi"]]
    probe_id = dat[["probe_name"]]
    probe_conc = dat[["conc (ug/ml)"]]
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

titration_plot = function(adam, 
                          sample_size=FALSE, 
                          max_mfi=NULL) {
    
    res_t = split(adam, adam$"target_spec_name")
    
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

        par(op)
        
        hold = list()
        for (cell in names(res_t)) {
            res = titration_hmap(res_t[[cell]], cell, max_mfi=p_max)
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
                            max_mfi=NULL,
                            cluster_rows=FALSE,
                            ncut=38) {

    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["mfi"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), mean, na.rm=TRUE)
    
    probe_conc = dat[["conc (ug/ml)"]]             
    
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

        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }
    
    return(MAT)
}

single_dose_sample_size = function(dat,
                                   cluster_rows=FALSE, 
                                   ncut=38) {
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    
    # data
    target_spec_name = dat[["target_spec_name"]]
    probe_name = dat[["probe_name"]]
    mfi = dat[["mfi"]]
    
    MAT = tapply(mfi, list(probe_name, target_spec_name), length)
    
    probe_conc = dat[["conc (ug/ml)"]]             
    
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

single_dose_plot = function(adam, 
                            sample_size=FALSE,
                            max_mfi=NULL) {
    res_s = split(adam, adam$"conc (ug/ml)")
    
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
 
        hold1 = list()
        for (k in names(res_s)) {
            res = single_dose_hmap(res_s[[k]], max_mfi=p_max)
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

notes_summary_func = function(notes) {
    notes = as.character(notes)
    I = c("none"="None",
          "neg_ref"="Negative reference",
          "benchmark"="Benchmark",
          "drop"="Drop")
    notes = factor(I[notes], levels=I)

    x = as.data.frame(table(notes))
    colnames(x) = c("notes", "counts")
    x
}

dup_analysis = function(adam) {
    rownames(adam) = paste0(adam$platename, "|", adam$position)
    target = adam$target_spec_name
    probe = adam$probe_name
    conc = adam$"conc (ug/ml)"
    
    duplicates = rep("none", nrow(adam))
    names(duplicates) = rownames(adam)
    
    x = paste0("(", target, ") x (", probe, ") x (", conc, " ug/ml)")
    dat_list = split(adam, x)
    ndups = sapply(dat_list, nrow)
    
    if (any(ndups > 1)) {
        dat_list = dat_list[order(-ndups)]
        dat_list = dat_list[ndups > 1]
        
        for (i in 1:length(dat_list)) {
            X = dat_list[[i]]
            sel = rownames(X)
            group_name = paste0("DUP-", sprintf("%03d", i))
            duplicates[sel] = group_name
        }
    }
    
    adam$duplicates = duplicates
    
    return(adam)
}

adam_plot = function(adam, 
                     sample_names=NULL, 
                     group_name=NULL,
                     session_name=NULL,
                     filename=NULL) {
    
    if (is.null(session_name)) {
        session_name = sapply(strsplit(as.character(Sys.time()), "\\."), "[[", 1)
    }
    
    adam$probe_name = as.character(adam$probe_name)
    adam$target_spec_name = as.character(adam$target_spec_name)
    
    if (is.null(group_name)) {
        group_name = "SELECTED SAMPLES"
    }
    
    set.seed(1983)
    o = sample(1:nrow(adam), nrow(adam), replace = F)
    dat = adam[o,,drop=F]
    rownames(dat) = paste0(dat$platename, "|", dat$position)

    y = dat$mfi
    loc = 1:length(y)
    names(y) = rownames(dat)
    names(loc) = rownames(dat)
    
    if (!is.null(filename)) {
        pdf(filename, height = 2.65, width = 6)
    }
   
    op = par(mar=c(3, 3, 0.5, 1), mfrow=c(1, 2), mgp=c(1.8, .8, 0))

    # fig 1
    plot(loc, y,
         col="gray85",
         pch=19,
         ylab="MFI",
         ylim=c(0, max(y, na.rm=TRUE)),
         cex.axis=0.9,
         xlab=paste0("Samples (", length(y), ")"))
    
    if (!is.null(sample_names)) {
        points(loc[sample_names], y[sample_names], 
               pch=19,
               col="red", 
               cex=1.2)
        ave = round(mean(y[sample_names], na.rm = T), 1)
        abline(h=ave, col="red4", lwd=3)
    }else{
        ave = round(mean(y, na.rm = T), 1)
        abline(h=ave, col="black", lwd=3)
    }

    # fig 2
    par(mar=c(3, 0, 0.5, 0.5), mgp=c(1.8, .8, 0))
    
    plot(0, 0, ylim=c(0.5, 10.5), xlim=c(0, 10),
         axes=F, xlab="", ylab="", main="",
         pch="")
    
    col2 = "gray30"
    col3 = "gray90"

    if (is.null(sample_names)) {
        col1 = "gray50"
        
        group_name = "ALL DATA"
        probe_name = paste0(length(unique(adam$probe_name))," PROBES")
        target_name = paste0(length(unique(adam$target_spec_name))," TARGETS")
        
        rng = range(adam$"conc (ug/ml)")
        if (rng[1]==rng[2]) {
            conc_name = paste0("Conc. ", rng[1], " (ug/ml)")
        }else{
            conc_name = paste0("Conc. ", rng[1], " - ", rng[2], " (ug/ml)")
        }
    }else{
        col1 = "red3"
        
        target = unique(dat[sample_names, "target_spec_name"])
        probe = unique(dat[sample_names, "probe_name"])
        rng = range(dat[sample_names, "conc (ug/ml)"])
        
        lt = length(target)
        if (lt==1) {
            target_name = target
        }else{
            target_name = paste0(lt, " TARGETS")
        }
        
        lp = length(probe)
        if (lp==1) {
            probe_name = probe
        }else{
            probe_name = paste0(lp, " PROBES")
        }
        
        if (rng[1]==rng[2]) {
            conc_name = paste0("Conc. ", rng[1], " (ug/ml)")
        }else{
            conc_name = paste0("Conc. ", rng[1], " - ", rng[2], " (ug/ml)")
        }
        
        if (nchar(target_name) > 25) {
            target_name = paste0(substr(target_name, 1, 17),"*TRUNC*")
        }
        
        if (nchar(probe_name) > 25) {
            probe_name = paste0(substr(probe_name, 1, 17),"*TRUNC*")
        }
        
        if (nchar(group_name) > 25) {
            group_name = paste0(substr(group_name, 1, 17),"*TRUNC*")
        }
    }
    
    if (is.null(sample_names)) {
        sample_size = paste0("Sample size: ", length(y))
        mean_note = paste0("Mean: ", ave)
        range_note = paste0("Range: ", 
                            round(min(y, na.rm = T), 1), 
                            " - ",
                            round(max(y, na.rm = T), 1))
    }else{
        sample_size = paste0("Sample size: ", length(y[sample_names]))
        mean_note = paste0("Mean: ", ave)
        range_note = paste0("Range: ", 
                            round(min(y[sample_names], na.rm = T), 1), 
                            " - ",
                            round(max(y[sample_names], na.rm = T), 1))
    }

    text(5, 10 - 1.5 * 0, group_name, cex=1.2, font=2)
    text(5, 10 - 1.5 * 1, probe_name, cex=1.2, col=col1)
    text(5, 10 - 1.5 * 2, target_name, cex=1.2, col=col1)
    text(5, 10 - 1.5 * 3, conc_name, cex=1.2, col=col1)
    text(5, 10 - 1.5 * 4, sample_size, cex=1.2, col=col2)
    text(5, 10 - 1.5 * 5, mean_note, cex=1.2, col=col2)
    text(5, 10 - 1.5 * 6, range_note, cex=1.2, col=col2)

    mtext("SESSION_NAME", side = 1, line = 0.5, col=col3, cex=0.8)
    mtext(session_name, side = 1, line = 1.25, col=col3, cex=0.8)
    
    par(op)
    
    if (!is.null(filename)) {
        dev.off()
    }

}

dup_adam_plot = function(adam, 
                         session_name=NULL, 
                         filename=NULL) {
    
    check = adam$duplicates %in% "none"

    if (!is.null(filename)) {
        pdf(filename, height = 2.65, width = 6)
    }
    
    if (all(check)) {

        op = par(mar=c(3, 0, 0.5, 0.5), mgp=c(1.8, .8, 0))
        plot(0, 0, ylim=c(0.5, 10.5), xlim=c(0, 10),
             axes=F, xlab="", ylab="", main="",
             pch="")
        text(5, 5, "GREAT!\nNo duplicates found", cex=1.5)
        par(op)

    }else{
        
        adam_sub = adam[!check,,drop=FALSE]
        adam_sub_list = split(adam_sub, adam_sub$duplicates)
        for (dup_id in names(adam_sub_list)) {
            m = adam_sub_list[[dup_id]]
            sample_names = paste0(m$platename, "|", m$position)
            group_name = dup_id
            adam_plot(adam,
                      sample_names=sample_names,
                      group_name=group_name,
                      session_name=session_name,
                      filename=NULL)
        }
    }
    
    if (!is.null(filename)) {
        dev.off()
    }

}


titration_res_table = function(adam, sample_size=FALSE) {
    
    res_t = split(adam, adam$"target_spec_name")
    
    ncells = length(res_t)
    nsmpls = sum(sapply(res_t, nrow))

    hold = list()
    for (cell in names(res_t)) {
        dat = res_t[[cell]]
        
        mfi = dat[["mfi"]]
        probe_id = dat[["probe_name"]]
        probe_conc = dat[["conc (ug/ml)"]]
        
        if (sample_size) {
            res = tapply(mfi, list(probe_id, probe_conc), length)
        }else{
            res = tapply(mfi, list(probe_id, probe_conc), mean, na.rm=T)
        }
        
        colnames(res) = paste0(colnames(res), " (ug/ml)")
        df = data.frame("target_spec"=rep(cell, nrow(res)),
                        probe_name=rownames(res),
                        check.names = F)
        df = cbind(df, round(res))
        hold[[cell]] = df  
    }
    hold = do.call(rbind, hold)
    rownames(hold) = NULL
    
    return(hold)
}

single_dose_res_table = function(adam, sample_size=FALSE) {
    res_s = split(adam, adam$"conc (ug/ml)")
    
    names(res_s) = paste0(names(res_s), " (ug/ml)")
    
    ncells = length(unique(sapply(res_s, function(x) unique(x[["target_spec_name"]]))))
    nsmpls = sum(sapply(res_s, nrow))
    
    hold = list()
    for (k in names(res_s)) {
        dat = res_s[[k]]
        target_spec_name = dat[["target_spec_name"]]
        probe_name = dat[["probe_name"]]
        mfi = dat[["mfi"]]
        
        if (sample_size) {
            res = tapply(mfi, list(probe_name, target_spec_name), length)
        }else{
            res = tapply(mfi, list(probe_name, target_spec_name), mean, 
                         na.rm=TRUE)    
        }
        
        df = data.frame("probe_conc"=rep(k, nrow(res)),
                        probe_name=rownames(res),
                        check.names = F)
        df = cbind(df, round(res))
        hold[[k]] = df
    }
    hold = do.call(rbind, hold)
    rownames(hold) = NULL
    
    return(hold)
}

make_res_table = function(adam, table_type) {
    if (table_type == "Probe x Conc") {
        res = titration_res_table(adam)
    }else{
        res = single_dose_res_table(adam)
    }
    
    d = rowSums(is.na(res[,-c(1, 2),drop=F])) == (ncol(res)-2)
    res = res[!d,,drop=F]
    return(res)
    
}


plot_res_table = function(res, ncut=38) {
    # mnc = max(nchar(res[,1]))
    # rn_ = sprintf(paste0("%-", mnc, "s"), res[,1])
    # rn = paste0(rn_,  " ", res[, 2])
    rn = paste0(res[,2], " ", res[,1])
    
    MAT = res[,-c(1, 2),drop=F]
    rownames(MAT) = rn
    
    # graph params
    cellheight = 15
    cellwidth = 35
    legend = FALSE
    border_color = "white"
    number_color = "black"
    cluster_rows = FALSE
    
    max_mfi = max(MAT, na.rm = TRUE)
    N = 100
    breaks = seq(0, max_mfi, length.out = N + 1)
    colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(N)
    
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
        main = paste0("Probe Conc: (ug/ml)",
                      "\nProbe x Target Spec (Results: MFI)")
        
        mat = mat_list[[i]]
        txt = round(mat)
        txt[is.na(txt)] = "."
        
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
        
        msg = paste0("Date: ", Sys.Date(), " | ",
                     "Page ", i , "/", lm_)
        grid.text(msg, x=0.5, y=0.98, gp=gpar(fontsize=10))
    }
    
    # # p <- pheatmap::pheatmap(mat, fontsize_row = 9, silent = TRUE)
    # 
    # # Find and modify the row names grob
    # grid.newpage()
    # p$gtable$grobs[[which(p$gtable$layout$name == "row_names")]]$gp$fontfamily <- "mono"
    # grid.draw(p$gtable)
    
    
}
