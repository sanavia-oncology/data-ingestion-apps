# Kwame Okrah
# 2026-10-28

results_summary_table = function(project_fldr) {
    
    all_files = list.files(project_fldr,
                           recursive  = TRUE,
                           full.names = TRUE)
    
    HOLD = list()
    
    results_files = all_files[grep("gated-results.csv$", all_files)]
    
    if (length(results_files) > 0) {
        
        hold        = list()
        hold_cls    = list()
        hold_probes = list()
        
        for (fl in results_files) {
            
            tmp = read.csv(fl, header = T, check.names = F)
            
            
            nsmpls          = nrow(tmp)
            mfi_channel     = tmp$mfi_channel[1]
            dose_type       = tmp$dose_type[1]
            dosing_schedule = tmp$dosing_schedule[1]
            
            cls     = sort(unique(tmp$"Target Spec ID"))
            ncells  = length(cls)
            
            uprobes = unique(tmp$"Probe ID")
            nprobes = length(uprobes)
            
            nplates = length(unique(tmp$Platename))
            
            author_exp    = tmp$author_exp[1]
            author_qc     = tmp$author_qc[1]
            author_gating = tmp$author_gating[1]
            
            uconc = unique(tmp$`Probe Quant Value`)
            rng   = range(as.numeric(uconc))
            crng  = paste0("[", paste0(rng, collapse = ", "), "]")
            
            fl_        = gsub(project_fldr, "", fl)
            fl_        = strsplit(fl_, "/")
            proj_group = sapply(fl_, "[[", 2)
            proj_name  = sapply(fl_, "[[", 3)
            
            file_metadata = file.info(fl)
            creation_time = sapply(strsplit(as.character(file_metadata$ctime),
                                            "\\ "), "[[", 1)
            
            sana_project_ = sort(unique(tmp$sana_project))
            sana_project_01 = ifelse(length(grep("01", sana_project_)) == 0,
                                     "", "SANA01")
            sana_project_02 = ifelse(length(grep("02", sana_project_)) == 0,
                                     "", "SANA02")
            sana_project_03 = ifelse(length(grep("03", sana_project_)) == 0,
                                     "", "SANA03")
            sana_project_04 = ifelse(length(grep("04", sana_project_)) == 0,
                                     "", "SANA04")
            
            sana_project_C = c(sana_project_01, sana_project_02,
                               sana_project_03, sana_project_04)
            sana_project_C = sort(sana_project_C[sana_project_C != ""])
            sana_project   = paste0(sana_project_C, collapse = ", ")
            
            experiment_type = unique(tmp$experiment_type)
            
            res = c(date_created    = creation_time,
                    project_group   = proj_group,
                    project_name    = proj_name,
                    sana_project    = sana_project,
                    experiment_type = experiment_type,
                    dosing_schedule = dosing_schedule,
                    dose_type       = dose_type,
                    wells_n         = nsmpls,
                    cell_type_n     = ncells,
                    probe_n         = nprobes,
                    mfi_channel     = mfi_channel,
                    author_exp      = author_exp,
                    author_qc       = author_qc,
                    author_gating   = author_gating,
                    plates_n        = nplates)
            
            hold[[fl]]        = res
            hold_cls[[fl]]    = cls
            hold_probes[[fl]] = uprobes
            
            tmp$project_group = proj_group
            tmp$project_name = proj_name
            HOLD[[fl]] = tmp
        }
        
        tab = do.call(rbind, hold)
        tab = as.data.frame(tab)
        
        tab$experiment_type = ifelse(tab$experiment_type == "binding_expr",
                                     "Binding expression", "Internalization")
        
        o   = rev(order(tab[, "date_created"]))
        tab = tab[o, , drop = F]
        rownames(tab) = NULL
        
        return(list(summary_table=tab, sheets=HOLD))
        
    } else {
        
        return(NA)
        
    }
}


get_sana_projects = function(sheets, path_to_drop=NULL, verbose=TRUE) {
    dat = do.call(rbind, sheets)
    rownames(dat) = NULL
    
    n = nrow(dat)
    
    if (!is.null(path_to_drop)) {
        drop_mat = read.csv(path_to_drop, header=TRUE, stringsAsFactors=FALSE)
        dk = paste0(drop_mat$Platename, "|", drop_mat$well)
        keep = !paste0(dat$Platename, "|", dat$"Plate Position") %in% dk
        dat = dat[keep,,drop=FALSE]
    }
    
    if (verbose) {
        cat(paste0("dropped: ", n - nrow(dat), " / ", n, " row(s)\n"))    
    }
    
    sana_project = dat$sana_project
    u_sana_project = unique(sana_project)
    u_sana_project = unlist(strsplit(u_sana_project, "\\|"))
    u_sana_project = sort(unique(u_sana_project))
    
    hold = list()
    for (proj in u_sana_project) {
        sel = grep(proj, sana_project)
        hold[[proj]] = dat[sel,,drop=FALSE]
    }
    
    if (verbose) {
        cat(paste0("sana_project: ", length(u_sana_project), " detected\n"))    
    }
    
    return(hold)    
}