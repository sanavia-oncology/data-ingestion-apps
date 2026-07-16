# Kwame Okrah
# 2026-05-29

results_summary_list = function(project_fldr) {
    
    all_files = list.files(project_fldr, 
                           recursive=TRUE, 
                           full.names=TRUE)
    
    results_files = all_files[grep("gated-results.csv$", all_files)]
    
    if (length(results_files) > 0) {
        hold = list()
        hold_cls = list()
        hold_probes = list()
        for (fl in results_files) {
            tmp = read.csv(fl, header = T, check.names = F)
            nsmpls = nrow(tmp)
            mfi_channel = tmp$mfi_channel[1]
            dose_type = tmp$dose_type[1]
            cls = sort(unique(tmp$"Target Spec ID"))
            ncells = length(cls)
            uprobes = unique(tmp$"Probe ID")
            nprobes = length(uprobes)
            nplates = length(unique(tmp$Platename))
            author_exp = tmp$author_exp[1]
            author_qc = tmp$author_qc[1]
            author_gating = tmp$author_gating[1]
            uconc = unique(tmp$`Probe Quant Value`)
            rng = range(as.numeric(uconc))
            crng = paste0("[", paste0(rng, collapse = ", "), "]")
            
            fl_ = gsub(project_fldr, "", fl)
            fl_ = strsplit(fl_, "/")
            proj_group = sapply(fl_, "[[", 2)
            proj_name = sapply(fl_, "[[", 3)
            
            file_metadata = file.info(fl)
            creation_time = sapply(strsplit(as.character(file_metadata$ctime), 
                                            "\\ "), "[[", 1)
            
            res = c(date_created=creation_time,
                    project_group=proj_group,
                    project_name=proj_name,
                    project_desc="Not available yet",
                    results_n=nsmpls,
                    dose_type=dose_type,
                    cell_type_n=ncells,
                    probe_n=nprobes,
                    mfi_channel=mfi_channel,
                    full_path=fl)
            
            hold[[fl]] = res 
            hold_cls[[fl]] = cls
            hold_probes[[fl]] = uprobes
        }
        
        tab = do.call(rbind, hold)
        tab = as.data.frame(tab)
        rownames(tab) = NULL
        res = list(front_page_table=tab, 
                   unique_cell_lines=hold_cls,
                   unique_probes=hold_probes,
                   author_exp=author_exp,
                   author_qc=author_qc,
                   author_gating=author_gating,
                   "conc [min, max] (ug/ml)"=crng,
                   plates_n=nplates)
        return(res)
    }else{
        return(NA)
    }
}
