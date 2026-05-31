# Kwame Okrah
# 2026-05-29

get_paths_by_project2 = function(project_fldr, assay_type) {
    
    all_files = list.files(project_fldr, 
                           recursive=TRUE, 
                           full.names=TRUE)
    
    qcr_folders = all_files[grep("qc_report/.*merged-data.csv$", all_files)]
    qcr_folders = sapply(strsplit(qcr_folders, "qc_report"), "[[", 1)
    proj_folders = sapply(strsplit(qcr_folders, "/"), function(x) x[length(x)])
    proj_folders = unique(proj_folders)
    
    hold = list()
    for (k in proj_folders) {
        tmp = all_files[grep(k, all_files)]
        pinfos = tmp[grep("plate_information_sheets", tmp)]
        assay_data = tmp[grep("assay_data", tmp)]
        qc_report = tmp[grep("qc_report", tmp)]
        gating_results = tmp[grep("gating_results", tmp)]
        
        if (length(grep("pinfo.csv", pinfos)) > 0) {
            hold[[k]] = list(pinfos=pinfos, 
                             assay_data=assay_data,
                             qc_report=qc_report,
                             gating_results=gating_results)    
        }
    }
    
    return(hold)
}


front_page_table2 = function(project_paths, assay_type) {
    hold = list()
    
    for (k in names(project_paths)) {
        x = project_paths[[k]]
        
        pinfos_paths = x[["pinfos"]][grep("pinfo.csv", x[["pinfos"]])]
        n_pinfos = length(pinfos_paths)
        n_assays = length(grep(assay_type, x[["assay_data"]]))
        # n_qcrepo = length(grep("merged-data.csv$", x[["qc_report"]]))
        n_gatres = length(grep("gated-results.csv$", x[["gating_results"]]))
        
        proj_group = sapply(strsplit(pinfos_paths[1], k), "[[", 1)
        proj_group = sapply(strsplit(proj_group, "/"), function(x) x[length(x)])
        
        ctime = as.Date(sort(file.info(pinfos_paths)$mtime)[1])
        if (n_gatres > 0) {
            is_gated = "Yes"  
        }else{
            is_gated = "No"
        } 
        
        hold[[k]] = data.frame("Date Created"=ctime,
                               "Project Group"=proj_group,
                               "Project Name"=k, 
                               "Plate Sheets"=n_pinfos,
                               "FCS Files"=n_assays,
                               "Is Gated"=is_gated,
                               check.names = FALSE)
    }
    
    hold = do.call(rbind, hold)
    rownames(hold) = NULL
    
    o = order(hold[["Date Created"]], decreasing = T)
    hold = hold[o,,drop=FALSE]
    
    rownames(hold) = NULL
    
    return(hold)
}

