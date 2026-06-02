# author: Kwame Okrah
# date: 2025-11-14

read_derived_results_csvs = function(assay_file_paths) {
    assay_file_paths = assay_file_paths[grep("derived-results.csv$", 
                                             assay_file_paths)]
    
    hold = list()
    
    for (fl in assay_file_paths) {
        fl_nam = sapply(strsplit(fl, "/"), function(x) x[length(x)])
        x = read.csv(fl, header=TRUE, check.names=FALSE)
        x$Filename = fl_nam
        hold[[fl_nam]] = x
    }
    
    res = do.call(rbind, hold)
    rownames(res) = NULL
    
    return(res)
}

read_fcs_head = function(assay_file_paths) {
    assay_file_paths = assay_file_paths[grep(".fcs$", assay_file_paths)]
    
    # read fcs files (header only)
    assay_files = list()
    
    for (fl in assay_file_paths) {
        fl_nam = sapply(strsplit(fl, "/"), function(x) x[length(x)])
        af = flowCore::read.FCSheader(fl)[[1]]
      
        af_nams = names(af)
        SEL = c("$PLATENAME", "$WELLID", "$DATE", "$TOT")
    
        hold = list()
        for (k in SEL) {
            if (k %in% af_nams) {
                hold[[gsub("\\$", "", k)]] = af[[k]]
            }else{
                hold[[gsub("\\$", "", k)]] = "not_in_fcs"
            }
        }
        hold = c(hold, Filename = fl_nam)
        rel_params = unlist(hold)

        # add all parameters found in fcs file
        pars_ = af[paste0("$P", 1:as.numeric(af[["$PAR"]]), "N")]
        pars = paste0(pars_, collapse=";")
        rel_params = c(rel_params, "PAR_STRING"=pars)
        
        # save relevant parameters
        assay_files[[fl_nam]] = rel_params
    }
    
    res = do.call(rbind, assay_files)
    rownames(res) = NULL

    res[,"PLATENAME"][res[,"PLATENAME"] == "NA"] = "00_Missing_Platename"
    res = as.data.frame(res)

    res$"Platename" = res$PLATENAME
    res$"Plate Position" = res$WELLID
    res$"Analysis Date" = res$DATE
    res$"Analysis Type" = "Antibody Binding"
    res$"Assay Data Type" = "Flow Cytometry"
    res$"Result Type" = "FCS Total Events"
    res$"Result" = res$TOT
    
    return(res)
}

read_assay_data = function(assay_file_paths, selected_assay) {
    
    if (selected_assay == "fcs") {
        assay_file_paths = assay_file_paths[grep(".fcs$", assay_file_paths)]
        res = read_fcs_head(assay_file_paths)
    }

    if (selected_assay == "derived-results") {
        assay_file_paths = assay_file_paths[grep("derived-results.csv$", 
                                                 assay_file_paths)]
        res = read_derived_results_csvs(assay_file_paths)
    }

    return(res)
}
