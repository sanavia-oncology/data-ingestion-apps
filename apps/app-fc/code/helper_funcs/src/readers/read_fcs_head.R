# author: Kwame Okrah
# date: 2026-03-05

# read_fcs_head (read fcs head)
read_fcs_head = function(assay_file_paths) {
    # read fcs files (header only)
    assay_file_paths = assay_file_paths[grep(".fcs$", assay_file_paths)]
    
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
        hold = c(hold, Filename = fl_nam, full_path=fl)
        rel_params = unlist(hold)
        
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