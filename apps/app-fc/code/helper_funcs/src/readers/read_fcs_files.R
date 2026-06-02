# author: Kwame Okrah
# date: 2026-03-05

# read_fcs_files (read fcs fils)
read_fcs_files = function(assay_file_paths, verbose=FALSE) {
    if (verbose) {
        cat("loading fcs files...\n")
    }
    
    assay_file_paths = assay_file_paths[grep(".fcs$", assay_file_paths)]
    
    # 1. read header only
    FCS_HEAD = read_fcs_head(assay_file_paths)
    
    FCS_HEAD$unam = paste0(FCS_HEAD$PLATENAME, "|",
                           FCS_HEAD$"Plate Position")
    
    rownames(FCS_HEAD) = FCS_HEAD$unam
    
    # 2. read fcs file
    fcs_list = list()
    for (i in 1:nrow(FCS_HEAD)) {
        if (verbose) {
            fcs_nam = FCS_HEAD$full_path[i]
            fcs_nam = sapply(strsplit(fcs_nam, "/"), function(x) x[length(x)])
            print(fcs_nam)
        }
        
        fcs = tryCatch(
            expr = {
                flowCore::read.FCS(FCS_HEAD$full_path[i], 
                                   transformation=FALSE)
            },
            error = function(e) {
                return(NA)
            }
        )
        
        fcs_list[[FCS_HEAD$unam[i]]] = fcs
    }
    
    if (verbose) {
        cat("done\n")
    }
    
    return(fcs_list)
}