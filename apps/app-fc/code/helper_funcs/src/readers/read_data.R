# author: Kwame Okrah
# date: 2026-03-05

# read_data
read_data = function(paths, verbose=FALSE) {
    assay_file_paths = paths[grep(".fcs$", paths)]
    merged_data_path = paths[grep("qc_report/.*merged-data.csv$", paths)]
    mfi_channel_path = paths[grep("qc_report/.*mfi-channel.csv$", paths)]
    
    fcs_list = read_fcs_files(assay_file_paths, verbose=verbose)
    merged_data = read_merged_data(merged_data_path)
    o = rownames(merged_data)
    fcs_list = fcs_list[o]
    
    mfi_channel = read.csv(mfi_channel_path, header = T)
    merged_data$mfi_channel = mfi_channel$mfi_channel[1]

    fcs_files = fcs_list
    
    tmp_hold = colnames(Biobase::exprs(fcs_files[[1]]))
    for (i in 1:length(fcs_files)) {
        tmp = fcs_files[[i]]
        tmp_ = Biobase::exprs(tmp)
        tmp_hold = intersect(tmp_hold, colnames(tmp_))
    }
    
    for (i in 1:length(fcs_files)) {
        tmp = fcs_files[[i]]
        fcs_files[[i]] = tmp[,tmp_hold]
    }
    
    res = list(pinfos=merged_data, fcs_files=fcs_files)
    
    return(res)
}