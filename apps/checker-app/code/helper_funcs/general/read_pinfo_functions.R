# author: Kwame Okrah
# date: 2025-11-14

get_file_paths = function(data_path, selected_assay) {
    res = list()
    res[["dir_test"]] = character()
    res[["pinfo_csv_paths"]] = character()
    res[["assay_file_paths"]] = character()
    
    # 1. check data_path validity
    if (file_test("-d", data_path)) {
        res[["dir_test"]] = "pass"
    }else{
        res[["dir_test"]] = "fail"
        return(res)
    }
  
    if (selected_assay=="frd") {
        make_fred_pinfo_assay_data(data_path)
    }
  
    # 2. read in all files in data_path
    FLS_pinfo = list.files(paste0(data_path, "/plate_information_sheets"),
                           recursive=TRUE, full.names=TRUE)
    FLS_assay = list.files(paste0(data_path, "/assay_data"),
                           recursive=TRUE, full.names=TRUE)
  
    # 3. get pinfo files
    pinfo_csv_paths = sort(FLS_pinfo[grep(".csv$", FLS_pinfo)])
    res[["pinfo_csv_paths"]] = pinfo_csv_paths
  
    # 4. get assay files
    # selected_assay == "none" (do nothing)
    if (selected_assay == "fcs") {
        fcs_fils = FLS_assay[grep(".fcs$", FLS_assay)]
        res[["assay_file_paths"]] = sort(fcs_fils)
    }
  
    if (selected_assay == "frd") {
        frd_fils = FLS_assay[grep("frd.csv$", FLS_assay)]
        res[["assay_file_paths"]] = frd_fils
    }
  
    if (selected_assay == "varioskan-skax") {
        res[["assay_file_paths"]] = sort(FLS_assay[grep(".skax$", FLS_assay)])
    }

    if (selected_assay == "derived-results") {
        res[["assay_file_paths"]] = sort(FLS_assay[grep("_derived.csv$", FLS_assay)])
    }
  
    # 5. return
    return(res)  
}

read_pinfo_csvs = function(pinfo_csv_paths, selected_assay=NULL) {
    # read plate info. sheets csvs    
    pinfos = list()
    
    for (fl in pinfo_csv_paths) {
        fl_nam = sapply(strsplit(fl, "/"), function(x) x[length(x)])
        x = read.csv(fl, header=TRUE, check.names=FALSE)
        x$Filename = fl_nam
        pinfos[[fl_nam]] = x
    }
    
    if (!is.null(selected_assay)) {
        if (selected_assay=="frd") {
              pinfos_ = pinfos[[1]]
              pinfos = split(pinfos_, pinfos_$Platename)
        }
    }

    return(pinfos)
}
