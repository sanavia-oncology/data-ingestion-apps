# author: Kwame Okrah
# date: 2025-11-14

assay_plate_position_check = function(data_list) {
    
    position_check_list = list()
    for (k in names(data_list)) {
        position_check_list[[k]] = plate_position_check(data_list[[k]][["Plate Position"]])
    }

    position_check_summary = sapply(lapply(position_check_list, function(x) apply(x[,-1,drop=FALSE], 2, all)), 
                                    all)
  
    res = list(position_check_summary = position_check_summary,
               position_check_detailed = position_check_list)
  
    return(res)
}

merge_assay_data = function(assay_data, 
                            pinfo_sheets_list) {

    # Run checks on assay_data
    data_list = split(assay_data, assay_data[, "Platename"])
    dres = assay_plate_position_check(data_list)
    position_check_summary = dres[["position_check_summary"]]
    position_check_detailed = dres[["position_check_detailed"]]
  
    hold = list()
  
    for (pid in names(pinfo_sheets_list)) {
        # Check 1. Is Platename and Plate Position in PINFO error free?
        check_list = list()
      
        pinfo_dat = pinfo_sheets_list[[pid]]
      
        CHECKS_DETAILED_Platename = platename_check(pinfo_dat$Platename)
        CHECKS_DETAILED_Plate_Position = plate_position_check(pinfo_dat$"Plate Position")
    
        CHECKS_DETAILED_Platename_ = all(CHECKS_DETAILED_Platename[,-1,drop=FALSE])
        CHECKS_DETAILED_Plate_Position_ = all(CHECKS_DETAILED_Plate_Position[,-1,drop=FALSE])
    
        check1 = all(CHECKS_DETAILED_Platename_, CHECKS_DETAILED_Plate_Position_)
        check_list[["check1"]] = check1
      
        if (check1) {
            # Check 2. Is PINFO|Platename in ASSAY|Platename?
            pid_platename = pinfo_dat[1, "Platename"]
            names(pid_platename) = pid
            check2 = pid_platename %in% names(data_list)
        }else{
            pid_platename = NULL
            check2 = FALSE
        }
        check_list[["check2"]] = check2
      
        if (check2) {
            # Check 3. Is ASSAY|Plate Position error free?
            check3 = position_check_summary[pid_platename]
            names(check3) = NULL
        }else{
            check3 = FALSE
        }
        check_list[["check3"]] = check3
      
        if (check3) {
            # Check 4. Are all SID in dat_rn?
            dat = data_list[[pid_platename]]      
            dat_rn = paste0(dat$Platename, "|", dat$"Plate Position")
            SID = paste0(pinfo_dat$Platename, "|", pinfo_dat$"Plate Position")    
            check4 = all(SID %in% dat_rn)
        }else{
            SID = NULL
            dat_rn = NULL
            check4 = FALSE
        }
        check_list[["check4"]] = check4
      
        # Merge data
        if (check4) {
            rownames(dat) = dat_rn
            pinfo_dat$SID = SID
          
            merged_data = cbind(pinfo_dat, dat[pinfo_dat$SID,,drop=F])
            rownames(merged_data) = NULL
            merged_data$"PINFO_QC_DATE" = Sys.Date()
    
        }else{
            merged_data = NULL
        }
        
        hold[[pid]] = list("merge_checks" = unlist(check_list),
                           "merged_data" = merged_data,
                           "pinfo_sid" = SID,
                           "assay_sid" = dat_rn,
                           "key" = pid_platename)
    }       

    res = list(merge_info = hold,
               assay_position_check_summary = position_check_summary,
               assay_position_check_detailed = position_check_detailed)
  
    
    return(res)
}
