# author: Kwame Okrah
# date: 2025-11-14


print_pinfo_error_msg = function(plate_nam, 
                                 pinfo_col, 
                                 checks_detailed) {
    x = checks_detailed[[plate_nam]][[pinfo_col]]
    
    cat("Today's date:", as.character(Sys.Date()))
    cat("\n\n")

    if (pinfo_col == "Required Columns") {      
        colnames(x) = gsub(" ", "_", colnames(x))
        x[,1] = paste0("'", x[,1], "'")
        print(x)
            
    }else{

        hm = paste0("Column: ", pinfo_col)
        cat(hm, "\n")
        cat("Length of column:", nrow(x), "\n")
        x1 = x[,1]
      
        if (pinfo_col == "Probe Quant Value") {

            cat("\n")
            cat("Column summary table:\n")
            cat("---------------------\n") 
            print(summary(x1))
          
        }else{

            cat("\n")
            cat("Column summary table:\n")
            cat("---------------------")
            
            if (pinfo_col == "Plate Position") {
                x1 = substr(x1, 1, 1)
            }
          
            tab = -sort(-table(paste0("'", x1, "'")))
            tab_df = data.frame("Value" = names(tab),
                                "Count" = as.numeric(tab))
          
            rownames(tab_df) = 1:nrow(tab_df)

            cat("\n")
            print(tab_df)
          
        }
    }
  
    err_sel = rowSums(!x[,-1,drop=F]) > 0
    y = x[err_sel,,drop=F]
    
    if (nrow(y) > 0) {   
      cat("\n")
      cat("Status: FAIL!", "\n")

      if (pinfo_col == "Required Columns") {
            cat("\n")
            cat("Plate information sheet column(s) with errors:\n")
            cat("----------------------------------------------\n")
            pinfo_colnames = attr(x, "pinfo_colnames")
            pinfo_colnames = paste0("'", pinfo_colnames[1:11], "'")
            sel = !pinfo_colnames %in% x[,"Required_Columns"]
        
            mia_col_df = data.frame("Unrecognized_Columns"=pinfo_colnames[sel])
            print(mia_col_df)

      }else{
            cat("\n")
            cat("Plate information sheet row(s) with errors:\n")
            cat("-------------------------------------------\n")
            
            to_drop = which(apply(y[,-1,drop=F], 2, any)) + 1
            if (length(to_drop) > 0) {
                y = y[,-to_drop,drop=F]
            } 
            
            colnames(y) = gsub(" ", "_", colnames(y))
            y[,1] = paste0("'", y[,1], "'")
            y = cbind("Row"=rownames(y), y[,,drop=F])
            rownames(y) = NULL
          
            print(y)
      }  

    }else{
        cat("\n")
        cat("Status: PASS!\n")
    }
    
}

print_merge_error_msg = function(plate_nam, 
                                 merge_info,
                                 assay_position_check_summary,
                                 assay_position_check_detailed) {
    
    x = merge_info[[plate_nam]][["merge_checks"]]
    
    cat("Today's date:", as.character(Sys.Date()))
    cat("\n\n")
    cat("Data merge information:\n")
    cat("-----------------------\n\n") 
  
    if (all(x)) {
        pinfo_sid = merge_info[[plate_nam]][["pinfo_sid"]]
        assay_sid = merge_info[[plate_nam]][["assay_sid"]]

        cat(paste0(length(pinfo_sid), " samples in Plate_Information_Sheet|Platename matched\n"))
        cat(paste0(length(assay_sid), " samples in Assay_Data|Platename.\n"))
        cat("\n")
        cat("Status: PASS!")
      
    }else{
 
        check_ind = min(which(!x))
        apnam = merge_info[[plate_nam]][["key"]]
      
        # Check 1. Is Platename and Plate Position in PINFO error free?      
        if (check_ind == 1) {
            msg = paste0("Plate_Information_Sheet|Platename and/or\nPlate_Information_Sheet|Plate_Position failed.\n")
            cat(msg)
        }
      
        # Check 2. Is PINFO|Platename in FCS|PLATENAME?
        if (check_ind == 2) {
            assay_platenames = paste0("'", names(assay_position_check_summary), "'")
            assay_platenames_df = data.frame("Assay_Data|Platename"=assay_platenames,
                                             check.names = FALSE)
          
            msg = paste0("Plate_Information_Sheet|Platename = ", "'", apnam, "'",
                         "\nnot found in Assay_Data.\n\n",
                         "The following Assay_Data|Platename were detected:", "\n\n")
            
            cat(msg)
            print(assay_platenames_df)
        }
      
        # Check 3. Is FCS|WELLID error free?
        if (check_ind == 3) {
            ac = assay_position_check_detailed[[apnam]]
            colnames(ac) = gsub(" ", "_", colnames(ac))
            sel = rowSums(!ac[,-1,drop=F]) > 0
            ac = ac[sel,,drop=F]
            ac = cbind("Row"=rownames(ac), ac)
            rownames(ac) = NULL
          
            msg = paste0("Errors in Assay_Data|Plate_Position.\n\n",
                         "See table below for rows with errors:", "\n\n")
            
            cat(msg)
            cat(paste0("Assay_Data|Platename = ", "'", apnam, "'"), "\n\n")
            print(ac)
        }
      
        # Check 4. Are all SID in dat_rn?
        if (check_ind == 4) {
              pinfo_sid = merge_info[[plate_nam]][["pinfo_sid"]]
              assay_sid = merge_info[[plate_nam]][["assay_sid"]]
              mia = pinfo_sid[!pinfo_sid %in% assay_sid]
              mia_df = data.frame("Platename|Plate_Position" = paste0("'", mia, "'"),
                                  check.names = FALSE)
          
              msg = paste0("Samples in the Plate_Information_Sheet did not\nfind matches in the Assay_Data.\n\n",
                           "The non-matching sample(s) are:", "\n\n")
          
              cat(msg)
              print(mia_df)
        }

        cat("\n")
        cat("Status: FAIL!", "\n")
    }
  
}

print_error_msg = function(plate_nam, 
                           pinfo_col = NULL, 
                           checks_detailed = NULL,
                           merge_info = NULL,
                           assay_position_check_summary = NULL,
                           assay_position_check_detailed = NULL) {
    
    if (pinfo_col=="DATA MERGE") {
        print_merge_error_msg(plate_nam = plate_nam, 
                              merge_info = merge_info,
                              assay_position_check_summary = assay_position_check_summary,
                              assay_position_check_detailed = assay_position_check_detailed)
    }else{
        print_pinfo_error_msg(plate_nam = plate_nam, 
                              pinfo_col = pinfo_col, 
                              checks_detailed = checks_detailed)
    }

}
