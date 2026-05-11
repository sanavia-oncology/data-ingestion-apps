# author: Kwame Okrah
# date: 2025-11-14

target_specimen_type_check = function(target_specimen_type) {
    if (!is.null(target_specimen_type)) {
        x = target_specimen_type

        x[is.na(x)] = ""
        is_available = !(x == "")
    
        is_identical = x == names(sort(-table(x)))[1]
        
        expected_target_specimen_type = c("cell", "serum", 
                                          "tissue", "protein", 
                                          "peptide", "other")
        right_format = x %in% expected_target_specimen_type
    
        res = data.frame("Target Spec Type" = target_specimen_type,
                         "Is Available?" = is_available,
                         "Is Identical?" = is_identical,
                         "Right Format?" = right_format,
                         check.names = FALSE)
    }else{
        res = data.frame("Target Spec Type" = "Is Missing",
                         "Is Available?" = FALSE,
                         "Is Identical?" = FALSE,
                         "Right Format?" = FALSE,
                         check.names = FALSE)
    }

    return(res)
}
