# author: Kwame Okrah
# date: 2025-11-14

required_columns_check = function(sheet_colnames) {
    # required Columns
    required_columns = c("Platename", 
                         "Plate Position",
                         "Creator",
                         "Target Spec Type",
                         "Target Spec ID", 
                         "Probe Type", 
                         "Probe ID",
                         "Probe Quant Type",
                         "Probe Quant Value",
                         "Primary Role",
                         "Subrole")
    
    is_available = required_columns %in% sheet_colnames

    res = data.frame("Required Columns" = required_columns,
                     "Is Available?" = is_available,
                     check.names = FALSE)
    
    attr(res, "pinfo_colnames") = sheet_colnames
     
    return(res)
}