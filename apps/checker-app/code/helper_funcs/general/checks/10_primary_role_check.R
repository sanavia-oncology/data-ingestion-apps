# author: Kwame Okrah
# date: 2025-11-14

primary_role_check = function(primary_role) {
    if (!is.null(primary_role)) {
        x = primary_role
    
        x[is.na(x)] = ""
        is_available = !(x == "")

        expected_primary_role = c("sample", "control", "standard", "others")

        right_format = x %in% expected_primary_role
    
        res = data.frame("Primary Role" = primary_role,
                         "Is Available?" = is_available,
                         "Right Format?" = right_format,
                         check.names = FALSE)
    }else{
        res = data.frame("Primary Role" = "Is Missing",
                         "Is Available?" = FALSE,
                         "Right Format?" = FALSE,
                         check.names = FALSE)
    }
  
    return(res)
}
