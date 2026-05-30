# author: Kwame Okrah
# date: 2025-11-14

subrole_check = function(subrole) {
    if (!is.null(subrole)) {
        x = subrole
    
        x[is.na(x)] = ""
    
        expected_subrole = c("negative", 
                             "positive", 
                             "max-kill ctrl",
                             "min-kill ctrl",
                             "reference", 
                             "others",
                             "")
        right_format = x %in% expected_subrole    
    
        res = data.frame("Subrole" = subrole,
                         "Right Format?" = right_format,
                         check.names = FALSE)
    }else{
        res = data.frame("Subrole" = "Is Missing",
                         "Right Format?" = FALSE,
                         check.names = FALSE)
    }

    return(res)
}