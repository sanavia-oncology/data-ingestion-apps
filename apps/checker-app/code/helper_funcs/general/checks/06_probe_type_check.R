# author: Kwame Okrah
# date: 2025-11-14

probe_type_check = function(probe_type) {
    if (!is.null(probe_type)) {
        x = probe_type
        
        x[is.na(x)] = ""
        is_available = !(x == "")
    
        is_identical = x == names(sort(-table(x)))[1]

        expected_probe_types = c("antibody", "cell", "car-t", "adc", "other")
        right_format = x %in% expected_probe_types
    
        res = data.frame("Probe Type" = probe_type,
                         "Is Available?" = is_available,
                         "Is Identical?" = is_identical,
                         "Right Format?" = right_format,
                         check.names = FALSE)
    }else{
        res = data.frame("Probe Type" = "Is Missing",
                         "Is Available?" = FALSE,
                         "Is Identical?" = FALSE,
                         "Right Format?" = FALSE,
                         check.names = FALSE)
    }
  
    return(res)
}
