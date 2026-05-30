# author: Kwame Okrah
# date: 2025-11-14

probe_quantification_type_check = function(probe_quantification_type) {
    if (!is.null(probe_quantification_type)) {
        x = probe_quantification_type

        x[is.na(x)] = ""
        is_available = !(x == "")
    
        is_identical = x == names(sort(-table(x)))[1]

        expected_probe_quant_type = c("concentration (ug/ml)",
                                      "e:t ratio",
                                      "dilution factor",
                                      "car-t dilution",
                                      "cell dilution",
                                      "others")

        right_format = x %in% expected_probe_quant_type
    
        res = data.frame("Target Spec Type" = probe_quantification_type,
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
