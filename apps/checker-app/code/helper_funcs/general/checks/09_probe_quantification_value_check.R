# author: Kwame Okrah
# date: 2025-11-14

probe_quantification_value_check = function(probe_quantification_value) {
    if (!is.null(probe_quantification_value)) {
        x = probe_quantification_value

        # allow NAs (no checks -- always)
        is_available = rep(TRUE, length(x))

        res = data.frame("Probe Quant Value" = probe_quantification_value,
                         "Is Available?" = is_available,
                        check.names = FALSE)
    }else{
        res = data.frame("Probe Quant Value" = "Is Missing",
                         "Is Available?" = FALSE,
                         check.names = FALSE)
    }

    return(res)
}
