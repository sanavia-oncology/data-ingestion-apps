# author: Kwame Okrah
# date: 2025-11-14

probe_id_check = function(probe_id) {
    if (!is.null(probe_id)) {
        x = probe_id
        
        x[is.na(x)] = ""
        is_available = !(x == "")
    
        res = data.frame("Probe ID" = probe_id,
                        "Is Available?" = is_available,
                        check.names = FALSE)
    }else{
        res = data.frame("Probe ID" = "Is Missing",
                         "Is Available?" = FALSE,
                         check.names = FALSE)
    }

    return(res)
}