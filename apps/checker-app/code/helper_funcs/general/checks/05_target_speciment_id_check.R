# author: Kwame Okrah
# date: 2025-11-14

target_speciment_id_check = function(targed_specimen_id) {
    if (!is.null(targed_specimen_id)) {
        x = targed_specimen_id

        x[is.na(x)] = ""  
        is_available = !(x == "")

        res = data.frame("Target Spec ID" = targed_specimen_id,
                         "Is Available?" = is_available,
                         check.names = FALSE)
    }else{
        res = data.frame("Target Spec ID" = "Is Missing",
                         "Is Available?" = FALSE,
                         check.names = FALSE)
    }
  
    return(res)
}
