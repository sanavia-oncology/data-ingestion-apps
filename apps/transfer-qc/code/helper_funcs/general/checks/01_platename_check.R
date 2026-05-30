# author: Kwame Okrah
# date: 2025-11-14

platename_check = function(platename) {
    if (!is.null(platename)) {
        x = platename

        x[is.na(x)] = ""
        is_available = !(x == "")
        
        is_identical = x == names(sort(-table(x)))[1]
        
        res = data.frame(Platename = platename,
                        "Is Available?" = is_available,
                        "Is Identical?" = is_identical,
                        check.names = FALSE)
        
    }else{
        res = data.frame(Platename = "Is Missing",
                "Is Available?" = FALSE,
                "Is Identical?" = FALSE,
                check.names = FALSE)
    }

    return(res)
}