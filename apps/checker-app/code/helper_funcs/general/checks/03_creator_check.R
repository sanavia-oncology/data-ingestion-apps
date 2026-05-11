# author: Kwame Okrah
# date: 2025-11-14

creator_check = function(creator) {
    if (!is.null(creator)) {
        x = creator
    
        x[is.na(x)] = ""
        is_available = !(x == "")
    
        is_identical = x == names(sort(-table(x)))[1]
        
        expected_creators = c("silvana.digiandomenico",  
                              "nan.chen",
                              "brendan.buehler",
                              "maria.sjostrand",
                              "remy.schneider",
                              "glenn.gregorio",
                              "bernardo.reis",
                              "megan.mccloskey",
                              "louis.mattera",
                              "kwame.okrah",
                              "karl.sebby",
                              "server.ertem",
                              "valentina.marchionni",
                              "lab.user")
    
        right_format = x %in% expected_creators
    
        res = data.frame(Creator = creator,
                        "Is Available?" = is_available,
                        "Is Identical?" = is_identical,
                        "Right Format?" = right_format,
                        check.names = FALSE)      
    }else{
        res = data.frame(Creator = "Is Missing",
                         "Is Available?" = FALSE,
                         "Is Identical?" = FALSE,
                         "Right Format?" = FALSE,
                         check.names = FALSE) 
    }

  
    return(res)
}