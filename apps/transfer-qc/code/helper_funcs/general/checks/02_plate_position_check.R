# author: Kwame Okrah
# date: 2025-11-14
 
plate_position_check = function(plate_position) {
    if (!is.null(plate_position)) {    
        x = plate_position
        x[is.na(x)] = ""
        is_available = !(x == "")

        is_distinct = !duplicated(x)
        
        well_rows = c("A","B","C","D","E","F","G","H")
        expected_wells = unlist(lapply(well_rows, function(r) paste0(r, seq_len(12))))

        right_format = x %in% expected_wells
        
        res = data.frame("Plate Position" = plate_position,
                         "Is Available?" = is_available,
                         "Is Distinct?" = is_distinct,
                         "Right Format?" = right_format,
                         check.names = FALSE)

    }else{
        
        res = data.frame("Plate Position" = "Is Missing",
                         "Is Available?" = FALSE,
                         "Is Distinct?" = FALSE,
                         "Right Format?" = FALSE,
                         check.names = FALSE)

    }
  
    return(res)
}