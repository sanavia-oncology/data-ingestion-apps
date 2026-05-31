# Kwame Okrah
# 2025-11-18

qc_table = function(error_matrix, show_rownames=TRUE) {
    error_matrix = t(error_matrix)
    if (ncol(error_matrix) == 1) {
        colnames(error_matrix) = "QC_00"
    }else{
        colnames(error_matrix) = paste0("QC_", sprintf(1:ncol(error_matrix),fmt="%02d"))
    }
    
    # params
    nr = nrow(error_matrix)
    qc_check_groups = colnames(error_matrix)
    nc = length(qc_check_groups)
    
    width = "width: 45px;"
    height = "height: 35px;"

    font_size = "font-size: 10px";

    cell_style = paste(width,
                       height,
                       "border: 1px solid #f0efefff;",
                       "vertical-align: middle;",
                       "text-align: center;",
                       "margin: 0;")

    bs = paste(width,
               height,
               "font-size: 13px;",
               "border-radius: 0px;",
               "border: 0px solid #ffffff;")

    col_nam_row = tagList()
    col_nam_row[[1]] = tags$th("")
    for (i in 1:nc) {
        col_nam_row[[i+1]] = tags$th(
                                  style = paste0("text-align: center;", font_size),
                                  qc_check_groups[i])
    }
    col_nam = tags$tr(col_nam_row)

    tab_row_list = tagList()
  
    RNAM = rownames(error_matrix)
    
    for (k in 1:nr) {
        rnam = RNAM[k]
        
        tab_row = tagList()

        if (show_rownames) {
            tab_row[[1]] = tags$th(rnam, style=paste("width: 50px;",
                                                     "text-align: right;",
                                                     font_size))
        }else{
            tab_row[[1]] = tags$th("")
        }

      
        for (i in 1:nc) {
            cell_msg = error_matrix[rnam, i]
          
            if (cell_msg=="Pass") {
                bs_update = paste(bs, "background-color: #51b551ff;")
                
                cell_id = paste0(rnam, "-", gsub(" ", "_", qc_check_groups[i]))
                
                cell_msg_b = tags$button(cell_msg, 
                                         style = bs_update, 
                                         class = "cell-btn",
                                         id = cell_id,
                                         onclick = "")
                
            }else{
                bs_update = paste(bs, "background-color: #e54c4cff;")
                
                cell_id = paste0(rnam, "-", gsub(" ", "_", qc_check_groups[i]))
                
                cell_msg_b = tags$button(cell_msg, 
                                         style = bs_update, 
                                         class = "cell-btn",
                                         id = cell_id,
                                         onclick = "sendButtonID(this.id)")
            }
            
            # cell_id = paste0(rnam, "-", gsub(" ", "_", qc_check_groups[i]))
            # 
            # cell_msg_b = tags$button(cell_msg, 
            #                          style = bs_update, 
            #                          class = "cell-btn",
            #                          id = cell_id,
            #                          onclick = "sendButtonID(this.id)")
          
            tab_row[[i+1]] = tags$td(cell_msg_b, style=cell_style)
        }
      
        tab_row_list[[k]] = tags$tr(tab_row)
    }

    res = tags$div(
        tags$table(
            tags$thead(col_nam),
            tags$tbody(tab_row_list))
    )

    return(res)
}

