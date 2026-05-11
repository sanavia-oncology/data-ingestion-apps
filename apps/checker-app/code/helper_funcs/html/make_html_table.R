# Kwame Okrah
# 2025-11-19

make_html_table = function(df, 
                           show_rownames = TRUE, 
                           show_colnames = TRUE,
                           font_size = "font-size: 12px;",
                           height = "height: 30px;") {

    # table info
    nc = ncol(df)
    nr = nrow(df)
    
    if (nr > 1) {
        NCHAR = apply(df, 2, function(x) nchar(as.character(x)))
        colwidths = apply(NCHAR, 2, max, na.rm=T)
    }else{
        colwidths = sapply(df[1,], function(x) nchar(as.character(x)))
    }

    column_names_vec = colnames(df)
    RNAM = rownames(df)
    
    if (is.null(RNAM)) {
        RNAM = as.character(sprintf(1:nr, fmt="%02d"))
        rownames(df) = RNAM
    } 
    
    if (is.null(column_names_vec)) {
        column_names_vec = as.character(sprintf(1:nc, fmt="%02d"))
        colnames(df) = column_names_vec
    } 
    
    # # style info. (colnames size)
    # font_size = "font-size: 12px;"
    
    # make colnames
    col_nam_row = tagList()
    
    col_nam_row[[1]] = tags$th("")
    for (i in 1:nc) {
        
        if (show_colnames) {
            col_nam_row[[i+1]] = tags$th(
                style = paste0("text-align: center;", font_size),
                column_names_vec[i])
        }else{
            col_nam_row[[i+1]] = tags$th(
                style = paste0("text-align: center;", font_size),
                "")
        }

    }
    col_nam = tags$tr(col_nam_row)
    

    # # style info. (data cells and buttons)
    # height = "height: 30px;"
    
    cell_style = paste(height,
                       "border: 1px solid #9c9a9aff;",
                       "vertical-align: middle;",
                       "text-align: center;",
                       "margin: 0;")
    
    bs = paste(height,
               "font-size: 12px;",
               "border-radius: 5px;",
               "border: 1px solid #ffffff;")
    
    # make row data inserts
    tab_row_list = tagList()
    
    for (k in 1:nr) {
        rnam = RNAM[k]
        
        tab_row = tagList()
        
        if (show_rownames) {
            tab_row[[1]] = tags$td(rnam, style=font_size)    
        }else{
            tab_row[[1]] = tags$td("", style=font_size)
        } 
        
        
        for (i in 1:nc) {
            
            width = ifelse(colwidths[i] <= 5, "width: 70px;",
                           ifelse(colwidths[i] <= 10, "width: 80px;", 
                                  ifelse(colwidths[i] <= 15, "width: 90px;", 
                                         "width: 150px;")))

            bs_update = paste(bs, width)
            cell_style_update = paste(cell_style, width)
            
            cell_msg = df[rnam, i]
            cell_id = paste0(rnam, "-", gsub(" ", "_", column_names_vec[i]))
            
            cell_msg_b = tags$button(cell_msg, 
                                     style = bs_update,
                                     id = cell_id)
            
            tab_row[[i+1]] = tags$td(cell_msg_b, 
                                     style = cell_style_update)
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
