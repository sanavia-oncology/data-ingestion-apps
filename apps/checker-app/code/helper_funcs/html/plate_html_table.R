
plate_html_table = function(msg_vec=NULL) {

    if (is.null(msg_vec)) {
        msg_vec = c("A1"="Hello", 
                    "A2"="Kwame", 
                    "B5"="How", 
                    "H8"="are", 
                    "C12"="you",
                    "E6"="?")
    }

    show_rownames = TRUE
    show_colnames = TRUE
    
    df = matrix(NA, nrow = 8, ncol = 12)
    rownames(df) = LETTERS[1:nrow(df)]
    colnames(df) = as.character(1:ncol(df))
    msg_df = df
    
    for (i in rownames(df)) {
        for (j in colnames(df)) {
            df[i, j] = paste0(i, j)
        }
    }
     
    for (nam in names(msg_vec)) {
        if (nam %in% df) {
            nam1 = substr(nam, 1, 1)
            nam2 = gsub("[A-Z]", "", nam)
            msg_df[nam1, nam2] = msg_vec[nam]
        }
    }
    
    # table info
    NCHAR = apply(df, 2, function(x) nchar(as.character(x)))
    colwidths = apply(NCHAR, 2, max, na.rm=T)
    
    column_names_vec = colnames(df)
    RNAM = rownames(df)
    
    nc = length(column_names_vec)
    nr = length(RNAM)
    
    # style info. (colnames size)
    font_size = "font-size: 13px";
    
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
    
    # style info. (data cells and buttons)
    width = "width: 34px;"
    height = "height: 34px;"
    
    cell_style = paste(width,
                       height,
                       "font-size: 9px;",
                       "border: 1px solid #0088dd;",
                       "vertical-align: middle;",
                       "text-align: center;",
                       "margin: 0;")
    
    bs = paste(width, 
               height, 
               "border: 2px solid white;",
               "border-radius: 17px;")
    
    # make row data inserts
    tab_row_list = tagList()
    
    for (k in 1:nr) {
        rnam = RNAM[k]
        
        tab_row = tagList()
        
        if (show_rownames) {
            tab_row[[1]] = tags$th(rnam, style=font_size)    
        }else{
            tab_row[[1]] = tags$th("", style=font_size)
        } 
        
        for (i in 1:nc) {
            
            cell_msg = df[rnam, i]
            cell_id = paste0(rnam, "-", gsub(" ", "_", column_names_vec[i]))
            
            msg = msg_df[rnam, i]
            
            if (is.na(msg)) {
                cell_msg_b = tags$button(cell_msg, 
                                         style = bs,
                                         id = cell_id,
                                         title = "")
            }else{
                bs_update = paste(bs, "background-color: #5c64b1ff;")
                
                cell_msg_b = tags$button(cell_msg, 
                                         style = bs_update,
                                         id = cell_id,
                                         title = msg,
                                         class = "cell-btn")
            }

            tab_row[[i+1]] = tags$td(cell_msg_b, 
                                     style = cell_style)
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
