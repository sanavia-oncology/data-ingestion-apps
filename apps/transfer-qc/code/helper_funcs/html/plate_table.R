
plate_table = function() {
    # params
    nr = 8
    nc = 12
  
    width = "width: 30px;"
    height = "height: 30px;"
  
    cell_style = paste(width,
                       height,
                       "font-size: 9px;",
                       "border: 1px solid #0088dd;",
                       "vertical-align: middle;",
                       "text-align: center;",
                       "margin: 0;")

    bs = paste(width, 
               height, 
               "border-radius: 15px;")

    # Column names  
    col_nam_row = tagList()
  
    col_nam_row[[1]] = tags$td("")
    for (i in 1:nc) {
        col_nam_row[[i+1]] = tags$td(i, style="text-align: center;")
    }
    col_nam = tags$tr(col_nam_row)

    # Row elements
    tab_row_list = tagList()
  
    for (k in 1:nr) {
        rnam = LETTERS[k]
        tab_row = tagList()
        tab_row[[1]] = tags$td(rnam)
    
        for (i in 1:nc) {
            id = paste0(rnam, i)
            
            id_b = tags$button(id, style=bs, class="well-btn")
          
            tab_row[[i+1]] = tags$td(id_b, style=cell_style)
        }
      
        tab_row_list[[k]] = tags$tr(tab_row)
    }


  res = tags$table(
            tags$thead(col_nam),
            tags$tbody(tab_row_list)
          )
  
  return(res)
}
