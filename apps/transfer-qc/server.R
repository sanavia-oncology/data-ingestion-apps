# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    # step0: set project_fldr
    project_fldr = "/Users/kwameokrah/data_depo/flow-cytometry"
    
    # step1: organize file paths by order
    proj_paths = tryCatch(
        get_paths_by_project(project_fldr),
        error = function(e) {
            error_msg = "'get_paths_by_project()' an error occurred (K.Okrah)"
            return(error_msg)
        }
    )
    
    # step2: make front table
    observe({
        if (isTruthy(input$selected_assay)) {
            removeUI(selector = "#main_contents1")
        }
        
        
        #---------------------- welcome note
        if (input$selected_assay=="") {
            insert_me1 = tags$p("Welcome, please select an assay type.", 
                                class="h5 text-secondary")
        }
        
        
        #---------------------- flow cytometry
        if (input$selected_assay=="fcs") {
            print(input$selected_assay)  
            
            # make front page
            if (!is.character(proj_paths)) {
                table_front_page = tryCatch(
                    front_page_table(proj_paths, input$selected_assay),
                    error = function(e) {
                        error_msg = "'front_page_table()' an error occurred (K.Okrah)"
                        return(error_msg)
                    }
                )
                
                if (!is.character(table_front_page)) {
                    
                    # table definition
                    output$table = DT::renderDataTable(DT::datatable({
                        data = table_front_page
                        if (input$has_qcr != "All") {
                            data = data[data[["Has QC Report"]] == input$has_qcr,]
                        }
                        data
                    }, 
                    selection = "single",
                    options = list(pageLength = 7,  
                                   dom = "tpf",
                                   columnDefs = list(
                                       list(className = 'dt-nowrap', targets = '_all'))
                    )))
                    
                    # table insert
                    insert_me1 = tags$div(
                        tags$p("Select A Project", 
                               class="h3 text-primary fw-bold text-center"),
                        tags$p("Click on a row to select project and proceed to QC",
                               class="h6 text-secondary text-center"),
                        fluidRow(
                            selectInput("has_qcr",
                                        "Has QC Report",
                                        c("All", "No", "Yes"))
                        ),
                        DT::dataTableOutput("table")
                    )
                    
                }else{
                    
                    # error message
                    insert_me1 = tags$div(
                        tags$p(table_front_page, 
                               class="h5 text-danger")
                    )
                    
                }

            }else{
                
                # error message
                insert_me1 = tags$div(
                    tags$p(proj_paths, 
                           class="h5 text-danger")
                )
                
            }
        }
        
        
        #---------------------- other assays: not implemented
        if (!input$selected_assay %in% c("fcs", "")) {
            print(input$selected_assay)
            
            # table insert
            insert_me1 = tags$div(
                tags$p("Select A Project", 
                       class="h3 text-primary fw-bold text-center"),
                tags$p("Click on a row to select project and proceed to QC",
                       class="h6 text-secondary text-center"),
                fluidRow(
                    selectInput("has_qcr",
                                "Has QC Report",
                                c("All", "No", "Yes"))
                ),
                tags$p(paste0("(", input$selected_assay, ") coming soon!"),
                       class="h6 text-danger")
            )
        }
        
        
        #---------------------- insert UI
        insertUI(
            selector = "#main_contents",
            where    = "afterEnd",
            ui = tags$div(
                id = "main_contents1",
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col",
                        id    = "main_contents_col1",
                        insert_me1
                    ),
                    tags$div(
                        class = "col",
                        id    = "main_contents_col2",
                        tags$div(id = "main_contents_col2_1"),
                        # do nothing
                    )
                )
            )
        )

    }) |> bindEvent(input$selected_assay)
    
    

}