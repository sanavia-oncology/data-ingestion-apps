# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    # Step 0: Get data file paths
    input_files = reactiveValues()
    
    roots = c(Projects = "/Users/kwameokrah/claude_code/shiny/data/flow-cytometry")
    shinyDirChoose(input, "folder", roots = roots, allowDirCreate = FALSE)
    
    observe({

        if (isTruthy(input$folder)) {
            removeUI(selector = "#loaded_folder_div")
        }
        if (isTruthy(input$folder)) {
            removeUI(selector = "#main_contents1")
        }
        
        req(input$folder)
        folder_path = parseDirPath(roots, input$folder)
        req(nchar(folder_path) > 0)

        paths = list.files(folder_path, full.names = TRUE, recursive = TRUE)
        fnam = sapply(strsplit(folder_path, "/"), function(x) x[length(x)])
   
        output$folder_mesage_box = renderPrint({
            cat("Selected project:\n")
            cat(fnam)
            cat("\n")
        })
        
        insertUI(
            selector = "#folder", 
            where = "afterEnd",
            
            ui = tags$div(
                id = "loaded_folder_div",
                card(
                    verbatimTextOutput("folder_mesage_box", placeholder = TRUE)
                ),
            
                actionButton(
                    inputId = "read_data",
                    label = "Read Flow Cytometry Data",
                    class = "btn-secondary w-100"),
            )
        )
        
        input_files$folder_path = folder_path
        input_files$paths = paths

    }) |> bindEvent(input$folder)
    
    # Step 1: Read in data
    input_react_vals = reactiveValues()
    
    observe({
        if (isTruthy(input$read_data)) {
            removeUI(selector = "#loaded_files_div")
        }
        if (isTruthy(input$read_data)) {
            removeUI(selector = "#main_contents1")
        }
        if (isTruthy(input$read_data)) {
            removeUI(selector = "#table_page")
        }

        res = read_data(input_files$paths)
        pinfos = res[["pinfos"]]
        fcs_files = res[["fcs_files"]]

        n_plates = length(unique(pinfos$Platename))
        n_fcs = length(fcs_files)

        if (n_plates==1) {
            msg_pinfo = paste0(n_plates, " plate info. sheet")
        }else{
            msg_pinfo = paste0(n_plates, " plate info. sheets")
        }
        
        if (n_fcs==1) {
            msg_assay = paste0(n_fcs, " fcs file")
        }else{
            msg_assay = paste0(n_fcs, " fcs files") 
        }
        
        insertUI(
            selector = "#read_data", 
            where = "afterEnd",
            ui = tags$div(
                id = "loaded_files_div",
                tags$br(),
                tags$p(
                    class = "text-muted",
                    "Loading complete!"),
                card(
                    card_image("www/img/data_loaded2.jpg"),
                    card_body(tags$p(
                        class = "fs-6",
                        msg_pinfo,
                        tags$br(),
                        msg_assay)
                    ),
                    actionButton(
                        inputId = "proceed",
                        label = "Proceed",
                        class = "btn-primary")
                ),
            )
        )

        input_react_vals$pinfos = pinfos
        input_react_vals$fcs_files = fcs_files
        
    }) |> bindEvent(input$read_data)
    
    # Step 3: Summarize total events data
    observe({
        if (isTruthy(input$proceed)) {
            removeUI(selector = "#main_contents1")
        }
        if (isTruthy(input$proceed)) {
            removeUI(selector = "#table_page")
        }
      
        pinfos = input_react_vals$pinfos

        tab = summary_events(pinfos$Result, "Total Events")
        output$stat_summary_box = renderPrint({ tab })
        
        # plot total events
        reset_dir("/Users/kwameokrah/shiny/demos/app-fc/www/docs/tlgs/")

        file_path = paste0("/Users/kwameokrah/shiny/demos/app-fc/www/docs/tlgs/",
                           paste0(Sys.Date(), "_plate-events-fig.pdf"))

        plot_plate_events(pinfos, is_gated = FALSE, fig_path = file_path)
        
        # events boxplot
        output$plot = renderPlot({
            events_boxplot(pinfos$Result)
        })

        # column1
        insert_me1 = tags$div(
            id = "ref_sample_gating_div",
            tags$div(
                id = "ref_sample_gating_div2",
                tags$p("Optimal Viability Gate Detection", 
                       class="h5 text-primary fw-bold"),
                tags$p("Computationaly determine optimal gates for all samples up to viabliliy.",
                       class="text-secondary")
            ),
            tags$div(
                actionButton("automatic_gate", 
                             "Automatically Gate up to Viability",
                             class="btn-warning")
            )
        )
        
        # column2
        insert_me2 = tags$div(
            id = "gating_summary_div",
            
            tags$div(
                class="row",
                
                tags$p("Total Number of Events (Not Gated)", 
                       class="h5 text-secondary fw-bold"),

                tags$div(
                    style="height: 340px",
                    tags$iframe(
                        src=paste0("docs/tlgs/", Sys.Date(), "_plate-events-fig.pdf"),
                        width="100%",
                        height="100%"
                    )
                ),
                
                tags$p("Each page corresponds to a single plate.",
                       class="text-secondary")
            ),

            tags$div(
                class="row",

                tags$div(
                    class="row",
                    tags$div(plotOutput("plot", height = "110px"))
                ),
                
                tags$div(
                    class="row",
                    tags$div(
                         style = "width: 100%;",
                         verbatimTextOutput("stat_summary_box", placeholder = TRUE)
                    )
                )
            )
        )
        
        insertUI(
            selector = "#main_contents",
            where = "afterEnd",
            
            ui = tags$div(
                id = "main_contents1",
                
                tags$div(
                    class="row",
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col1",
                        insert_me1    
                    ),
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col2",
                        tags$div(
                            id = "main_contents_col2_topmark_div",
                        ),
                        insert_me2
                    )
                )
            )
        )
        
    }) |> bindEvent(input$proceed)
  
    # Step 4: Automatically gate up to viability
    optimal_verts_react_vals = reactiveValues()
  
    observe({
        # select refrence samples
        pinfos = input_react_vals$pinfos
        fcs_files = input_react_vals$fcs_files

        ref = rownames(pinfos)
        refl = split(ref, pinfos$Platename)

        nsel = 5
        fref = lapply(refl, function(x) {
            if (length(x) <= nsel) {
                return(x)
            }else{
                return(x[1:nsel])
            }
        })

        sel_refs = unlist(fref)
        fcs_refs = fcs_files[sel_refs]

        print("computing optimal gates - start")
        optimal_vertices_res = compute_optimal_vertices(fcs_refs, default_ch)
        print("computing optimal gates - end")
        
        output$plot_vertices = renderPlot({
            plot_optimal_vertices(optimal_vertices_res)
        })
      
        insertUI(
            selector = "#ref_sample_gating_div", 
            where = "afterEnd",
            ui = tags$div(
                id = "auto_viability_gate_results",

                tags$div(
                    tags$br(),
                    tags$p("Computation complete!")
                ),
                tags$p("Inspect automatic gates", 
                       class="h5 text-primary"),
                tags$p(paste0("The sample below is a random sampling of events used for ",
                               "qualitative assessment of the automatic gates (not a real ",
                               "sample)."),
                       class="text-secondary"),
                tags$div(
                    tags$div(plotOutput("plot_vertices", height = "225px"))
                ),

                tags$br(),
                tags$p("Gate all samples", 
                       class="h5 text-primary"),
                tags$p("Apply the gates above to all samples in the project.",
                       class="text-secondary"),
                tags$div(
                    actionButton("gate_all", 
                                 "Apply the Gates to All Samples",
                                 class="btn-secondary"),
                )
            )
        )
      
      optimal_verts_react_vals$verts = optimal_vertices_res[["optim_verts_list"]]
      optimal_verts_react_vals$default_ch = optimal_vertices_res[["default_ch"]]
      
    }) |> bindEvent(input$automatic_gate)
  
    # Step 5: Gate all samples
    gated_fcs_files = reactiveValues()
    
    observe({
        if (isTruthy(input$automatic_gate)) {
            removeUI(selector = "#main_contents1")
        }
        
        pinfos = input_react_vals$pinfos
        fcs_files = input_react_vals$fcs_files
        verts = optimal_verts_react_vals$verts
        
        # set the MFI channel
        default_ch = optimal_verts_react_vals$default_ch
        default_ch["ab+", "x_ch"] = pinfos[["mfi_channel"]][1]
      
        # gate all samples
        gres_list = list()
      
        for (k in names(fcs_files)) {            
            fcs = fcs_files[[k]]
            mat = flowCore::exprs(fcs)
            result = tryCatch({
                gate2_viability(mat, default_ch, verts)
            }, error = function(e) {
                NA
            })
            gres_list[[k]] = result
        }

        results_table = make_results_table(gres_list, pinfos)

        tab = summary_viable_events(results_table)
        output$stat_summary_viable_box = renderPrint({ tab })
        
        # plot total viable events
        file_path1 = paste0("/Users/kwameokrah/shiny/demos/app-fc/www/docs/tlgs/",
                            paste0(Sys.Date(), "_plate-viable-events-fig.pdf"))

        plot_plate_events(results_table, is_gated = TRUE, fig_path=file_path1)
      
        # plot mfi
        file_path2 = paste0("/Users/kwameokrah/shiny/demos/app-fc/www/docs/tlgs/",
                            paste0(Sys.Date(), "_plate-viable-mfi-fig.pdf"))

        plot_mfi(results_table, fig_path=file_path2)
        
        # events boxplot
        output$plot_viable = renderPlot({
            events_boxplot(results_table$Result, results_table[, "/intact/singlet/viable"])
        })
              
        # column1
        insert_me1 = tags$div(
            id = "mfi_summary_viable_div",

            tags$div(
                class="row",

                tags$p("Results: Log10 MFI of Viable Cells", 
                       class="h5 text-primary fw-bold"),

                tags$div(
                    style="height: 340px",
                    tags$iframe(
                        src=paste0("docs/tlgs/", Sys.Date(), "_plate-viable-mfi-fig.pdf"),
                        width="100%",
                        height="100%"
                    )
                ),
                
                tags$p("Each page corresponds to a single plate.",
                       class="text-secondary")
            ),

            tags$div(
                class="row",

                tags$div(
                    class="row",
                    tags$p("Go to Review and Annotation Page", 
                        class="h5 text-secondary"),
                        tags$p("Review results sample by sample and make annotations for downstream analysis.",
                            class="text-secondary"),
                        tags$div(
                            actionButton("review_board", 
                                        "Review and Annotate Samples",
                                        class="btn-secondary"),
                    )
                ),


            )
        )
        
        # column2
        insert_me2 = tags$div(
            id = "gating_summary_viable_div",

            tags$div(
                class="row",
                
                tags$p("Total Number of Viable Cells", 
                       class="h5 text-primary fw-bold"),
                
                tags$div(
                    style="height: 340px",
                    tags$iframe(
                        src=paste0("docs/tlgs/", Sys.Date(), "_plate-viable-events-fig.pdf"),
                        width="100%",
                        height="100%"
                    )
                ),
                
                tags$p("Each page corresponds to a single plate (black dots = total events; blue dots = viable cells).",
                       class="text-secondary")
            ),

            tags$div(
                class="row",

                tags$div(
                    class="row",
                    tags$div(plotOutput("plot_viable", height = "110px"))
                ),
                
                tags$div(
                    class="row",
                    tags$div(
                         style = "width: 100%;",
                         verbatimTextOutput("stat_summary_viable_box", placeholder = TRUE)
                    )
                )
            )
        )
        
        insertUI(
            selector = "#main_contents",
            where = "afterEnd",
            
            ui = tags$div(
                id = "main_contents1",
                
                tags$div(
                    class="row",
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col1",
                        insert_me1   
                    ),
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col2",
                        tags$div(
                            id = "main_contents_col2_topmark_div",
                        ),
                        insert_me2
                    )
                )
            )
        )
      
        gated_fcs_files$results_table = results_table
        gated_fcs_files$gres_list = gres_list
      
    }) |> bindEvent(input$gate_all)
  
    # Step 6: Annotation page
    annot_react_vals = reactiveValues()
  
    observe({
        if (isTruthy(input$review_board)) {
            removeUI(selector = "#main_contents1")
        }
        
        results_table_all = gated_fcs_files$results_table
        gres_list = gated_fcs_files$gres_list
      
        scol = c("Platename", "Plate Position", "Primary Role",
                 "/intact/singlet/viable", "Cells Neg", "Log10 MFI")

        results_table = results_table_all[,scol]
        results_table$"Log10 MFI" = sprintf("%0.3f", results_table$"Log10 MFI")
        results_table$"Cells Neg" = sprintf("%0.2f", results_table$"Cells Neg"*100)
        rownames(results_table) = NULL
        
        plate_cols = gsub("[A-Z]", "", results_table[,"Plate Position"])
        plate_rows = gsub("[0-9]|[0-9]", "", results_table[,"Plate Position"])
        
        colnames(results_table)[colnames(results_table) == "/intact/singlet/viable"] = "Viable"
        colnames(results_table)[colnames(results_table) == "Plate Position"] = "Well"
        colnames(results_table)[colnames(results_table) == "Primary Role"] = "Role"
        Cells_Neg = paste0(results_table_all[1,"mfi_channel"], " Neg (%)")
        colnames(results_table)[colnames(results_table) == "Cells Neg"] = Cells_Neg

        uplatnam = unique(as.character(results_table$Platename))
        results_table = cbind(plate_rows = plate_rows, 
                              plate_cols = plate_cols, 
                              results_table)
      
        # initialize annotation container 
        annot_react_vals$annot_vector = c()
      
        # Filter data based on selections
        output$table <- DT::renderDataTable(DT::datatable({
            data <- results_table
            if (input$platename != "All") {
                data <- data[data$Platename == input$platename,,drop=F]
            }
            if (input$plate_rows != "All") {
                data <- data[data$plate_rows == input$plate_rows,,drop=F]
            }
            if (input$plate_cols != "All") {
                data <- data[data$plate_cols == input$plate_cols,,drop=F]
            }
            data
        }, selection = "single",
           options = list(pageLength = 10,  
                          dom = "tp",
                          columnDefs = list(
                                            list(visible = FALSE, targets = c(1, 2)),
                                            list(className = 'dt-nowrap', targets = '_all'))
                         )))
      
        # column1
        insert_me1 = tags$div(
            id = "annotation_table_div",
            
            tags$div(
                id = "annotation_table_div1",
                class="row",

                tags$p("Click on Row to Select Sample", 
                    class="h5 text-primary fw-bold"),
                
                tags$div(
                    class = "col",
                    id = "table_page_col1",
                    fluidRow(
                        column(8,
                            selectInput("platename",
                                        "Platename",
                                        c("All", uplatnam))
                        ),
                        column(2,
                            selectInput("plate_rows",
                                        "Plate Row",
                                        c("All",
                                          unique(as.character(results_table$plate_rows))))
                        ),
                        column(2,
                            selectInput("plate_cols",
                                        "Plate Column",
                                        c("All",
                                          unique(as.character(results_table$plate_cols))))
                        )
                    ),
                    
                    DT::dataTableOutput("table")  
                )
            )
        )

        notes = rep("Keep", nrow(results_table))
        Note = factor(notes, levels=c("Keep", "Drop", "Warn"))
        output$samples_noted = renderPrint({
            cat("Samples noted:\n")
            print(as.data.frame(table(Note)))
            cat("\n")
        })
      
        # column2
        insert_me2 = tags$div(
            id = "annot_form_div",
            tags$p("Annotation Form", 
                class="h5 text-primary fw-bold"),
            tags$p("If necessary make changes to notes below and submit.",
                class="text-secondary"),
                fluidRow(
                    column(2,
                            radioButtons( 
                                inputId = "drop_radio", 
                                label = "Flag", 
                                choices = list( 
                                    "Keep" = "Keep", 
                                    "Drop" = "Drop",
                                    "Warn"="Warn"
                                )
                            )
                        ),
                        column(4,
                            selectInput( 
                                "reason_select", 
                                "Reason for Flag", 
                                list("None" = "None",
                                     "Low Ab Conc" = "Low Ab Conc", 
                                     "Tech Dup" = "Tech Dup",
                                     "Other" = "Other") 
                            )
                        ),
                        column(6,
                            tags$div(
                                id="samples_noted_div"
                            ),
                            tags$div(
                                id="samples_noted_div1",

                                style = "width: 100%;",
                                    verbatimTextOutput("samples_noted", placeholder = TRUE)
                            )
                        )
            ),
            tags$div(
                actionButton("annotate_smpl", 
                             "Submit notes",
                             class="btn-secondary"),
            ),
            tags$br(),
            tags$div(
                tags$p("Save results and exit app", class="h6 text-secondary"),
                actionButton("save_exit", 
                             "Download Results & Exit App",
                             class="btn-warning")
            ),
            tags$br(),
            tags$div(
                id = "main_contents_col2_topmark_div_annot",
            )
        )

        insertUI(
            selector = "#main_contents",
            where = "afterEnd",
            
            ui = tags$div(
                id = "main_contents1",
                
                tags$div(
                    class="row",
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col1_annot",
                        insert_me1   
                    ),
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col2_annot",
                        insert_me2
                    )
                )
            )
        )
      
      
    }) |> bindEvent(input$review_board)
  
    # 7. Sample Profile
    observe({
        if (isTruthy(input$table_rows_selected)) {
            removeUI(selector = "#plot_sample_profile_div")
        }

        results_table = gated_fcs_files$results_table
        gres_list = gated_fcs_files$gres_list
        sel_row_ind = input$table_rows_selected
            
        k = rownames(results_table)[sel_row_ind]  
      
        tsi = paste0("Target Spec ID = ", results_table[k, "Target Spec ID", drop=T])
        pi = paste0("Probe ID = ", results_table[k, "Probe ID", drop=T])
        msg = paste0(tsi, " | ", pi)
      
        output$plot_sample_profile = renderPlot({
            sample_profile_plot(k, gres_list)
        })
            
        insertUI(
            selector = "#main_contents_col2_topmark_div_annot", 
            where = "afterEnd",
            ui = tags$div(
                id = "plot_sample_profile_div",
                card(
                    tags$p(msg),
                    plotOutput("plot_sample_profile", height = "180px"),
                ),
            )
        )
        
    }) |> bindEvent(input$table_rows_selected)
  
    # 8. Add notes
    observe({
        if (isTruthy(input$annotate_smpl)) {
            removeUI(selector = "#samples_noted_div1")
        }

        results_table = gated_fcs_files$results_table
        annot_vector = annot_react_vals$annot_vector
        
        sel_row_ind = input$table_rows_selected    
        k = rownames(results_table)[sel_row_ind]
        
        notes = rep("Keep", nrow(results_table))
        names(notes) = rownames(results_table)
        
        if (input$drop_radio!="Keep") {
            annot_vector[k] = paste0(input$drop_radio, 
                                     "|", 
                                     input$reason_select)
            annot_react_vals$annot_vector = annot_vector
            notes[names(annot_vector)] = annot_vector
        }        
        
        notes = sapply(strsplit(notes, "\\|"), "[[", 1)
        Note = factor(notes, levels=c("Keep", "Drop", "Warn"))
        output$samples_noted = renderPrint({
            cat("Samples noted:\n")
            print(as.data.frame(table(Note)))
            cat("\n")
        })

        insertUI(
            selector = "#samples_noted_div", 
            where = "afterEnd",
            ui = tags$div(
                    id="samples_noted_div1",  
                    style = "width: 100%;",
                        verbatimTextOutput("samples_noted", placeholder = TRUE)
                 )
        )
      
    }) |> bindEvent(input$annotate_smpl)
  
    # 9. Save and exit app
    observe({
        if (isTruthy(input$save_exit)) {
            removeUI(selector = "#annotation_table_div1")
        }
        if (isTruthy(input$save_exit)) {
            removeUI(selector = "#annot_form_div")
        }
      
        results_table = gated_fcs_files$results_table
        
        to_drop = rep("Keep|None", nrow(results_table))
        names(to_drop) = rownames(results_table)        

        annot_vector0 = annot_react_vals$annot_vector
        
        if (length(annot_vector0) > 0) {
            to_drop[names(annot_vector0)] = annot_vector0
        }
        results_table$to_drop = to_drop 

        data_path = input_files$folder_path
        gating_path = paste0(data_path, "/gating_results")
        reset_dir(gating_path)
        res_path = paste0(gating_path, "/", Sys.Date(), "-gated-results.csv")

        write.csv(results_table, file=res_path, row.names = F)

        insertUI(
            selector = "#annotation_table_div",
            where = "afterEnd",
            ui = tags$div(
                id = "exit_div",
                card(tags$p("Thanks for using this app. Check your download folder for the results. Close the browser to exit."))
            )
        )
       
    }) |> bindEvent(input$save_exit)

}
