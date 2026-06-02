# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    #------------------------------ step 0: on start
    # get app directory
    app_dir = paste0(getwd(), "/")
    
    # set path to fixed data source
    dotenv::load_dot_env("~/.env_data_ingestion_apps")
    folder_path = Sys.getenv("DATA_DIR")
    
    # get paths of files in folder_path and organize by project;
    # filtered to only projects with a qc report "qc_report/.*merged-data.csv$"
    project_paths = tryCatch(
        get_paths_by_project2(folder_path),
        error = function(e) {
            error_msg = "'get_paths_by_project()' an error occurred (K.Okrah)"
            return(error_msg)
        }
    )
    
    # make front page table
    input_react_vals = reactiveValues()
    
    observe({
        table_front_page = tryCatch(
            front_page_table2(project_paths, "fcs"),
            error = function(e) {
                error_msg = "'front_page_table2()' an error occurred (K.Okrah)"
                return(error_msg)
            }
        )
        
        output$projects_table = DT::renderDataTable(DT::datatable({
            data = table_front_page
            if (input$is_gated != "All") {
                data = data[data[["Is Gated"]] == input$is_gated,]
            }
            if (input$proj_group != "All") {
                data = data[data[["Project Group"]] == input$proj_group,]
            }
            data
        },
        selection = "single",
        options = list(pageLength = 7,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))

        insert_me1 = tags$div(
            tags$p("Select A Project",
                   class="h3 text-primary fw-bold text-center"),
            tags$p("Click on a row to select project and proceed to 
                   automatically gate",
                   class="h6 text-secondary text-center"),
            fluidRow(
                selectInput("proj_group",
                            "Project Group",
                            c("All", sort(unique(table_front_page[["Project Group"]])))),
                selectInput("is_gated",
                            "Is Gated",
                            c("No", "Yes", "All"))
            ),
            DT::dataTableOutput("projects_table")
        )
        
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
                        tags$div(id = "main_contents_col2_1")
                    )
                )
            )
        )
        
        input_react_vals$table_front_page = table_front_page
    })
    
    
    #------------------------------ step 1: select a project
    observe({
        if (!is.null(input$projects_table_rows_selected)) {
            updateActionButton(session, "load_project", disabled=FALSE)
        }

    }) |> bindEvent(input$projects_table_rows_selected)
    
    observe({
        if (isTruthy(input$load_project)) {
            removeUI(selector = "#main_contents1")
        }
    
        # filter to match front table    
        table_front_page = input_react_vals$table_front_page
    
        sel = rep(TRUE, nrow(table_front_page))
        if (input$is_gated != "All") {
            sel = sel & table_front_page[["Is Gated"]] == input$is_gated
        }
        if (input$proj_group != "All") {
            sel = sel & table_front_page[["Project Group"]] == input$proj_group
        }
        table_front_page_sub = table_front_page[sel,]
        
        # select project
        k = input$projects_table_rows_selected
        selected_project = table_front_page_sub[k, "Project Name"]
        
        # read in fcs files
        path_list = project_paths[[selected_project]]
        paths = c(path_list[["assay_data"]], path_list[["qc_report"]])
        
        res = tryCatch({
            withProgress(message = "Reading FCS files...", value = 0.6, {
                r = read_data(paths)
                setProgress(value = 1, detail = "Done")
                r
            })
        }, error = function(e) {
            error_msg = "'read_data()' an error occurred (K.Okrah)"
            return(error_msg)
        })
        
        # insert selected project ui
        insertUI(
            selector = "#load_project",
            where = "afterEnd",
            ui = tags$div(
                id = "loaded_files_div",
                tags$p("Project Name"),
                tags$p(selected_project,
                       class="text-secondary")
            )
        )
        
        removeUI(selector = "#load_project")
        
        # make summary graphs before gating
        pinfos = res[["pinfos"]]
        fcs_files = res[["fcs_files"]]
        
        tab = summary_events(pinfos$Result, "Total Events")
        output$stat_summary_box = renderPrint({ tab })
        
        tlgs_dir = paste0(app_dir, "www/docs/tlgs/")
        file_path = paste0(tlgs_dir, "plate-events-fig.pdf")
        plot_plate_events(pinfos, is_gated = FALSE, fig_path = file_path)

        output$plot = renderPlot({
            events_boxplot(pinfos$Result)
        })
        
        # define column1 insert
        insert_me1 = tags$div(
            id = "ref_sample_gating_div",
            tags$div(
                id = "ref_sample_gating_div2",
                tags$p("Optimal Viability Gate Detection",
                       class="h5 text-primary fw-bold"),
                tags$p("Computationaly determine optimal gates for all 
                       samples up to viabliliy.",
                       class="text-secondary")
            ),
            tags$div(
                id = "automatic_gate_div",
                actionButton("automatic_gate",
                             "Automatically Gate up to Viability",
                             class="btn-warning")
            )
        )
        
        # define column2 insert
        insert_me2 = tags$div(
            id = "gating_summary_div",
            tags$div(
                class="row",
                tags$p("Total Number of Events (Not Gated)",
                       class="h5 text-secondary fw-bold"),
                tags$div(
                    style="height: 340px",
                    tags$iframe(
                        src="docs/tlgs/plate-events-fig.pdf",
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
                        verbatimTextOutput("stat_summary_box", placeholder=TRUE)
                    )
                )
            )
        )
        
        # implement inserts
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
        
        # save relevant info. 
        input_react_vals$pinfos = pinfos
        input_react_vals$fcs_files = fcs_files
        path2_proj_folder = sapply(strsplit(paths[1], selected_project), 
                                   "[[", 1)
        path2_proj_folder = paste0(path2_proj_folder, selected_project, "/")
        input_react_vals$path2_proj_folder = path2_proj_folder
        
    }) |> bindEvent(input$load_project)
    
    
    #------------------------------ step 4: gate up to viability
    optimal_verts_react_vals = reactiveValues()
    
    observe({
        if (isTruthy(input$automatic_gate)) {
            removeUI(selector = "#ref_sample_gating_div2")
            removeUI(selector = "#automatic_gate_div")
        }

        # compute optimal vertices
        optimal_vertices_res = withProgress(
            message = "Computing optimal gates...", value = 0, {
                # select reference samples
                setProgress(value = 0.1, detail = "Selecting references...")
                pinfos = input_react_vals$pinfos
                fcs_files = input_react_vals$fcs_files
                
                ref = rownames(pinfos)
                refl = split(ref, pinfos$Platename)
                
                nsel = 1
                fref = lapply(refl, function(x) {
                    if (length(x) <= nsel) {
                        return(x)
                    }else{
                        return(x[1:nsel])
                    }
                })
                
                sel_refs = unlist(fref)
                fcs_refs = fcs_files[sel_refs]
                
                setProgress(value = 0.3, detail = "Fitting gates...")
                r = compute_optimal_vertices(fcs_refs, default_ch)
                setProgress(value = 1, detail = "Done")
                r
            })
        
        # gate ref samples
        verts = optimal_vertices_res[["optim_verts_list"]]
        default_ch = optimal_vertices_res[["default_ch"]]
        
        # set mfi channel
        default_ch["ab+", "x_ch"] = pinfos[["mfi_channel"]][1]
        
        # gate reference samples
        gres_list_refs = list()
        n_total = length(fcs_refs)
        
        for (i in seq_along(names(fcs_refs))) {
            k = names(fcs_refs)[i]
            fcs = fcs_refs[[k]]
            mat = flowCore::exprs(fcs)
            result = tryCatch({
                gate2_viability(mat, default_ch, verts)
            }, error = function(e) {
                NA
            })
            gres_list_refs[[k]] = result
        }
        
        # plot ref. gating results
        output$plot_vertices = renderPlot({
            sample_profile_plot(1, gres_list_refs, FALSE)
        })
        
        if (n_total > 1) {
            si = tags$div(
                sliderInput(
                    "reference_samples",
                    "Reference sample:",
                    min = 1, 
                    max = n_total,
                    value = 1,
                    step=1,
                )
            )
            
            msg = tags$p(paste0("Sample A1 is selected from each plate as reference. 
                                 Cycle through the slider below to inspect the 
                                 gates for each plate in the projrct."),
                         class="text-secondary")
        }else{
            si = NULL
            
            msg = tags$p(paste0("Sample A1 is selected from each plate as reference.
                                 See sample below for inspection."),
                         class="text-secondary")
        }
        
        insertUI(
            selector = "#ref_sample_gating_div", 
            where = "afterEnd",
            ui = tags$div(
                id = "auto_viability_gate_results",
                tags$p("Inspect gating for selected samples", 
                       class="h5 text-primary fw-bold"),
                msg,
                si,
                tags$div(
                    id = "ref_clycle_plot_divs_top"
                ),
                tags$div(
                    id = "ref_clycle_plot_divs",
                    tags$p(names(fcs_refs)[1], 
                           class="h6 text-secondary"),
                    tags$div(
                        tags$div(plotOutput("plot_vertices", height = "225px"))
                    ),
                ),
                tags$br(),
                tags$p("Continue", 
                       class="h5 text-primary"),
                tags$p("Proceed to gate all samples or adjust the gates.",
                       class="text-secondary"),
                tags$div(
                    class="row",
                    bslib::layout_column_wrap(
                        actionButton("manually_gate", 
                                     "Manually Adjust the Gates",
                                     class="btn-secondary"),
                        actionButton("gate_all", 
                                     "Apply the Gates to All Samples",
                                     class="btn-primary")
                    )
                )
            )
        )
        
        # save results
        optimal_verts_react_vals$optimal_vertices_res = optimal_vertices_res
        optimal_verts_react_vals$gres_list_refs = gres_list_refs
        optimal_verts_react_vals$n_total = n_total
        optimal_verts_react_vals$fcs_refs = fcs_refs
        
    }) |> bindEvent(input$automatic_gate)
    
    # cycle through gated reference samples 
    observe({
        if (isTruthy(input$reference_samples)) {
            removeUI(selector = "#ref_clycle_plot_divs")
        }
        
        k = input$reference_samples
        
        gres_list_refs = optimal_verts_react_vals$gres_list_refs
        fcs_refs = optimal_verts_react_vals$fcs_refs
        
        output$plot_vertices = renderPlot({
            sample_profile_plot(k, gres_list_refs, FALSE)
        })
        
        insertUI(
            selector = "#ref_clycle_plot_divs_top", 
            where = "afterEnd",
            ui = tags$div(
                id = "ref_clycle_plot_divs",
                tags$p(names(fcs_refs)[k], 
                       class="h6 text-secondary"),
                tags$div(
                    tags$div(plotOutput("plot_vertices", height = "225px"))
                )
            )
        )
        
    }) |> bindEvent(input$reference_samples)
    
    # manually gate
    observe({
        if (isTruthy(input$manually_gate)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#main_contents2")
        }
        
        optimal_vertices_res = optimal_verts_react_vals$optimal_vertices_res
        n_total = optimal_verts_react_vals$n_total
        fcs_refs = optimal_verts_react_vals$fcs_refs
        
        verts = optimal_vertices_res[["optim_verts_list"]]
        
        # intact
        intact_ab_h_0 = c(verts$intact["A","x"], verts$intact["B","x"])
        intact_ab_v_0 = c(verts$intact["A","y"], verts$intact["D","y"])
        
        # singlet
        singlet_ab_h_0 = c(verts$singlet["A","x"], verts$singlet["B","x"])
        singlet_ab_v_0 = c(verts$singlet["A","y"], verts$singlet["B","y"])
        singlet_shift_0 = verts$singlet["D","y"] - verts$singlet["A","y"]
        
        # viable
        viable_b_h_0 = verts$viable["B","x"]
        
        if (n_total > 1) {
            si = tags$div(
                sliderInput(
                    "reference_samples",
                    "Reference sample:",
                    min = 1, 
                    max = n_total,
                    value = 1,
                    step=1,
                )
            )
            
            msg = tags$p(paste0("Sample A1 is selected from each plate as reference. 
                                 Cycle through the slider below to inspect the 
                                 gates for each plate in the projrct."),
                         class="text-secondary")
            
        }else{
            si = NULL
            
            msg = tags$p(paste0("Sample A1 is selected from each plate as reference.  
                                See sample below to for inspection."),
                         class="text-secondary")
        }
        
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
                        tags$div(
                            id = "main_contents_col2_topmark_div",
                        ),
                        
                        tags$p("Adjust Gating",
                               class="h5 text-primary fw-bold"),
                        
                        tags$p("Adjust the polygon vertices below.",
                               class="text-secondary"),
                        
                        # intact
                        tags$p("Intact",
                               class="h5 text-primary"),
                        
                        tags$div(
                            class="row",
                            bslib::layout_column_wrap(
                                sliderInput(
                                    "intact_width",
                                    "Adjust box width",
                                    min = 0, 
                                    max = 1e+06,
                                    value = c(intact_ab_h_0[1], 
                                              intact_ab_h_0[2])
                                ),
                                sliderInput(
                                    "intact_height",
                                    "Adjust box height",
                                    min = 0, 
                                    max = 1e+06,
                                    value = c(intact_ab_v_0[1], 
                                              intact_ab_v_0[2])
                                ),
                            ) 
                        ),
                        
                        # singlet
                        tags$p("Singlet",
                               class="h5 text-primary"),
                        
                        tags$div(
                            class="row",
                            bslib::layout_column_wrap(
                                sliderInput(
                                    "singlet_ab_x",
                                    "Adjust bottom left & right (x-axis)",
                                    min = 1e+04, 
                                    max = 1e+06,
                                    value = c(singlet_ab_h_0[1], 
                                              singlet_ab_h_0[2])
                                ),
                                sliderInput(
                                    "singlet_ab_y",
                                    "Adjust bottom left & right (y-axis)",
                                    min = 1000,
                                    max = 500000,
                                    value = c(singlet_ab_v_0[1], 
                                              singlet_ab_v_0[2])
                                ),
                                sliderInput(
                                    "singlet_shift",
                                    "Adjust height",
                                    min = 1000, max = 250000,
                                    value = singlet_shift_0
                                ),
                            ) 
                        ),
                        
                        # viable
                        tags$p("Viable",
                               class="h5 text-primary"),
                        
                        tags$div(
                            class="row",
                            sliderInput(
                                "viable_line",
                                "Adjust line",
                                min = 0, max = 5,
                                value = viable_b_h_0,
                                step=0.01,
                            ),
                        ),
                        
                    ),
                    
                    tags$div(
                        class="col",
                        id = "main_contents_col2",
                        tags$div(
                            id = "main_contents_col2_topmark_div",
                        ),
                        
                        tags$p("Gate All Samples",
                               class="h5 text-primary fw-bold"),
                        
                        tags$p("Inspect gating vertices below.",
                               class="text-secondary"),
                        msg,
                        si,
                        
                        tags$div(
                            id = "ref_clycle_plot_divs_top"
                        ),
                        
                        tags$div(
                            id = "ref_clycle_plot_divs",
                            tags$p(names(fcs_refs)[1], 
                                   class="h6 text-secondary"),
                            tags$div(
                                tags$div(plotOutput("plot_vertices", 
                                                    height="225px"))
                            ),
                        ),
                        
                        tags$br(),
                        
                        tags$div(
                            class="row",
                            bslib::layout_column_wrap(
                                actionButton("refresh_gates", 
                                             "Refresh gates",
                                             class="btn-warning"),
                                actionButton("gate_all", 
                                             "Apply the Gates to All Samples",
                                             class="btn-primary")
                            )
                        ),
                    )
                )
            )
        )
        
    }) |> bindEvent(input$manually_gate)
    
    # refresh gates
    observe({
        if (isTruthy(input$refresh_gates)) {
            removeUI(selector="#ref_clycle_plot_divs")
        }
        
        pinfos = input_react_vals$pinfos
        optimal_vertices_res = optimal_verts_react_vals$optimal_vertices_res
        fcs_refs = optimal_verts_react_vals$fcs_refs
        
        # gate ref samples
        verts = optimal_vertices_res[["optim_verts_list"]]
        default_ch = optimal_vertices_res[["default_ch"]]
        
        # update mfi_channel
        default_ch["ab+", "x_ch"] = pinfos[["mfi_channel"]][1]
        
        # update verts: start
        # intact
        intact_width = input$intact_width
        intact_height = input$intact_height
        
        verts$intact["A", "x"] = intact_width[1]
        verts$intact["B", "x"] = intact_width[2]
        verts$intact["C", "x"] = intact_width[2]
        verts$intact["D", "x"] = intact_width[1]
        
        verts$intact["A", "y"] = intact_height[1]
        verts$intact["B", "y"] = intact_height[1]
        verts$intact["C", "y"] = intact_height[2]
        verts$intact["D", "y"] = intact_height[2]
        
        # singlet
        singlet_ab_x = input$singlet_ab_x
        singlet_ab_y = input$singlet_ab_y
        singlet_shift = input$singlet_shift
        
        verts$singlet["A", "x"] = singlet_ab_x[1]
        verts$singlet["B", "x"] = singlet_ab_x[2]
        verts$singlet["A", "y"] = singlet_ab_y[1]
        verts$singlet["B", "y"] = singlet_ab_y[2]
        
        verts$singlet["C", "x"] = singlet_ab_x[2]
        verts$singlet["D", "x"] = singlet_ab_x[1]
        verts$singlet["C", "y"] = singlet_ab_y[2] + singlet_shift
        verts$singlet["D", "y"] = singlet_ab_y[1] + singlet_shift
        
        # viable
        viable_line = input$viable_line
        verts$viable["B", "x"] = viable_line[1]
        
        #--------------------- update verts: end
        gres_list_refs = list()
        n_total = length(fcs_refs)
        
        for (i in seq_along(names(fcs_refs))) {
            k = names(fcs_refs)[i]
            fcs = fcs_refs[[k]]
            mat = flowCore::exprs(fcs)
            result = tryCatch({
                gate2_viability(mat, default_ch, verts)
            }, error = function(e) {
                NA
            })
            gres_list_refs[[k]] = result
        }
        
        if (n_total==1) {
            k = 1    
        }else{
            k = input$reference_samples
        }
        
        output$plot_vertices = renderPlot({
            sample_profile_plot(k, gres_list_refs, FALSE)
        })
        
        insertUI(
            selector = "#ref_clycle_plot_divs_top", 
            where = "afterEnd",
            
            ui = tags$div(
                id = "ref_clycle_plot_divs",
                tags$p(names(fcs_refs)[k], 
                       class="h6 text-secondary"),
                tags$div(
                    tags$div(plotOutput("plot_vertices", height = "225px"))
                ),
            )
        )
        
        # update verts in optimal_vertices_res
        optimal_vertices_res[["optim_verts_list"]] = verts
        optimal_verts_react_vals$optimal_vertices_res = optimal_vertices_res
        optimal_verts_react_vals$gres_list_refs = gres_list_refs
        
    }) |> bindEvent(input$refresh_gates)
    
    
    #------------------------------ step 5: gate all samples
    gated_fcs_files = reactiveValues()
    
    observe({
        if (isTruthy(input$automatic_gate)) {
            removeUI(selector = "#main_contents1")
        }
        
        pinfos = input_react_vals$pinfos
        fcs_files = input_react_vals$fcs_files
        
        optimal_vertices_res = optimal_verts_react_vals$optimal_vertices_res
        verts = optimal_vertices_res[["optim_verts_list"]]
        default_ch = optimal_vertices_res[["default_ch"]]
        
        # set the MFI channel
        default_ch["ab+", "x_ch"] = pinfos[["mfi_channel"]][1]
        
        # gate all samples
        gres_list = list()
        n_total = length(fcs_files)

        withProgress(message = "Gating samples...", value = 0, {
            for (i in seq_along(names(fcs_files))) {
                k = names(fcs_files)[i]
                fcs = fcs_files[[k]]
                mat = flowCore::exprs(fcs)
                result = tryCatch({
                    gate2_viability(mat, default_ch, verts)
                }, error = function(e) {
                    NA
                })
                gres_list[[k]] = result
                setProgress(value = i / n_total,
                            detail = paste0(i, " / ", n_total))
            }
        })
        
        author_gating = paste0(Sys.info()[["user"]], 
                               " | ",
                               Sys.info()[["nodename"]])
        
        results_table = make_results_table(gres_list, pinfos,
                                           author_gating = author_gating)

        tab = summary_viable_events(results_table)
        output$stat_summary_viable_box = renderPrint({ tab })

        # plot total viable events
        tlgs_dir = paste0(app_dir, "www/docs/tlgs/")
        file_path1 = paste0(tlgs_dir, "plate-viable-events-fig.pdf")
        plot_plate_events(results_table, is_gated = TRUE, fig_path=file_path1)

        # plot mfi
        file_path2 = paste0(tlgs_dir, "plate-viable-mfi-fig.pdf")
        plot_mfi(results_table, fig_path=file_path2)

        # events boxplot
        output$plot_viable = renderPlot({
            events_boxplot(results_table$Result, 
                           results_table[, "/intact/singlet/viable"])
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
                        src="docs/tlgs/plate-viable-mfi-fig.pdf",
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
                    tags$p("Review results sample by sample and make 
                            annotations for downstream analysis.",
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
                        src="docs/tlgs/plate-viable-events-fig.pdf",
                        width="100%",
                        height="100%"
                    )
                ),

                tags$p("Each page corresponds to a single plate 
                       (black dots = total events; blue dots = viable cells).",
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
                        verbatimTextOutput("stat_summary_viable_box", 
                                           placeholder = TRUE)
                    )
                )
            )
        )

        # implement inserts
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

        # save results
        gated_fcs_files$results_table = results_table
        gated_fcs_files$gres_list = gres_list
        
    }) |> bindEvent(input$gate_all)
    
    
    #------------------------------ step 6: Annotation page
    annot_react_vals = reactiveValues()

    observe({
        if (isTruthy(input$review_board)) {
            removeUI(selector = "#main_contents1")
        }

        results_table_all = gated_fcs_files$results_table
        gres_list = gated_fcs_files$gres_list

        scol = c("Platename", "Plate Position", "Primary Role",
                 "/intact/singlet/viable", "gMFI", "Log10 MFI")

        results_table = results_table_all[,scol]
        results_table$"Log10 MFI" = sprintf("%0.3f", results_table$"Log10 MFI")
        results_table$"gMFI" = sprintf("%0.1f", results_table$"gMFI")

        plate_cols = gsub("[A-Z]", "", results_table[,"Plate Position"])
        plate_rows = gsub("[0-9]|[0-9]", "", results_table[,"Plate Position"])

        colnames(results_table)[colnames(results_table) == "/intact/singlet/viable"] = "Viable"
        colnames(results_table)[colnames(results_table) == "Plate Position"] = "Well"
        colnames(results_table)[colnames(results_table) == "Primary Role"] = "Role"

        uplatnam = unique(as.character(results_table$Platename))
        results_table = cbind(plate_rows = plate_rows,
                              plate_cols = plate_cols,
                              results_table)

        # initialize annotation container
        annot_react_vals$annot_vector = c()
        annot_react_vals$display_table = results_table

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

                    DT::DTOutput("table")
                )
            )
        )

        notes = rep("Keep", nrow(results_table))
        Note = factor(notes, levels=c("Keep", "Drop", "Warn"))
        output$samples_noted_ui = renderUI({
            tbl = as.data.frame(table(Note))
            tags$pre(paste0("Samples noted:\n",
                            paste(capture.output(print(tbl, row.names = FALSE)), 
                                  collapse = "\n")))
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
                       uiOutput("samples_noted_ui")
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
                             class="btn-warning"),
                tags$div(id = "save_confirm_div")
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

    # shared filtered table reactive
    filtered_table = reactive({
        data = annot_react_vals$display_table
        req(data)

        # add annotation status column
        av = annot_react_vals$annot_vector
        note = rep("Keep", nrow(data))
        names(note) = rownames(data)
        if (length(av) > 0) {
            shared = intersect(names(av), names(note))
            note[shared] = sapply(strsplit(av[shared], "\\|"), "[[", 1)
        }
        data$`.note` = note

        if (isTruthy(input$platename) && input$platename != "All")
            data = data[data$Platename == input$platename,,drop=F]
        if (isTruthy(input$plate_rows) && input$plate_rows != "All")
            data = data[data$plate_rows == input$plate_rows,,drop=F]
        if (isTruthy(input$plate_cols) && input$plate_cols != "All")
            data = data[data$plate_cols == input$plate_cols,,drop=F]
        data
    })

    output$table = DT::renderDT({
        dt = filtered_table()
        note_col = which(colnames(dt) == ".note") - 1  # 0-indexed
        DT::datatable(dt,
                      selection = "single",
                      rownames = FALSE,
                      options = list(pageLength = 10,
                                     dom = "tp",
                                     columnDefs = list(
                                         list(visible = FALSE, targets = c(0, 1, note_col)),
                                         list(className = 'dt-nowrap', targets = '_all')),
                                     rowCallback = DT::JS(sprintf(
                                         "function(row, data) {
                                   var note = data[%d];
                                   if (note === 'Drop') {
                                       $(row).find('td').css('background-color', '#f8d0d0');
                                   } else if (note === 'Warn') {
                                       $(row).find('td').css('background-color', '#fff3cd');
                                   }
                               }", note_col))
                      ))
    }, server = FALSE)
    
    # sample profile
    observe({
        if (isTruthy(input$table_rows_selected)) {
            removeUI(selector = "#plot_sample_profile_div")
        }

        gres_list = gated_fcs_files$gres_list
        sel_row_ind = input$table_rows_selected

        k = rownames(filtered_table())[sel_row_ind]
        full_table = gated_fcs_files$results_table

        tsi = paste0("Target Spec ID = ", full_table[k, "Target Spec ID", drop=T])
        pi = paste0("Probe ID = ", full_table[k, "Probe ID", drop=T])
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
        full_table = gated_fcs_files$results_table
        annot_vector = annot_react_vals$annot_vector

        sel_row_ind = input$table_rows_selected
        k = rownames(filtered_table())[sel_row_ind]

        notes = rep("Keep", nrow(full_table))
        names(notes) = rownames(full_table)

        if (input$drop_radio != "Keep") {
            annot_vector[k] = paste0(input$drop_radio, "|", input$reason_select)
        } else {
            annot_vector = annot_vector[names(annot_vector) != k]
        }
        annot_react_vals$annot_vector = annot_vector
        notes[names(annot_vector)] = annot_vector

        notes = sapply(strsplit(notes, "\\|"), "[[", 1)
        Note = factor(notes, levels=c("Keep", "Drop", "Warn"))
        output$samples_noted_ui = renderUI({
            tbl = as.data.frame(table(Note))
            tags$pre(paste0("Samples noted:\n",
                            paste(capture.output(print(tbl, row.names = FALSE)), 
                                  collapse = "\n")))
        })

    }) |> bindEvent(input$annotate_smpl)
    
    
    #-------------------------------- save and exit
    observe({
        
        if (isTruthy(input$save_exit)) {
            removeUI(selector = "#main_contents1")
        }
        
        results_table = gated_fcs_files$results_table
        
        to_drop = rep("Keep|None", nrow(results_table))
        names(to_drop) = rownames(results_table)
        
        annot_vector0 = annot_react_vals$annot_vector
        
        if (length(annot_vector0) > 0) {
            to_drop[names(annot_vector0)] = annot_vector0
        }
        results_table$to_drop = to_drop

        data_path = input_react_vals$path2_proj_folder
        gating_results_fldr = paste0(data_path, "gating_results")
        reset_dir(gating_results_fldr)
    
        print(gating_results_fldr)
        
        gate_file_name = "gated-results.csv"
        gate_file_path = paste0(gating_results_fldr, "/", gate_file_name)
        write.csv(results_table, file = gate_file_path, row.names = FALSE)
        
        # save optim_verts_list
        optimal_vertices_res = optimal_verts_react_vals$optimal_vertices_res
        verts = optimal_vertices_res[["optim_verts_list"]]
        verts_ = list()
        for (j in names(verts)) {
            verts_[[j]] = cbind(gate=rep(j, nrow(verts[[j]])), verts[[j]])
        }
        verts_mat = do.call(rbind, verts_)
        
        verts_file_name = "polygon-vertices.csv"
        verts_file_path = paste0(gating_results_fldr, "/", verts_file_name)
        write.csv(verts_mat, file = verts_file_path, row.names = F)
        
        # exit page
        insert_me1 = tags$div(
            tags$p("Done",
                   class="h5 text-secondary fw-bold"),
            tags$p("Gated results are in the project folder.
                    Go back to the home page or proceed to gate another project.",
                   class="text-seconday"),
            actionButton(
                "reload_app",
                "Gate another project",
                class="btn-primary"
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
                    )
                )
            )
        )
        
        # clear tlgs folder in app
        TLGs_FLS = list.files(paste0(app_dir, "www/docs/tlgs"), full.names=T)
        if (length(TLGs_FLS) > 0) {
            for (fl in TLGs_FLS) {
                file.remove(fl)    
            }
        }
        
        # copy to gdrive
        GDRIVE_DIR = Sys.getenv("GDRIVE_DIR")
        GDRIVE_DIR_SAVE = paste0(GDRIVE_DIR, 
                                 gsub(folder_path, "", gating_results_fldr))
        GDRIVE_DIR_SAVE = gsub("gating_results", "", GDRIVE_DIR_SAVE)
        
        if (!dir.exists(GDRIVE_DIR_SAVE)) {
            dir.create(GDRIVE_DIR_SAVE, recursive = TRUE)
        }
        
        sys_cmp = paste0("cp -r ", gating_results_fldr, " ", GDRIVE_DIR_SAVE)
        system(sys_cmp)
        
    }) |> bindEvent(input$save_exit)
    
    # reload
    observe({
        session$reload()
    }) |> bindEvent(input$reload_app)
}
