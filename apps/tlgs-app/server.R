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
            front_page_table3(project_paths, "fcs"),
            error = function(e) {
                error_msg = "'front_page_table2()' an error occurred (K.Okrah)"
                return(error_msg)
            }
        )
        
        output$projects_table = DT::renderDataTable(DT::datatable({
            data = table_front_page
            data = data[data[["Is Gated"]] == "Yes",]
            data[["Is Gated"]] = NULL
            rownames(data) = NULL
            
            if (input$dose_type != "All") {
                data = data[data[["Dose Type"]] == input$dose_type,]
            }
            if (input$proj_group != "All") {
                data = data[data[["Project Group"]] == input$proj_group,]
            }
            data
        },
        options = list(pageLength = 7,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        insert_me1 = tags$div(
            tags$p("Select a Project (or Multiple Projects)",
                   class="h3 text-primary fw-bold text-center"),
            tags$p("Click on row to select project(s) and proceed to 
                   the next page",
                   class="h6 text-secondary text-center"),
            fluidRow(
                selectInput("proj_group",
                            "Project Group",
                            c("All", 
                              sort(unique(table_front_page[["Project Group"]])))),
                selectInput("dose_type",
                            "Dose Type",
                            c("All", "single_dose", "titration",
                              "multi_dose", "other"))
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
    
    # select projects
    observe({
        if (!is.null(input$projects_table_rows_selected)) {
            updateActionButton(session, "load_project", disabled=FALSE)
        }
        
    }) |> bindEvent(input$projects_table_rows_selected)
    observe({
        if (isTruthy(input$load_project)) {
            removeUI(selector = "#main_contents1")
            bslib::toggle_sidebar(id="my_sidebar", open = FALSE)
        }
        
        # filter to match front table    
        table_front_page = input_react_vals$table_front_page
    
        sel = table_front_page[["Is Gated"]] == "Yes"
        if (input$dose_type != "All") {
            sel = sel & table_front_page[["Dose Type"]] == input$dose_type
        }
        if (input$proj_group != "All") {
            sel = sel & table_front_page[["Project Group"]] == input$proj_group
        }
        table_front_page_sub = table_front_page[sel,]

        selected_project = table_front_page_sub[["Project Name"]][input$projects_table_rows_selected]

        tmp = list()
        for (k in selected_project) {
            k_ = project_paths[[k]][["gating_results"]]
            k_ = k_[grep("gated-results.csv$", k_)]
            tmp[[k]] = read.csv(k_, header = T, check.names = F)
        }
        
        dat = do.call(rbind, tmp)
        
        probe_id = sort(unique(dat[["Probe ID"]]))
        target_spec_id = sort(unique(dat[["Target Spec ID"]]))
        
        probe_dict = data.frame(probe_id = probe_id,
                                has_alias = "No",
                                probe_alias = "no_alias")
        
        target_spec_dict = data.frame(target_spec_id = target_spec_id,
                                      has_alias = "No",
                                      target_spec_alias = "no_alias")
        
        input_react_vals$dat = dat
        input_react_vals$probe_dict = probe_dict
        input_react_vals$target_spec_dict = target_spec_dict
        
        output$probe_dict = DT::renderDataTable(DT::datatable({
            data = probe_dict
            if (input$has_alias != "All") {
                data = data[data[["has_alias"]] == input$has_alias,]
            }
            data
        },
        selection = 'none',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        removeUI(selector = "#load_project")
        
        # define column1 insert
        insert_me1 = tags$div(
            id = "ref_probe_id_div",
            tags$div(
                class="row",
                tags$div(
                    id = "ref_probe_id_div2",
                    tags$p("Load Probe ID Dictionary",
                           class="h3 text-primary fw-bold"),
                    tags$p("Load a probe_id dictionary sheet.",
                           class="text-secondary"),
                    
                )
            ),
            tags$div(
                class="row",
                fileInput("file1", "Choose CSV File", accept=".csv")
            ),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    id = "update_probe_ids",
                    actionButton("add_probe_alias",
                                 "Add Probe ID Alias",
                                 class="btn-warning",
                                 width="100%")
                ),
                tags$div(
                    class="col",
                    id = "proceed_to_target_id",
                    actionButton("target_id",
                                 "Proceed to Target IDs",
                                 class="btn-secondary",
                                 width="100%")
                ),
            )
        )
        
        # define column2 insert
        check = probe_dict$has_alias == "Yes"
        msg = paste0(sum(check), " / ", length(check), 
                     " (", round(mean(check)*100, 2), 
                     "%) probe_ids have a probe_alias.")
        
        insert_me2 = tags$div(
            tags$div(
                class="row",
                tags$p("Probe ID Alias Table",
                       class="h3 text-primary fw-bold"),
                tags$p("Check whether plate info. probe_id has an alias. 
                   If has no_alias then probe_id will be used in TLGs.",
                       class="text-secondary"),
                fluidRow(
                    selectInput("has_alias",
                                "Has alias?",
                                c("All", "No", "Yes"))
                ),
                DT::dataTableOutput("probe_dict")
            ),
            tags$div(
                class="row",
                tags$p("Match Rate",
                       class="h6 text-secondary fw-bold"),
                tags$p(msg,
                       class="h6 text-secondary"),
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

    }) |> bindEvent(input$load_project)
    
    # update probe_ids
    observe({
        if (is.null(input$file1)) {
            showNotification(
                ui = paste("Please upload the csv file."),
                type = "error",
                duration = NULL
            )
        }else{
            probe_id_dict = read.csv(input$file1$datapath, 
                                     header = T)
            check = all(c("probe_id", "probe_alias") %in% colnames(probe_id_dict))
            
            # check == TRUE
            if (check) {
                removeUI(selector = "#main_contents1")
                
                # update probe_dict
                probe_dict = input_react_vals$probe_dict
                probe_alias = probe_id_dict[["probe_alias"]]
                names(probe_alias) = probe_id_dict[["probe_id"]]
                probe_alias_ = probe_alias[probe_dict$probe_id]
                is_updated = !is.na(probe_alias_)
                probe_dict$has_alias[is_updated] = "Yes"
                probe_alias_[!is_updated] = "no_alias"
                probe_dict$probe_alias = probe_alias_
                input_react_vals$probe_dict = probe_dict
                
                # update page
                output$probe_dict_u = DT::renderDataTable(DT::datatable({
                    data = probe_dict
                    if (input$has_alias != "All") {
                        data = data[data[["has_alias"]] == input$has_alias,]
                    }
                    data
                },
                selection = 'none',
                options = list(pageLength = 6,
                               dom = "tpf",
                               columnDefs = list(
                                   list(className='dt-nowrap', targets='_all'))
                )))
                
                # define column1 insert
                insert_me1 = tags$div(
                    id = "ref_probe_id_div",
                    tags$div(
                        class="row",
                        tags$div(
                            id = "ref_probe_id_div2",
                            tags$p("Load Probe ID Dictionary",
                                   class="h3 text-primary fw-bold"),
                            tags$p("Load a probe_id dictionary sheet.",
                                   class="text-secondary"),
                            
                        )
                    ),
                    tags$div(
                        class="row",
                        fileInput("file1", "Choose CSV File", accept=".csv")
                    ),
                    
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            id = "update_probe_ids",
                            actionButton("add_probe_alias",
                                         "Add Probe ID Alias",
                                         class="btn-warning",
                                         width="100%")
                        ),
                        tags$div(
                            class="col",
                            id = "proceed_to_target_id",
                            actionButton("target_id",
                                         "Proceed to Target IDs",
                                         class="btn-secondary",
                                         width="100%")
                        ),
                    )
                )
                
                # define column2 insert
                check = probe_dict$has_alias == "Yes"
                msg = paste0(sum(check), " / ", length(check), 
                             " (", round(mean(check)*100, 2), 
                             "%) probe_ids have a probe_alias.")
                
                insert_me2 = tags$div(
                    tags$div(
                        class="row",
                        tags$p("Probe ID Alias Table",
                               class="h3 text-primary fw-bold"),
                        tags$p("Check whether plate info. probe_id has an alias. 
                   If has no_alias then probe_id will be used in TLGs.",
                               class="text-secondary"),
                        fluidRow(
                            selectInput("has_alias",
                                        "Has alias?",
                                        c("All", "No", "Yes"))
                        ),
                        DT::dataTableOutput("probe_dict_u")
                    ),
                    tags$div(
                        class="row",
                        tags$p("Match Rate",
                               class="h6 text-secondary fw-bold"),
                        tags$p(msg,
                               class="h6 text-secondary"),
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
                
            }else{
                showNotification(
                    ui = paste("Uploaded file must have 'probe_id' and 'probe_alias' columns (K.Okrah)."),
                    type = "error",
                    duration = NULL
                )
            }
        }
        
        
    }) |> bindEvent(input$add_probe_alias)
    
    # target id page
    observe({
        if (isTruthy(input$target_id)) {
            removeUI(selector = "#main_contents1")
        }
        
        target_spec_dict = input_react_vals$target_spec_dict
        
        output$target_spec_dict_table = DT::renderDataTable(DT::datatable({
            data = target_spec_dict
            if (input$has_alias != "All") {
                data = data[data[["has_alias"]] == input$has_alias,]
            }
            data
        },
        selection = 'none',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        # define column1 insert
        insert_me1 = tags$div(
            id = "ref_probe_id_div",
            tags$div(
                class="row",
                tags$div(
                    id = "ref_target_id_div2",
                    tags$p("Load Target Spec ID Dictionary",
                           class="h3 text-primary fw-bold"),
                    tags$p("Load a target_spec_id dictionary sheet.",
                           class="text-secondary"),
                    
                )
            ),
            tags$div(
                class="row",
                fileInput("file2", "Choose CSV File", accept=".csv")
            ),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    id = "add_target_alias",
                    actionButton("add_target_alias",
                                 "Add Target Spec ID Alias",
                                 class="btn-warning",
                                 width="100%")
                ),
                tags$div(
                    class="col",
                    id = "proceed_to_tlgs",
                    actionButton("proceed_to_tlgs",
                                 "Proceed to TLGs",
                                 class="btn-secondary",
                                 width="100%")
                ),
            )
        )
        
        # define column2 insert
        check = target_spec_dict$has_alias == "Yes"
        msg = paste0(sum(check), " / ", length(check), 
                     " (", round(mean(check)*100, 2), 
                     "%) target_spec_ids have a target_spec_alias.")
        
        insert_me2 = tags$div(
            tags$div(
                class="row",
                tags$p("Target Spec ID Alias Table",
                       class="h3 text-primary fw-bold"),
                tags$p("Check whether plate info. target_spec_id has an alias. 
                   If has no_alias then target_spec_id will be used in TLGs.",
                       class="text-secondary"),
                fluidRow(
                    selectInput("has_alias",
                                "Has alias?",
                                c("All", "No", "Yes"))
                ),
                DT::dataTableOutput("target_spec_dict_table")
            ),
            tags$div(
                class="row",
                tags$p("Match Rate",
                       class="h6 text-secondary fw-bold"),
                tags$p(msg,
                       class="h6 text-secondary"),
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
        
        
    }) |> bindEvent(input$target_id)

    # update probe_ids
    observe({
        if (is.null(input$file2)) {
            showNotification(
                ui = paste("Please upload the csv file."),
                type = "error",
                duration = NULL
            )
        }else{
            target_id_dict = read.csv(input$file2$datapath,
                                     header = T)
            check = all(c("target_spec_id", "target_spec_alias") %in% colnames(target_id_dict))

            # check == TRUE
            if (check) {
                removeUI(selector = "#main_contents1")

                target_dict = input_react_vals$target_spec_dict
                target_alias = target_id_dict[["target_spec_alias"]]
                names(target_alias) = target_id_dict[["target_spec_id"]]
                target_alias_ = target_alias[target_dict$target_spec_id]
                is_updated = !is.na(target_alias_)
                target_dict$has_alias[is_updated] = "Yes"
                target_alias_[!is_updated] = "no_alias"
                target_dict$target_spec_alias = target_alias_
                input_react_vals$target_spec_dict = target_dict
                
                output$target_spec_dict_table2 = DT::renderDataTable(DT::datatable({
                    data = target_dict
                    if (input$has_alias != "All") {
                        data = data[data[["has_alias"]] == input$has_alias,]
                    }
                    data
                },
                selection = 'none',
                options = list(pageLength = 6,
                               dom = "tpf",
                               columnDefs = list(
                                   list(className = 'dt-nowrap', targets = '_all'))
                )))
                
                insert_me1 = tags$div(
                    id = "ref_probe_id_div",
                    tags$div(
                        class="row",
                        tags$div(
                            id = "ref_target_id_div2",
                            tags$p("Load Target Spec ID Dictionary",
                                   class="h3 text-primary fw-bold"),
                            tags$p("Load a target_spec_id dictionary sheet.",
                                   class="text-secondary"),
                            
                        )
                    ),
                    tags$div(
                        class="row",
                        fileInput("file2", "Choose CSV File", accept=".csv")
                    ),
                    
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            id = "add_target_alias",
                            actionButton("add_target_alias",
                                         "Add Target Spec ID Alias",
                                         class="btn-warning",
                                         width="100%")
                        ),
                        tags$div(
                            class="col",
                            id = "proceed_to_tlgs",
                            actionButton("proceed_to_tlgs",
                                         "Proceed to TLGs",
                                         class="btn-secondary",
                                         width="100%")
                        ),
                    )
                )
                
                check = target_dict$has_alias == "Yes"
                msg = paste0(sum(check), " / ", length(check), 
                             " (", round(mean(check)*100, 2), 
                             "%) target_spec_ids have a target_spec_alias.")
                
                insert_me2 = tags$div(
                    tags$div(
                        class="row",
                        tags$p("Target Spec ID Alias Table",
                               class="h3 text-primary fw-bold"),
                        tags$p("Check whether plate info. target_spec_id has an alias. 
                   If has no_alias then target_spec_id will be used in TLGs.",
                               class="text-secondary"),
                        fluidRow(
                            selectInput("has_alias",
                                        "Has alias?",
                                        c("All", "No", "Yes"))
                        ),
                        DT::dataTableOutput("target_spec_dict_table2")
                    ),
                    tags$div(
                        class="row",
                        tags$p("Match Rate",
                               class="h6 text-secondary fw-bold"),
                        tags$p(msg,
                               class="h6 text-secondary"),
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
            }
    }
        
        
    }) |> bindEvent(input$add_target_alias)
    
    observe({
        if (isTruthy(input$target_id)) {
            removeUI(selector = "#main_contents1")
        }
        
        dat = input_react_vals$dat
        target_spec_dict = input_react_vals$target_spec_dict
        probe_dict = input_react_vals$probe_dict
        
        dat = clean_dat(dat, probe_dict, target_spec_dict)
        print(head(dat))
        
        res_t = prep_titration_data(dat)
        res_s = prep_single_dose_data(dat)
        
        app_dir = paste0(getwd(), "/")
        
        if (length(res_t) > 0) {
            preview_path = paste0(app_dir, 
                                  "www/docs/tlgs/titration-preview.pdf")
            pdf(preview_path, width = 8, height = 11)
            titration_tlgs(res_t)
            dev.off()
            
            img_titration = tags$div(
                style="height: 925px",
                tags$iframe(
                    src="docs/tlgs/titration-preview.pdf",
                    width="90%",
                    height="90%"
                )
            )
        }else{
            img_titration = tags$p("Does not apply")
        }
        
        if (length(res_s) > 0) {
            preview_path = paste0(app_dir, 
                                  "www/docs/tlgs/single-dose-preview.pdf")
            pdf(preview_path, width = 8, height = 11)
            single_dose_tlgs(res_s)
            dev.off()
            
            img_single_dose = tags$div(
                style="height: 925px",
                tags$iframe(
                    src="docs/tlgs/single-dose-preview.pdf",
                    width="90%",
                    height="90%"
                )
            )
        }else{
            img_single_dose = tags$p("Does not apply")
        }
        
        insert_me1 = tags$div(
            id = "ref_probe_id_div",
            tags$div(
                class="row",
                tags$div(
                    id = "ref_probe_id_div2",
                    tags$p("Single Dose TLGs",
                           class="h5 text-primary fw-bold")
                )
            ),
            img_single_dose,
        )
        
        insert_me2 = tags$div(
            class="row",
            id = "ref_probe_id_div",
            tags$div(
                tags$div(
                    id = "ref_probe_id_div2",
                    tags$p("Titration TLGs",
                           class="h5 text-primary fw-bold")
                )
            ),
            img_titration,
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
        
    }) |> bindEvent(input$proceed_to_tlgs)
}






