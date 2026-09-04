# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    # 0. set app directory
    app_dir = paste0(getwd(), "/")

    # set path to fixed data source
    dotenv::load_dot_env("~/.env_data_ingestion_apps")
    
    # 0. get paths to results csvs
    folder_path = Sys.getenv("DATA_DIR")
    
    res_list = tryCatch(
        results_summary_list(folder_path),
        error = function(e) {
            error_msg = "'results_summary_list()' an error occurred (K.Okrah)"
            return(error_msg)
        }
    )
    
    # 1. landing page 
    input_react_vals = reactiveValues()
    observe({
        ss = sapply(strsplit(as.character(Sys.time()), "\\."), "[[", 1)
        ss = gsub(" ", "_", ss)
        ss = gsub(":", "", ss)
        input_react_vals$session_name = ss
        
        table_front_page = tryCatch(
            res_list[["front_page_table"]],
            error = function(e) {
                error_msg = "'results_summary_list()' an error occurred (K.Okrah)"
                return(error_msg)
            }
        )
        
        input_react_vals$table_front_page = table_front_page
        
        N = sum(as.numeric(table_front_page$results_n), na.rm = T)
        N_Cells = length(unique(tolower(unlist(res_list[["unique_cell_lines"]]))))
        N_Probes = length(unique(tolower(unlist(res_list[["unique_probes"]]))))
        N_Projects = nrow(table_front_page)
        
        input_react_vals$N = N
        input_react_vals$N_Cells = N_Cells
        input_react_vals$N_Probes = N_Probes
        input_react_vals$N_Projects = N_Projects
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                card(
                    card_image("www/img/data_loaded1.jpg"),
                    card_body(
                        textInput( 
                            "session_name", 
                            "Session name (optional)", 
                            placeholder = "Enter text..."
                        ),
                        actionButton("submit_session_name",
                                     "Submit Session Name",
                                     class = "btn-primary w-100",
                                     disabled = FALSE)
                    )
                ),
                tags$div(
                    id="card_bottom_div"
                )
            )
        )
        
        insert_me1 = tags$div(
            class = "col",
            id    = "main_contents_col1",
            tags$p("WELCOME",
                   class = "h4 text-primary fw-bold"),
            tags$p("Welcome to the flow cytometry results repository.",
                   class = "text-secondary"),
            tags$p(paste0("Total Sample Size: ", N),
                   class="h4 text-secondary"),
            tags$br(),
            tags$p("Experiment types",
                   class = "h5 text-secondary"),
            tags$ul(
                tags$li("Binding"),
                tags$li("Internalization")
            ),
            tags$br(),
            tags$p(paste0("(", N_Cells, " cell lines & ", N_Probes, " probes)"),
                   class="h5 text-secondary"),
            tags$div(
                tags$p(""),
                tags$br(),
                tags$p("Session name",
                       class = "h4 text-primary fw-bold"),
                tags$p("The default session name is date & time, optionally specify one.",
                       class = "text-secondary"),
                tags$div(
                    id = "entered_session_div_top"
                ),
                tags$div(
                    id = "project_name_div",
                    tags$p(ss,
                           class = "h5 text-secondary",
                           style = "font-family: monospace;")
                )
            )
        )
        
        insert_me2 = tags$div(
            class = "col",
            id    = "main_contents_col2",
            tags$div(id = "main_contents_col2_1"),
            # tags$p("Load all samples",
            #        class = "h4 text-primary fw-bold"),
            # tags$p("Load database and proceed to select cell lines and probes of interest.",
            #        class = "text-secondary"),
            # 
            # actionButton("load_all_data",
            #              "Load All Samples",
            #              class = "btn-warning w-50",
            #              disabled = FALSE),
            # 
            # tags$p(""),
            # tags$br(),
            # tags$p("Or",
            #        class = "h6 text-secondary"),
            # tags$br(),
            
            tags$p("Select specific projects",
                   class = "h4 text-secondary"),
            
            tags$p("Go to the project selection page.",
                   class = "text-secondary"),
            
            actionButton("select_projects",
                         "Go to Select Projects",
                         class = "btn-warning w-50",
                         disabled = FALSE),
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
                        insert_me2
                    )
                )
            )
        )
        
    })
    observe({
        if (isTruthy(input$submit_session_name)) {
            removeUI(selector = "#project_name_div")
        }
        
        session_name = strsplit(input$session_name, split="")[[1]]
        
        if (length(session_name)==0) {
            showNotification("Enter a session name.", 
                             type = "message", 
                             duration = 2)
        }else{
            if (" " %in% session_name) {
                showNotification("Spaces are not allowed in session names.", 
                                 type = "message", 
                                 duration = 2)
            }else{
                if (!all(session_name %in% c(letters, LETTERS, 0:9, "-", "_"))){
                    showNotification("Only alphabets, numberic digits, '-', and, '_' are allowed.", 
                                     type = "message", 
                                     duration = 2)
                }else{
                    if (length(session_name) > 15) {
                        showNotification("The session name must not exceed 15 characters.", 
                                         type = "message", 
                                         duration = 2)
                    }else{
                        session_name = paste0(session_name, collapse = "")
                        input_react_vals$session_name = session_name
                        
                        insertUI(
                            "#entered_session_div_top",
                            "afterEnd",
                            ui = tags$div(
                                id = "project_name_div",
                                tags$p(session_name,
                                       class = "h5 text-secondary",
                                       style = "font-family: monospace;")
                            )
                        )
                    }
                }
            }
        }
        
    }) |> bindEvent(input$submit_session_name)
    observe({
        if (isTruthy(input$go_back_to_front_page)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
        }
        
        ss = input_react_vals$session_name
        table_front_page = input_react_vals$table_front_page
        N = input_react_vals$N
        N_Cells = input_react_vals$N_Cells
        N_Probes = input_react_vals$N_Probes
        N_Projects = input_react_vals$N_Projects
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                card(
                    card_image("www/img/data_loaded1.jpg"),
                    card_body(
                        textInput( 
                            "session_name", 
                            "Session name (optional)", 
                            placeholder = "Enter text..."
                        ),
                        actionButton("submit_session_name",
                                     "Submit Session Name",
                                     class = "btn-primary w-100")
                    )
                ),
                tags$div(
                    id="card_bottom_div"
                )
            )
        )
        
        insert_me1 = tags$div(
            class = "col",
            id    = "main_contents_col1",
            tags$p("WELCOME",
                   class = "h4 text-primary fw-bold"),
            tags$p("Welcome to the flow cytometry results repository.",
                   class = "text-secondary"),
            tags$p(paste0("Total Sample Size: ", N),
                   class="h4 text-secondary"),
            tags$br(),
            tags$p("Experiment types",
                   class = "h5 text-secondary"),
            tags$ul(
                tags$li("Binding"),
                tags$li("Internalization")
            ),
            tags$br(),
            tags$p(paste0("(", N_Cells, " cell lines & ", N_Probes, " probes)"),
                   class="h5 text-secondary"),
            tags$div(
                tags$p(""),
                tags$br(),
                tags$p("Session name",
                       class = "h4 text-primary fw-bold"),
                tags$p("The default session name is date & time, optionally specify one.",
                       class = "text-secondary"),
                tags$div(
                    id = "entered_session_div_top"
                ),
                tags$div(
                    id = "project_name_div",
                    tags$p(ss,
                           class = "h5 text-secondary",
                           style = "font-family: monospace;")
                )
            )
        )
        
        insert_me2 = tags$div(
            class = "col",
            id    = "main_contents_col2",
            tags$div(id = "main_contents_col2_1"),
            # tags$p("Load all samples",
            #        class = "h4 text-primary fw-bold"),
            # tags$p("Load database and proceed to select cell lines and probes of interest.",
            #        class = "text-secondary"),
            # 
            # actionButton("load_all_data",
            #              "Load All Samples",
            #              class = "btn-warning w-50",
            #              disabled = FALSE),
            # 
            # tags$p(""),
            # tags$br(),
            # tags$p("Or",
            #        class = "h6 text-secondary"),
            # tags$br(),
            
            tags$p("Select specific projects",
                   class = "h4 text-secondary"),
            
            tags$p("Go to the project selection page.",
                   class = "text-secondary"),
            
            actionButton("select_projects",
                         "Go to Select Projects",
                         class = "btn-warning w-50",
                         disabled = FALSE),
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
                        insert_me2
                    )
                )
            )
        )
        
    }) |> bindEvent(input$go_back_to_front_page)
    
    # # 2a. load all data and proceed to probe housekeeping
    # observe({
    #     if (isTruthy(input$load_all_data)) {
    #         removeUI(selector = "#main_contents1")
    #         removeUI(selector = "#load_project")
    #         removeUI(selector = "#card_div")
    #     }
    #     
    #     insertUI(
    #         selector="#card_div_top",
    #         where = "afterEnd",
    #         ui = tags$div(
    #             id = "card_div",
    #             tags$p("Session name",
    #                    class="h6 text-primary fw-bold"),
    #             tags$p(input_react_vals$session_name,
    #                    class="h6 text-secondary"),
    #             tags$br(),
    #             actionButton(
    #                 "go_back_to_front_page",
    #                 "Go Back To Front Page",
    #                 class="wt-100"
    #             )
    #         )
    #     )
    #     
    #     table_front_page = input_react_vals$table_front_page
    #     sel_projs = 1:nrow(table_front_page)
    #     selected_project_paths = table_front_page[["full_path"]][sel_projs]
    #     selected_proj_group = table_front_page[["proj_group"]][sel_projs]
    #     selected_proj_name = table_front_page[["project_name"]][sel_projs]
    #     selected_proj_ch = table_front_page[["mfi_channel"]][sel_projs]
    #     
    #     tmp = list()
    #     for (k in 1:length(selected_project_paths)) {
    #         tmp_dat = read.csv(selected_project_paths[k],
    #                            header = TRUE,
    #                            check.names = FALSE)
    #         tmp_dat$project_group = selected_proj_group[k]
    #         tmp_dat$project_name = selected_proj_name[k]
    #         tmp_dat$mfi_channel = selected_proj_ch[k]
    #         tmp[[k]] = tmp_dat
    #     }
    #     dat = do.call(rbind, tmp)
    #     to_keep = grep("^Keep", dat[["to_drop"]])
    #     dat = dat[to_keep,,drop=F]
    #     rownames(dat) = NULL
    #     
    #     probe_id = sort(unique(dat[["Probe ID"]]))
    #     target_spec_id = sort(unique(dat[["Target Spec ID"]]))
    #     
    #     probe_dict = data.frame(probe_id = probe_id,
    #                             has_alias = "No",
    #                             probe_alias = "no_alias")
    #     
    #     target_spec_dict = data.frame(target_spec_id = target_spec_id,
    #                                   has_alias = "No",
    #                                   target_spec_alias = "no_alias")
    #     
    #     probe_id_dict = read.csv(paste0(app_dir,
    #                                     "www/docs/probe_dict/20260712-probe-info.csv"),
    #                              header = T, check.names = F)
    #     
    #     probe_alias = probe_id_dict[["probe_alias"]]
    #     names(probe_alias) = probe_id_dict[["probe_id"]]
    #     
    #     probe_alias_ = probe_alias[probe_dict$probe_id]
    #     is_updated = !is.na(probe_alias_)
    #     probe_dict$has_alias[is_updated] = "Yes"
    #     probe_alias_[!is_updated] = "no_alias"
    #     probe_dict$probe_alias = probe_alias_
    #     
    #     target_id_dict = read.csv(paste0(app_dir,
    #                                      "www/docs/target_dict/20260712-target-dict.csv"),
    #                               header = T, check.names = F)
    #     
    #     target_alias = target_id_dict[["target_spec_alias"]]
    #     names(target_alias) = target_id_dict[["target_spec_id"]]
    #     
    #     target_alias_ = target_alias[target_spec_dict$target_spec_id]
    #     is_updated = !is.na(target_alias_)
    #     target_spec_dict$has_alias[is_updated] = "Yes"
    #     target_alias_[!is_updated] = "no_alias"
    #     target_spec_dict$target_spec_alias = target_alias_
    #     
    #     input_react_vals$dat = dat
    #     input_react_vals$probe_dict = probe_dict
    #     input_react_vals$target_spec_dict = target_spec_dict
    #     
    #     output$probe_dict = DT::renderDataTable(DT::datatable({
    #         data = probe_dict
    #         if (input$has_alias != "All") {
    #             data = data[data[["has_alias"]] == input$has_alias,]
    #         }
    #         data
    #     },
    #     selection = 'none',
    #     options = list(pageLength = 5,
    #                    dom = "tpf",
    #                    columnDefs = list(
    #                        list(className = 'dt-nowrap', targets = '_all'))
    #     )))
    #     
    #     insert_me1 = tags$div(
    #         id = "ref_probe_id_div",
    #         tags$div(
    #             class="row",
    #             tags$div(
    #                 id = "ref_probe_id_div2",
    #                 tags$p("Probe names (housekeeping)",
    #                        class="h3 text-primary fw-bold"),
    #                 tags$p("Load a probe_id dictionary sheet.",
    #                        class="text-secondary")
    #             )
    #         ),
    #         tags$div(
    #             class="row",
    #             fileInput("file1", "Choose CSV File", accept=".csv")
    #         ),
    #         tags$div(
    #             class="row",
    #             tags$div(
    #                 class="col",
    #                 id = "update_probe_ids",
    #                 actionButton("add_probe_alias",
    #                              "Update Probe ID Alias",
    #                              class="btn-warning",
    #                              width="100%",
    #                              disabled=FALSE)
    #             ),
    #             tags$div(
    #                 class="col",
    #                 id = "proceed_to_target_id",
    #                 actionButton("target_id",
    #                              "Proceed to Target IDs",
    #                              class="btn-secondary",
    #                              width="100%")
    #             )
    #         )
    #     )
    #     
    #     check = probe_dict$has_alias == "Yes"
    #     msg = paste0(sum(check), " / ", length(check),
    #                  " (", round(mean(check)*100, 2),
    #                  "%) probe_ids have a probe_alias.")
    #     
    #     insert_me2 = tags$div(
    #         tags$div(
    #             class="row",
    #             tags$p("Probe alias table",
    #                    class="h3 text-primary fw-bold"),
    #             tags$p("Check whether plate info. probe_id has an alias.
    #                If has no_alias then probe_id will be used in TLGs.",
    #                    class="text-secondary"),
    #             fluidRow(
    #                 selectInput("has_alias",
    #                             "Has alias?",
    #                             c("All", "No", "Yes"))
    #             ),
    #             DT::dataTableOutput("probe_dict")
    #         ),
    #         tags$div(
    #             class="row",
    #             tags$p("Match Rate",
    #                    class="h6 text-secondary fw-bold"),
    #             tags$p(msg,
    #                    class="h6 text-secondary"),
    #             tags$p(""),
    #             tags$br(),
    #             tags$div(
    #                 class="col",
    #                 downloadButton("probe_dict_download_csv", 
    #                                "Download Probe Dict as CSV")
    #             )
    #         )
    #     )
    #     
    #     insertUI(
    #         selector = "#main_contents",
    #         where = "afterEnd",
    #         ui = tags$div(
    #             id = "main_contents1",
    #             tags$div(
    #                 class="row",
    #                 tags$div(
    #                     class="col",
    #                     id = "main_contents_col1",
    #                     insert_me1
    #                 ),
    #                 tags$div(
    #                     class="col",
    #                     id = "main_contents_col2",
    #                     tags$div(
    #                         id = "main_contents_col2_topmark_div",
    #                     ),
    #                     insert_me2
    #                 )
    #             )
    #         )
    #     )
    #     
    # }) |> bindEvent(input$load_all_data)
    
    # 2b. go to select projects page
    observe({
        if (isTruthy(input$select_projects)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
        }
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                card(
                    card_image("www/img/data_loaded1.jpg"),
                    card_body(
                        actionButton("load_project",
                                     "Load Projects",
                                     class = "btn-secondary w-100",
                                     duration = TRUE)
                    )
                ),
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_front_page",
                    "Go Back To Front Page",
                    class="wt-100"
                )
            )
        )
        
        table_front_page = input_react_vals$table_front_page
        
        N = sum(as.numeric(table_front_page$results_n), na.rm = T)
        
        output$projects_table = DT::renderDataTable(DT::datatable({
            data = table_front_page
            
            if (input$dose_type != "All") {
                data = data[data[["dose_type"]] == input$dose_type,]
            }
            if (input$proj_group != "All") {
                data = data[data[["project_group"]] == input$proj_group,]
            }
            data
        },
        options = list(pageLength = 8,
                       dom = "tpf",
                       columnDefs = list(
                           list(visible = FALSE, targets = ncol(table_front_page)),
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        insert_me1 = tags$div(
            tags$p("Select a project (or multiple projects)",
                   class="h3 text-primary fw-bold text-center"),
            tags$p("Click on row to select project(s) and proceed to
                   the next page",
                   class="h6 text-secondary text-center"),
            fluidRow(
                selectInput("proj_group",
                            "project_group",
                            c("All",
                              sort(unique(table_front_page[["project_group"]])))),
                selectInput("dose_type",
                            "dose_type",
                            c("All", "single_dose", "titration",
                              "multi_dose", "other"))
            ),
            DT::dataTableOutput("projects_table"),
            tags$p(paste0("Total Sample Size: ", N),
                   class="h5 text-secondary"),
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
        
    }) |> bindEvent(input$select_projects)
    
    # load selected projects and proceed to probe housekeeping
    observe({
        if (is.null(input$projects_table_rows_selected)) {
            showNotification("Select one or more projects.", 
                             type = "message", 
                             duration = 2)
        }else{
            if (isTruthy(input$load_project)) {
                removeUI(selector = "#main_contents1")
                removeUI(selector = "#load_project")
                removeUI(selector = "#card_div")
            }
            
            insertUI(
                selector="#card_div_top",
                where = "afterEnd",
                ui = tags$div(
                    id = "card_div",
                    tags$p("Session name",
                           class="h6 text-primary fw-bold"),
                    tags$p(input_react_vals$session_name,
                           class="h6 text-secondary"),
                    tags$br(),
                    actionButton(
                        "go_back_to_front_page",
                        "Go Back To Front Page",
                        class="wt-100"
                    )
                )
            )
            
            table_front_page = input_react_vals$table_front_page
            
            sel = rep(TRUE, nrow(table_front_page))
            if (input$dose_type != "All") {
                sel = sel & table_front_page[["dose_type"]] == input$dose_type
            }
            if (input$proj_group != "All") {
                sel = sel & table_front_page[["project_group"]] == input$proj_group
            }
            table_front_page_sub = table_front_page[sel,]
            
            sel_projs = input$projects_table_rows_selected
            selected_project_paths = table_front_page_sub[["full_path"]][sel_projs]
            selected_proj_group = table_front_page_sub[["proj_group"]][sel_projs]
            selected_proj_name = table_front_page_sub[["project_name"]][sel_projs]
            selected_proj_ch = table_front_page[["mfi_channel"]][sel_projs]
            
            tmp = list()
            for (k in 1:length(selected_project_paths)) {
                tmp_dat = read.csv(selected_project_paths[k], 
                                   header = TRUE, 
                                   check.names = FALSE)
                tmp_dat$project_group = selected_proj_group[k]
                tmp_dat$project_name = selected_proj_name[k]
                tmp_dat$mfi_channel = selected_proj_ch[k]
                tmp[[k]] = tmp_dat
            }
            dat = do.call(rbind, tmp)
            to_keep = grep("^Keep", dat[["to_drop"]])
            dat = dat[to_keep,,drop=F]
            rownames(dat) = NULL
            
            probe_id = sort(unique(dat[["Probe ID"]]))
            target_spec_id = sort(unique(dat[["Target Spec ID"]]))
            
            probe_dict = data.frame(probe_id = probe_id,
                                    has_alias = "No",
                                    probe_alias = "no_alias")
            
            target_spec_dict = data.frame(target_spec_id = target_spec_id,
                                          has_alias = "No",
                                          target_spec_alias = "no_alias")
            
            probe_id_dict = read.csv(paste0(app_dir, 
                                            "www/docs/probe_dict/20260712-probe-info.csv"),
                                     header = T, check.names = F)
            
            probe_alias = probe_id_dict[["probe_alias"]]
            names(probe_alias) = probe_id_dict[["probe_id"]]
            
            probe_alias_ = probe_alias[probe_dict$probe_id]
            is_updated = !is.na(probe_alias_)
            probe_dict$has_alias[is_updated] = "Yes"
            probe_alias_[!is_updated] = "no_alias"
            probe_dict$probe_alias = probe_alias_
            
            target_id_dict = read.csv(paste0(app_dir, 
                                             "www/docs/target_dict/20260712-target-dict.csv"),
                                      header = T, check.names = F)
            
            target_alias = target_id_dict[["target_spec_alias"]]
            names(target_alias) = target_id_dict[["target_spec_id"]]
            
            target_alias_ = target_alias[target_spec_dict$target_spec_id]
            is_updated = !is.na(target_alias_)
            target_spec_dict$has_alias[is_updated] = "Yes"
            target_alias_[!is_updated] = "no_alias"
            target_spec_dict$target_spec_alias = target_alias_
            
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
            options = list(pageLength = 5,
                           dom = "tpf",
                           columnDefs = list(
                               list(className = 'dt-nowrap', targets = '_all'))
            )))
            
            insert_me1 = tags$div(
                id = "ref_probe_id_div",
                tags$div(
                    class="row",
                    tags$div(
                        id = "ref_probe_id_div2",
                        tags$p("Probe names (housekeeping)",
                               class="h3 text-primary fw-bold"),
                        tags$p("Load a probe_id dictionary sheet.",
                               class="text-secondary")
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
                                     "Update Probe ID Alias",
                                     class="btn-warning",
                                     width="100%",
                                     duration = FALSE)
                    ),
                    tags$div(
                        class="col",
                        id = "proceed_to_target_id",
                        actionButton("target_id",
                                     "Proceed to Target IDs",
                                     class="btn-secondary",
                                     width="100%")
                    )
                )
            )
            
            check = probe_dict$has_alias == "Yes"
            msg = paste0(sum(check), " / ", length(check),
                         " (", round(mean(check)*100, 2),
                         "%) probe_ids have a probe_alias.")
            
            insert_me2 = tags$div(
                tags$div(
                    class="row",
                    tags$p("Probe alias table",
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
                    tags$p(""),
                    tags$br(),
                    tags$div(
                        class="col",
                        downloadButton("probe_dict_download_csv", "Download Probe Dict as CSV")
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
            
        }
        
    }) |> bindEvent(input$load_project)
    
    # 3. update probe ids
    observe({
        if (length(input$file1$datapath)==0) {
            showNotification(
                ui = paste("Specify a CSV file."),
                type = "message",
                duration = 2
            )
        }else{
            probe_id_dict = read.csv(input$file1$datapath, header = TRUE)
            check = all(c("probe_id", "probe_alias") %in% colnames(probe_id_dict))
            
            if (check) {
                removeUI(selector = "#main_contents1")
                
                # update probe_dict
                probe_dict = input_react_vals$probe_dict
                probe_alias = probe_id_dict[["probe_alias"]]
                names(probe_alias) = probe_id_dict[["probe_id"]]
                probe_alias_ = probe_alias[probe_dict$probe_id]
                is_updated = !is.na(probe_alias_)
                probe_dict$has_alias[is_updated] = "Yes"
                probe_alias_[!is_updated] = probe_dict$probe_id[!is_updated]
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
                options = list(pageLength = 5,
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
                            tags$p("Probe names (housekeeping)",
                                   class="h3 text-primary fw-bold"),
                            tags$p("Load a probe_id dictionary sheet.",
                                   class="text-secondary")
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
                                         "Update Probe ID Alias",
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
                        )
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
                        tags$p("Probe alias table",
                               class="h3 text-primary fw-bold"),
                        tags$p("Check whether plate info. probe_id has an alias.
                                If has no_alias then probe_id will be used in 
                                TLGs.",
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
                               class="h6 text-secondary")
                    ),
                    tags$p(""),
                    tags$br(),
                    tags$div(
                        class="col",
                        downloadButton("probe_dict_download_csv", 
                                       "Download Probe Dict as CSV")
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
                    ui = paste("Uploaded file must have 'probe_id' and 
                               'probe_alias' columns."),
                    type = "message",
                    duration = NULL
                )
            }
        }
        
    }) |> bindEvent(input$add_probe_alias)
    observe({
        removeUI(selector = "#main_contents1")
        removeUI(selector = "#card_div")
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_front_page",
                    "Go Back To Front Page",
                    class="wt-100"
                )
            )
        )
        
        probe_dict = input_react_vals$probe_dict
        
        # update page
        output$probe_dict_u = DT::renderDataTable(DT::datatable({
            data = probe_dict
            if (input$has_alias != "All") {
                data = data[data[["has_alias"]] == input$has_alias,]
            }
            data
        },
        selection = 'none',
        options = list(pageLength = 5,
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
                    tags$p("Probe names (housekeeping)",
                           class="h3 text-primary fw-bold"),
                    tags$p("Load a probe_id dictionary sheet.",
                           class="text-secondary")
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
                                 "Update Probe ID Alias",
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
                )
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
                tags$p("Probe alias table",
                       class="h3 text-primary fw-bold"),
                tags$p("Check whether plate info. probe_id has an alias.
                                If has no_alias then probe_id will be used in 
                                TLGs.",
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
                       class="h6 text-secondary")
            ),
            tags$p(""),
            tags$br(),
            tags$div(
                class="col",
                downloadButton("probe_dict_download_csv", "Download Probe Dict as CSV")
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
    }) |> bindEvent(input$go_back_to_probe_name)
    
    # 4. update target ids
    observe({
        if (isTruthy(input$target_id)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
        }
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_probe_name",
                    "Go Back To Probe Name",
                    class="wt-100"
                )
            )
        )
        
        target_spec_dict = input_react_vals$target_spec_dict
        
        output$target_spec_dict_table = DT::renderDataTable(DT::datatable({
            data = target_spec_dict
            if (input$has_alias != "All") {
                data = data[data[["has_alias"]] == input$has_alias,]
            }
            data
        },
        selection = 'none',
        options = list(pageLength = 5,
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
                    tags$p("Target specimen name (housekeeping)",
                           class="h3 text-primary fw-bold"),
                    tags$p("Load a target_spec_id dictionary sheet.",
                           class="text-secondary")
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
                                 width="100%",
                                 duration = FALSE)
                ),
                tags$div(
                    class="col",
                    id = "proceed_to_cell_conc",
                    actionButton("proceed_to_cell_conc",
                                 "Cell Lines & Probe Concentration",
                                 class="btn-secondary",
                                 width="100%")
                )
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
                tags$p("Target specimen alias table",
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
                tags$p(""),
                tags$br(),
                tags$div(
                    class="col",
                    downloadButton("target_dict_download_csv", "Download Target Dict as CSV")
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
        
    }) |> bindEvent(input$target_id)
    observe({
        if (length(input$file2$datapath)==0) {
            showNotification(
                ui = paste("Specify a CSV file."),
                type = "message",
                duration = 2
            )
        }else{
            target_id_dict = read.csv(input$file2$datapath,
                                      header = T)
            check = all(c("target_spec_id", "target_spec_alias") %in% colnames(target_id_dict))
            
            if (check) {
                removeUI(selector = "#main_contents1")
                
                target_dict = input_react_vals$target_spec_dict
                target_alias = target_id_dict[["target_spec_alias"]]
                names(target_alias) = target_id_dict[["target_spec_id"]]
                target_alias_ = target_alias[target_dict$target_spec_id]
                is_updated = !is.na(target_alias_)
                target_dict$has_alias[is_updated] = "Yes"
                target_alias_[!is_updated] = target_dict$target_spec_id[!is_updated]
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
                options = list(pageLength = 5,
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
                            tags$p("Target specimen name (housekeeping)",
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
                            id = "proceed_to_cell_conc",
                            actionButton("proceed_to_cell_conc",
                                         "Cell Lines & Probe Concentration",
                                         class="btn-secondary",
                                         width="100%")
                        )
                    )
                )
                
                check = target_dict$has_alias == "Yes"
                msg = paste0(sum(check), " / ", length(check),
                             " (", round(mean(check)*100, 2),
                             "%) target_spec_ids have a target_spec_alias.")
                
                insert_me2 = tags$div(
                    tags$div(
                        class="row",
                        tags$p("Target specimen alias table",
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
                        tags$p(""),
                        tags$br(),
                        tags$div(
                            class="col",
                            downloadButton("target_dict_download_csv", "Download Target Dict as CSV")
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
            } else {
                showNotification(
                    ui = paste("Uploaded file must have 'target_spec_id' and 
                               'target_spec_alias' columns."),
                    type = "message",
                    duration = NULL
                )
            }
        }
        
        
        
    }) |> bindEvent(input$add_target_alias)
    observe({
        removeUI(selector = "#main_contents1")
        removeUI(selector = "#card_div")
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_probe_name",
                    "Go Back To Probe Name",
                    class="wt-100"
                )
            )
        )
        
        target_dict = input_react_vals$target_spec_dict
        
        output$target_spec_dict_table2 = DT::renderDataTable(DT::datatable({
            data = target_dict
            if (input$has_alias != "All") {
                data = data[data[["has_alias"]] == input$has_alias,]
            }
            data
        },
        selection = 'none',
        options = list(pageLength = 5,
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
                    tags$p("Target specimen name (housekeeping)",
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
                    id = "proceed_to_cell_conc",
                    actionButton("proceed_to_cell_conc",
                                 "Cell Lines & Probe Concentration",
                                 class="btn-secondary",
                                 width="100%")
                )
            )
        )
        
        check = target_dict$has_alias == "Yes"
        msg = paste0(sum(check), " / ", length(check),
                     " (", round(mean(check)*100, 2),
                     "%) target_spec_ids have a target_spec_alias.")
        
        insert_me2 = tags$div(
            tags$div(
                class="row",
                tags$p("Target specimen alias table",
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
                tags$p(""),
                tags$br(),
                tags$div(
                    class="col",
                    downloadButton("target_dict_download_csv", "Download Target Dict as CSV")
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
        
    }) |> bindEvent(input$go_back_to_target_name)
    
    # 5. proceed to query page
    observe({
        if (isTruthy(input$target_id)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
            bslib::toggle_sidebar(id="my_sidebar", open=FALSE)
        }
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_target_name",
                    "Go Back To Target Name",
                    class="wt-100"
                )
            )
        )
        
        dat = input_react_vals$dat
        probe_dict = input_react_vals$probe_dict
        target_spec_dict = input_react_vals$target_spec_dict
        
        cleaned_dat = clean_dat(dat, probe_dict, target_spec_dict)
        
        probe_conc = cleaned_dat[,"Probe Quant Value"]
        cell_lines = cleaned_dat[,"target_spec_name"]
        
        probe_conc_list = sort(unique(as.numeric(probe_conc)))
        names(probe_conc_list) = probe_conc_list
        probe_conc_list = c("All Concentrations"="All Conc.",
                            probe_conc_list)
        
        cell_lines_list = sort(unique(cell_lines))
        names(cell_lines_list) = cell_lines_list
        cell_lines_list = c("All Cell Lines"="All Cell Lines",
                            cell_lines_list)
        
        insert_me1 = tags$div(
            tags$p("Query database",
                   class="h3 text-primary fw-bold"),
            tags$p("Select cell lines and probe concentrations below and then submit.",
                   class="text-secondary"),
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            actionButton(
                                "submit_cell_lines_conc",
                                "Apply Selected",
                                class = "btn-warning",
                                width="100%",
                                disabled = FALSE
                            )
                        ),
                        tags$div(
                            class="col",
                            actionButton(
                                "proceed_to_cell_conc",
                                "Reset",
                                class="btn-primary",
                                width = "100%"
                            )
                        ),
                    )
                ),
                tags$div(
                    class="col",
                    ""
                )
                
            ),

            tags$p(""),
            # tags$br(),
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    tags$p("Experiment type",
                           class="h5 text-secondary fw-bold"),
                    radioButtons(
                        inputId = "select_expt_type",
                        label = "Select options below",
                        choices = list(
                            "Binding experiment" = "Binding",
                            "Internalization experiment" = "Internalization"
                        )
                    ),
                ),
                tags$div(
                    class="col",
                    tags$p("Dose type",
                           class="h5 text-secondary fw-bold"),
                    radioButtons(
                        inputId = "select_dose_type",
                        label = "Select options below",
                        choices = list(
                            "All dose types" = "All",
                            "Single Dose" = "single_dose",
                            "Titration" = "titration"
                        )
                    ),
                ),
            ),
            

            # tags$br(),
            tags$div(
                class="col",
                tags$div(
                    class="row",
                    tags$div(
                        class="col",
                        tags$p("Cell lines",
                               class="h5 text-secondary fw-bold"),
                        selectizeInput(
                            "select_cell_lines",
                            "Select cell line(s) -- in preferred order",
                            cell_lines_list,
                            multiple = TRUE,
                            width = "100%"
                        ),
                        # fileInput("file_target",
                        #           "Or upload a CSV with selected target(s)",
                        #           accept=".csv"),
                    ),
                    tags$div(
                        class="col",
                        tags$p("Probe concentration (ug/ml)",
                               class="h5 text-secondary fw-bold"),
                        selectizeInput(
                            "select_probe_conc",
                            "Select probe concentration(s)",
                            probe_conc_list,
                            multiple = TRUE,
                            width = "100%"
                        )
                    )
                )
            ),
        )
        
        insert_me2 = tags$div(
            tags$p("Probe selection",
                   class="h3 text-primary fw-bold"),
            
            id = "probe_table_div_top",
            
            tags$div(
                id = "probe_table_div",
                tags$p("Select cell lines and concentration in order to see probe table",
                       class="text-secondary"),
            )
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
                        tags$div(id = "main_contents_col2_1"),
                        insert_me2
                    )
                )
            )
        )
        
    }) |> bindEvent(input$proceed_to_cell_conc)
    
    # 6. apply cell lines and concentrations query
    probe_query_react_value = reactiveVal()
    observe({
        if (length(input$select_probe_conc) > 0 & length(input$select_cell_lines) > 0) {
            updateActionButton(session, 
                               "submit_cell_lines_conc", 
                               disabled=TRUE)
            
            removeUI(selector = "#probe_table_div")
            
            dat = input_react_vals$dat
            target_spec_dict = input_react_vals$target_spec_dict
            probe_dict = input_react_vals$probe_dict
            
            cleaned_dat = clean_dat(dat, probe_dict, target_spec_dict)
            
            mfi_channel = cleaned_dat[,"mfi_channel"]
            target_spec_name = cleaned_dat[,"target_spec_name"]
            probe_conc = cleaned_dat[,"Probe Quant Value"]
            probe_name = cleaned_dat[,"probe_name"]
            dose_type = cleaned_dat[,"dose_type"]
            

            if (input$select_expt_type=="Binding") {
                select_expt_type = mfi_channel %in% "RL1-H"
            }
            if (input$select_expt_type=="Internalization") {
                select_expt_type = mfi_channel %in% "YL1-H"
            }
            
            if (input$select_dose_type=="All") {
                select_dose_type = rep(TRUE, nrow(cleaned_dat))
            }else{
                select_dose_type = dose_type %in% input$select_dose_type
            }
           
            if (is.null(input$select_cell_lines)) {
                showNotification("Select at least one cell line.", 
                                 type = "message", duration = 5)
            }else{
                if ("All Cell Lines" %in% input$select_cell_lines) {
                    select_cell_lines = rep(TRUE, nrow(cleaned_dat))
                }else{
                    select_cell_lines = target_spec_name %in% input$select_cell_lines
                }
            }
            
            if (is.null(input$select_probe_conc)) {
                showNotification("Select at least one concentration.", 
                                 type = "message", duration = 5)
            }else{
                if ("All Conc." %in% input$select_probe_conc) {
                    select_probe_conc = rep(TRUE, nrow(cleaned_dat))
                }else{
                    select_probe_conc = probe_conc %in% input$select_probe_conc
                }
            }
            
            sel_rows = select_dose_type+select_expt_type+select_cell_lines+select_probe_conc==4
            select_probe_names_ = probe_name[sel_rows]
            
            if (length(select_probe_names_) == 0) {
                insertUI(
                    selector = "#probe_table_div_top",
                    where = "afterEnd",
                    ui = tags$div(
                        tags$div(
                            id = "probe_table_div",
                            tags$p("Select probes of interest.",
                                   class="text-secondary"),
                            tags$p("There are no samples that match your query!",
                                   class="h6 text-warning"),
                        )
                    )
                )
            }else{
                select_probe_names = sort(unique(select_probe_names_))
                probe_df = data.frame(probe_name=select_probe_names,
                                      selected=rep("false", length(select_probe_names)))
                
                probe_query_react_value(probe_df)
                
                output$probe_query_table = DT::renderDataTable(DT::datatable({
                    probe_query_react_value()
                },
                selection = 'multiple',
                options = list(pageLength = 6,
                               dom = "tpf",
                               columnDefs = list(
                                   list(className = 'dt-nowrap', targets = '_all'))
                )))
                
                msg_selected = paste0(sum(!probe_df$selected=="false"), " / ", 
                                      nrow(probe_df),
                                      " probes selected")
                
                insertUI(
                    selector = "#probe_table_div_top",
                    where = "afterEnd",
                    ui = tags$div(
                          tags$div(
                            id = "probe_table_div",
                            tags$p("Select probes of interest.",
                                   class="text-secondary"),
                            tags$div(
                                class="row",
                                tags$div(
                                    class="col",
                                    tags$p(paste0(nrow(probe_df), 
                                                  " probes match your query"),
                                           class="h6 text-primary"),
                                    downloadButton("probe_query_download_csv", 
                                                   "Download Probe List as CSV"),
                                    DT::dataTableOutput("probe_query_table"),
                                    tags$div(
                                        id = "probe_query_table_bottom",
                                    ),
                                    tags$div(
                                        id = "probe_sel_msg_div",
                                        tags$p(msg_selected,
                                               class="h6 text-secondary"),
                                    ),
                                ),
                                tags$div(
                                    class="col",
                                    tags$div(
                                        id="probe_sel_div_top",
                                        
                                        tags$div(
                                            id = "probe_sel_div",
                                            tags$p("Proceed after making selection(s)",
                                                   class="h6 txt-secondary"),
                                            tags$div(
                                                class="row",
                                                tags$div(
                                                    class="col",
                                                    actionButton(
                                                        "proceed_to_order_probes",
                                                        "Next",
                                                        class="btn-secondary",
                                                        width = "100%",
                                                        disabled = TRUE
                                                    ),
                                                ),
                                                tags$div(
                                                    class="col",
                                                    actionButton(
                                                        "reset_probes",
                                                        "Reset",
                                                        class="btn-primary",
                                                        width = "100%"
                                                    ),
                                                ),
                                            ),
                                            
                                            tags$p(""),
                                            tags$br(),
                                            tags$p("Select probe name(s)",
                                                   class="h5 text-secondary fw-bold"),
                                            tags$div(
                                                class="row",
                                                tags$div(
                                                    class="col",
                                                    actionButton(
                                                        "apply_select_probes",
                                                        "Add Selected",
                                                        class="btn-warning",
                                                        disabled = FALSE,
                                                        width = "100%"
                                                    ),
                                                ),
                                                tags$div(
                                                    class="col",
                                                    actionButton(
                                                        "drop_select_probes",
                                                        "Drop Selected",
                                                        class="btn-warning",
                                                        disabled = FALSE,
                                                        width = "100%"
                                                    ),
                                                ),
                                            ),
                                            tags$div(
                                                id="select_probe_names_div_top"
                                            ),
                                            tags$div(
                                                id="select_probe_names_div",
                                                tags$p(""),
                                                fileInput("file_probe", 
                                                          "Upload a CSV with selected probes", 
                                                          accept=".csv"),
                                                selectizeInput(
                                                    "probe_selections",
                                                    "Or select a few probe(s) here",
                                                    c("All Probes", select_probe_names),
                                                    multiple = TRUE
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            }
            
            input_react_vals$cleaned_dat = cleaned_dat
            input_react_vals$sel_rows = sel_rows
            
        }else{
            showNotification(
                ui = "Make selections for both Cell lines and Probe oncentration (ug/ml).",
                type = "message",
                duration = 5
            )
        }

    }) |> bindEvent(input$submit_cell_lines_conc)

    # 7. apply probe selection
    probe_order_react_val = reactiveVal()
    observe({
        case0 = length(input$probe_query_table_rows_selected) > 0
        case1 = length(input$file_probe$datapath)==0 & length(input$probe_selections)==0
        case2 = length(input$file_probe$datapath) >0 & length(input$probe_selections) >0
        case3 = length(input$file_probe$datapath) >0 & length(input$probe_selections)==0
        case4 = length(input$file_probe$datapath)==0 & length(input$probe_selections) >0
        
        probe_df = probe_query_react_value()
        
        if (case0) {
            
            sel = input$probe_query_table_rows_selected
            probe_df[sel,"selected"] = "true"
            probe_query_react_value(probe_df)
            
        }else{
            
            
            if (case1) {
                showNotification(
                    ui = "Make a probe selection to proceed.",
                    type = "message",
                    duration = 5
                )
            }else{
                if (case2 | case4) {
                    removeUI(selector = "#probe_sel_msg_div")
                    
                    if ("All Probes" %in% input$probe_selections) {
                        probe_df[,"selected"] = rep("true", nrow(probe_df))
                        probe_query_react_value(probe_df)
                        
                    }else{
                        x = probe_df[,"probe_name"] %in% input$probe_selections
                        probe_df[,"selected"][x] = "true"
                        probe_query_react_value(probe_df)
                    }
                    
                }else{
                    
                    if (case3) {
                        probe_csv = read.csv(input$file_probe$datapath, header = T)
                        check = "probe_name" %in% colnames(probe_csv)
                        
                        if (check) {
                            x = probe_df[,"probe_name"] %in% probe_csv[,"probe_name"]
                            
                            if (sum(x) > 0) {
                                probe_df[,"selected"][x] = "true"
                                probe_query_react_value(probe_df)
                                
                            }else{
                                showNotification(
                                    ui = "None of the probes in CSV are valid. Please check and try again.",
                                    type = "message",
                                    duration = 5
                                )
                            }
                        }else{
                            showNotification(
                                ui = paste("Uploaded file must have a 'probe_name' column."),
                                type = "message",
                                duration = 5
                            )
                        }
                    }
                }
            }
        }
        
        if ((case0+case1+case2+case3+case4) > 0) {
            
            removeUI(selector = "#probe_sel_msg_div")
            msg_selected = paste0(sum(!probe_df$selected=="false"), " / ", 
                                  nrow(probe_df),
                                  " probes selected")
            insertUI(
                "#probe_query_table_bottom",
                "afterEnd",
                ui = tags$div(
                    id = "probe_sel_msg_div",
                    tags$p(msg_selected,
                           class="h6 text-secondary"),
                )
            )
            
            # removeUI(selector = "#select_probe_names_div")
            # insertUI(
            #     "#select_probe_names_div_top",
            #     "afterEnd",
            #     ui = tags$div(
            #         id="select_probe_names_div",
            #         tags$p(""),
            #         fileInput("file_probe", 
            #                   "Upload a CSV with selected probes", 
            #                   accept=".csv"),
            #         selectizeInput(
            #             "probe_selections",
            #             "Or select a few probe(s) here",
            #             c("All Probes", probe_df$probe_name),
            #             multiple = TRUE
            #         )
            #     )
            # )
            
            output$probe_query_table = DT::renderDataTable(DT::datatable({
                probe_query_react_value()
            },
            selection = 'multiple',
            options = list(pageLength = 6,
                           dom = "tpf",
                           columnDefs = list(
                               list(className = 'dt-nowrap', 
                                    targets = '_all'))
            )))
        }
        
        if (any(probe_df$selected=="true")) {
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=FALSE)
        }else{
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=TRUE)
        }
        
    }) |> bindEvent(input$apply_select_probes)
    observe({
        case0 = length(input$probe_query_table_rows_selected) > 0
        case1 = length(input$file_probe$datapath)==0 & length(input$probe_selections)==0
        case2 = length(input$file_probe$datapath) >0 & length(input$probe_selections) >0
        case3 = length(input$file_probe$datapath) >0 & length(input$probe_selections)==0
        case4 = length(input$file_probe$datapath)==0 & length(input$probe_selections) >0
        
        probe_df = probe_query_react_value()
        
        if (case0) {
            sel = input$probe_query_table_rows_selected
            probe_df[sel,"selected"] = "false"
            probe_query_react_value(probe_df)
            
        }else{
            
            if (case1) {
                showNotification(
                    ui = "Make a probe selection to proceed.",
                    type = "message",
                    duration = 3
                )
            }else{
                if (case2 | case4) {
                    removeUI(selector = "#probe_sel_msg_div")
                    
                    if ("All Probes" %in% input$probe_selections) {
                        probe_df[,"selected"] = rep("false", nrow(probe_df))
                        probe_query_react_value(probe_df)
                        
                    }else{
                        x = probe_df[,"probe_name"] %in% input$probe_selections
                        probe_df[,"selected"][x] = "false"
                        probe_query_react_value(probe_df)
                    }
                    
                }else{
                    
                    if (case3) {
                        probe_csv = read.csv(input$file_probe$datapath, header = T)
                        check = "probe_name" %in% colnames(probe_csv)
                        
                        if (check) {
                            x = probe_df[,"probe_name"] %in% probe_csv[,"probe_name"]
                            
                            if (sum(x) > 0) {
                                probe_df[,"selected"][x] = "false"
                                probe_query_react_value(probe_df)
                                
                            }else{
                                showNotification(
                                    ui = "None of the probes in CSV are valid. Please check and try again.",
                                    type = "message",
                                    duration = 3
                                )
                            }
                        }else{
                            showNotification(
                                ui = paste("Uploaded file must have a 'probe_name' column."),
                                type = "message",
                                duration = 3
                            )
                        }
                    }
                }
            }
        }
        
        if ((case0+case1+case2+case3+case4) > 0) {
            
            removeUI(selector = "#probe_sel_msg_div")
            msg_selected = paste0(sum(!probe_df$selected=="false"), " / ", 
                                  nrow(probe_df),
                                  " probes selected")
            insertUI(
                "#probe_query_table_bottom",
                "afterEnd",
                ui = tags$div(
                    id = "probe_sel_msg_div",
                    tags$p(msg_selected,
                           class="h6 text-secondary"),
                )
            )
            
            output$probe_query_table = DT::renderDataTable(DT::datatable({
                probe_query_react_value()
            },
            selection = 'multiple',
            options = list(pageLength = 6,
                           dom = "tpf",
                           columnDefs = list(
                               list(className = 'dt-nowrap', 
                                    targets = '_all'))
            )))
        }
        
        if (any(probe_df$selected=="true")) {
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=FALSE)
        }else{
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=TRUE)
        }
        
    }) |> bindEvent(input$drop_select_probes)
    observe({
        # after making checks
        if (isTruthy(input$proceed_to_order_probes)) {
            removeUI(selector = "#probe_table_div")
        }
        
        probe_df = probe_query_react_value()
        probes_order_df = probe_df[probe_df$selected=="true",,drop=F]
        
        probes_order_df$group = rep("A", nrow(probes_order_df))
        probes_order_df$metric = 1:nrow(probes_order_df)
        probes_order_df$selected = NULL
        
        probe_order_react_val(probes_order_df)
        
        output$probes_order_table = DT::renderDataTable(DT::datatable({
            probes_order_df
        },
        selection = 'none',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))

        insertUI(
            selector = "#probe_table_div_top",
            where = "afterEnd",
            ui = tags$div(
                tags$div(
                    id = "probe_table_div",
                    tags$p("Order selected probes. The default order is alphabetical.",
                           class="text-secondary"),
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            tags$p(paste0(nrow(probes_order_df), 
                                          " probes selected"),
                                   class="h6 text-primary"),
                            downloadButton("probe_order_query_download_csv", 
                                           "Download Probe Order as CSV"),
                            DT::dataTableOutput("probes_order_table"),
                            tags$div(
                                id = "probe_query_table_bottom",
                            ),
                        ),
                        tags$div(
                            class="col",
                            tags$div(
                                id="probe_sel_div_top",
                                
                                tags$div(
                                    id = "probe_sel_div",
                                    tags$p("Proceed after making selection(s)",
                                           class="h6 txt-secondary"),
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "back_to_order_probes",
                                                "Back",
                                                class="btn-secondary",
                                                width = "100%",
                                                disabled = FALSE
                                            ),
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "reset_probes",
                                                "Reset",
                                                class="btn-primary",
                                                width = "100%"
                                            ),
                                        ),
                                    ),
                                    
                                    tags$p(""),
                                    tags$br(),
                                    tags$p("Define probe order",
                                           class="h5 text-secondary fw-bold"),

                                    tags$div(
                                        id="select_probe_names_div_top"
                                    ),
                                    tags$div(
                                        id="select_probe_names_div",
                                        tags$p(""),
                                        fileInput("file_order_probe", 
                                                  "Upload a CSV with probe order (optional)", 
                                                  accept=".csv")
                                    ),
                                    
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "update_probe_order",
                                                "Update Probe Order",
                                                class="btn-warning",
                                                width = "100%",
                                                disabled = FALSE
                                            )
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "proceed_to_annot_page",
                                                "Go to Annotation Page",
                                                width = "100%",
                                                class="btn-warning",
                                                disabled = FALSE
                                            )
                                        )
                                    ),
                        
                                )
                            )
                        )
                    )
                )
            )
        )

    }) |> bindEvent(input$proceed_to_order_probes)
    observe({
        if (isTruthy(input$back_to_order_probes)) {
            removeUI(selector = "#probe_table_div")
        }
        
        probe_df = probe_query_react_value()
        select_probe_names = probe_df$probe_name

        msg_selected = paste0(sum(!probe_df$selected=="false"), " / ", 
                              nrow(probe_df),
                              " probes selected")
        
        insertUI(
            selector = "#probe_table_div_top",
            where = "afterEnd",
            ui = tags$div(
                tags$div(
                    id = "probe_table_div",
                    tags$p("Select probes of interest.",
                           class="text-secondary"),
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            tags$p(paste0(nrow(probe_df), 
                                          " probes match your query"),
                                   class="h6 text-primary"),
                            downloadButton("probe_query_download_csv", 
                                           "Download Probe List as CSV"),
                            DT::dataTableOutput("probe_query_table"),
                            tags$div(
                                id = "probe_query_table_bottom",
                            ),
                            tags$div(
                                id = "probe_sel_msg_div",
                                tags$p(msg_selected,
                                       class="h6 text-secondary"),
                            ),
                        ),
                        tags$div(
                            class="col",
                            tags$div(
                                id="probe_sel_div_top",
                                
                                tags$div(
                                    id = "probe_sel_div",
                                    tags$p("Proceed after making selection(s)",
                                           class="h6 txt-secondary"),
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "proceed_to_order_probes",
                                                "Next",
                                                class="btn-secondary",
                                                width = "100%",
                                                disabled = FALSE
                                            ),
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "reset_probes",
                                                "Reset",
                                                class="btn-primary",
                                                width = "100%"
                                            ),
                                        ),
                                    ),
                                    
                                    tags$p(""),
                                    tags$br(),
                                    tags$p("Select probe name(s)",
                                           class="h5 text-secondary fw-bold"),
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "apply_select_probes",
                                                "Add Selected",
                                                class="btn-warning",
                                                disabled = FALSE,
                                                width = "100%"
                                            ),
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "drop_select_probes",
                                                "Drop Selected",
                                                class="btn-warning",
                                                disabled = FALSE,
                                                width = "100%"
                                            ),
                                        ),
                                    ),
                                    tags$div(
                                        id="select_probe_names_div_top"
                                    ),
                                    tags$div(
                                        id="select_probe_names_div",
                                        tags$p(""),
                                        fileInput("file_probe", 
                                                  "Upload a CSV with selected probes", 
                                                  accept=".csv"),
                                        selectizeInput(
                                            "probe_selections",
                                            "Or select a few probe(s) here",
                                            c("All Probes", select_probe_names),
                                            multiple = TRUE
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        
        if (any(probe_df$selected=="true")) {
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=FALSE)
        }else{
            updateActionButton(session, 
                               "proceed_to_order_probes", 
                               disabled=TRUE)
        }
        
    }) |> bindEvent(input$back_to_order_probes)
    observe({
        if (isTruthy(input$reset_probes)) {
            removeUI(selector = "#probe_table_div")
        }
        
        probe_df = probe_query_react_value()
        select_probe_names = probe_df$probe_name
        probe_df$selected = rep("false", nrow(probe_df))
        probe_query_react_value(probe_df)
        
        msg_selected = paste0(sum(!probe_df$selected=="false"), " / ", 
                              nrow(probe_df),
                              " probes selected")
        
        insertUI(
            selector = "#probe_table_div_top",
            where = "afterEnd",
            ui = tags$div(
                tags$div(
                    id = "probe_table_div",
                    tags$p("Select probes of interest.",
                           class="text-secondary"),
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            tags$p(paste0(nrow(probe_df), 
                                          " probes match your query"),
                                   class="h6 text-primary"),
                            downloadButton("probe_query_download_csv", 
                                           "Download Probe List as CSV"),
                            DT::dataTableOutput("probe_query_table"),
                            tags$div(
                                id = "probe_query_table_bottom",
                            ),
                            tags$div(
                                id = "probe_sel_msg_div",
                                tags$p(msg_selected,
                                       class="h6 text-secondary"),
                            ),
                        ),
                        tags$div(
                            class="col",
                            tags$div(
                                id="probe_sel_div_top",
                                
                                tags$div(
                                    id = "probe_sel_div",
                                    tags$p("Proceed after making selection(s)",
                                           class="h6 txt-secondary"),
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "proceed_to_order_probes",
                                                "Next",
                                                class="btn-secondary",
                                                width = "100%",
                                                disabled = TRUE
                                            ),
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "reset_probes",
                                                "Reset",
                                                class="btn-primary",
                                                width = "100%"
                                            ),
                                        ),
                                    ),
                                    
                                    tags$p(""),
                                    tags$br(),
                                    tags$p("Select probe name(s)",
                                           class="h5 text-secondary fw-bold"),
                                    tags$div(
                                        class="row",
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "apply_select_probes",
                                                "Add Selected",
                                                class="btn-warning",
                                                disabled = FALSE,
                                                width = "100%"
                                            ),
                                        ),
                                        tags$div(
                                            class="col",
                                            actionButton(
                                                "drop_select_probes",
                                                "Drop Selected",
                                                class="btn-warning",
                                                disabled = FALSE,
                                                width = "100%"
                                            ),
                                        ),
                                    ),
                                    tags$div(
                                        id="select_probe_names_div_top"
                                    ),
                                    tags$div(
                                        id="select_probe_names_div",
                                        tags$p(""),
                                        fileInput("file_probe", 
                                                  "Upload a CSV with selected probes", 
                                                  accept=".csv"),
                                        selectizeInput(
                                            "probe_selections",
                                            "Or select a few probe(s) here",
                                            c("All Probes", select_probe_names),
                                            multiple = TRUE
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        
    }) |> bindEvent(input$reset_probes)
    observe({
        
        path2_probe_order = input$file_order_probe$datapath
        
        if (length(path2_probe_order)==0) {
            showNotification(
                ui = paste("Upload a file."),
                type = "message",
                duration = 3
            )
        }else{
            orig_probe_order = probe_order_react_val()
            
            probe_order_in = read.csv(path2_probe_order,
                                      header = T, 
                                      check.names = F)
            
            check1 = all(colnames(probe_order_in) %in% colnames(orig_probe_order))
            
            if (check1) {
                
                check2 = nrow(probe_order_in) == nrow(orig_probe_order)
                
                if (check2) {
                    
                    check3 = all(probe_order_in$probe_name == orig_probe_order$probe_name)
                    
                    if (check3) {
                        
                        o = order(probe_order_in$group, probe_order_in$metric)
                        probes_order_df = probe_order_in[o,colnames(orig_probe_order),drop=F]
                        probe_order_react_val(probes_order_df)
                        rownames(probes_order_df) = NULL
                        
                        probe_order_react_val(probes_order_df)
                        
                        output$probes_order_table = DT::renderDataTable(DT::datatable({
                            probes_order_df
                        },
                        selection = 'none',
                        options = list(pageLength = 6,
                                       dom = "tpf",
                                       columnDefs = list(
                                           list(className = 'dt-nowrap', targets = '_all'))
                        )))
                        
                    }else{
                        
                        showNotification(
                            ui = paste("Uploaded file probe_names do not match table probe_names."),
                            type = "message",
                            duration = 3
                        )
                        
                    }
                }else{
                    
                    showNotification(
                        ui = paste("Uploaded file nrow do not match table nrow."),
                        type = "message",
                        duration = 3
                    )
                    
                }
            }else{
                
                showNotification(
                    ui = paste("Uploaded file colnames do not match table colnames."),
                    type = "message",
                   
                     duration = 3
                )
            }
            
            
        }
        
    }) |> bindEvent(input$update_probe_order)
    
    # 8. annotation and review page
    analysis_dataset = reactiveVal()
    observe({
        probe_df = probe_query_react_value()
        psel = probe_df[,"selected"] %in% "true"
    
        if (sum(psel) == 0) {
            showNotification(
                ui = "Select one or more probes to proceed.",
                type = "message",
                duration = 2
            )
        }else{
            
            if (isTruthy(input$proceed_to_annot_page)) {
                removeUI(selector = "#main_contents1")
                removeUI(selector = "#card_div")
            }
            
            insertUI(
                selector="#card_div_top",
                where = "afterEnd",
                ui = tags$div(
                    id = "card_div",
                    tags$p("Session name",
                           class="h6 text-primary fw-bold"),
                    tags$p(input_react_vals$session_name,
                           class="h6 text-secondary"),
                    tags$br(),
                    actionButton(
                        "proceed_to_cell_conc",
                        "Go Back To Query Page",
                        class="wt-100"
                    ),
                )
            )
            
            cleaned_dat = input_react_vals$cleaned_dat
            sel_rows = input_react_vals$sel_rows
            
            sel_probes = probe_df[,"probe_name"][psel]
            row_select = sel_rows+(cleaned_dat[,"probe_name"] %in% sel_probes)==2
            
            cleaned_dat$query_select = row_select
            input_react_vals$cleaned_dat = cleaned_dat
            
            scol = c("Platename",
                     "Plate Position",
                     "mfi_channel",
                     "dose_type",
                     "target_spec_name",
                     "probe_name",
                     "Probe Quant Value",
                     "gMFI")
            
            adam = cleaned_dat[row_select, scol, drop=F]
            
            pqt = cleaned_dat[,"Probe Quant Type"]
            upqt = unique(sapply(strsplit(pqt, "\\("), "[[", 2))
            
            if (length(upqt) > 1) {
                showNotification(
                    ui = "There are different units for probe concentration. Can not proceed. Talk to Kwame.",
                    type = "message",
                    duration = 2
                )
            }else{
                acn = c("platename",
                        "position",
                        "expr_type",
                        "dose_type",
                        "target_spec_name",
                        "probe_name",
                        "conc",
                        "mfi")
                acn[acn=="conc"] = paste0("conc (", upqt)
                
                colnames(adam) =  acn
                
                adam$expr_type = ifelse(adam$expr_type=="RL1-H","binding",
                                        ifelse(adam$expr_type=="YL1-H", "internalization", 
                                               "unknown"))
                
                adam$mfi = round(adam$mfi)
                plate_col = as.numeric(gsub("[A-Z]", "", adam[["position"]]))
                plate_row = substr(adam[["position"]], 1, 1)
                adam = cbind(plate_row=plate_row,
                             plate_col=plate_col,
                             adam)
                
                adam = dup_analysis(adam)
                rownames(adam) = NULL
                
                notes = rep("none", nrow(adam))
                Note = factor(notes, levels = c("none",
                                                "neg_ref",
                                                "benchmark",
                                                "drop"))
                adam$notes = Note
                
                probe_order_df = probe_order_react_val()
                probe_levels = probe_order_df$probe_name
                target_levels = input$select_cell_lines
                
                if ("All Cell Lines" %in% target_levels) {
                    target_levels = sort(as.character(unique(adam$target_spec_name)))
                }
                
                adam$probe_name = factor(as.character(adam$probe_name), 
                                         levels=probe_levels)
                adam$target_spec_name = factor(as.character(adam$target_spec_name), 
                                               levels=target_levels)
                
                analysis_dataset(adam)
                
                hide_cols = c(1, 2, 5, 6, 11)
                
                output$adam_table = DT::renderDataTable(DT::datatable({
                    data = adam
                    data$probe_name = as.character(data$probe_name)
                    data$target_spec_name = as.character(data$target_spec_name)
                    if (isTruthy(input$platename) && input$platename != "All")
                        data = data[data$platename == input$platename, , drop = FALSE]
                    if (isTruthy(input$plate_row) && input$plate_row != "All")
                        data = data[data$plate_row == input$plate_row, , drop = FALSE]
                    if (isTruthy(input$plate_col) && input$plate_col != "All")
                        data = data[data$plate_col == input$plate_col, , drop = FALSE]
                    if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
                        data = data[data$target_spec_name == input$target_spec_name, , drop = FALSE]
                    if (isTruthy(input$probe_name) && input$probe_name != "All")
                        data = data[data$probe_name == input$probe_name, , drop = FALSE]
                    if (isTruthy(input$duplicates) && input$duplicates != "All")
                        data = data[data$duplicates == input$duplicates, , drop = FALSE]
                    
                    data
                },
                selection = 'multiple',
                options = list(pageLength = 6,
                               dom = "tpf",
                               columnDefs = list(
                                   list(className = 'dt-nowrap', targets = '_all'),
                                   list(visible = FALSE, targets = hide_cols))
                )))
                
                output$notes_table = renderTable(
                    notes_summary_func(adam$notes), 
                    striped = TRUE,
                    spacing = "xs")
                
                adam_plot(adam, 
                          sample_names = NULL, 
                          group_name = NULL, 
                          filename = paste0(app_dir, 
                                            "www/img/adam-graph-home.pdf"), 
                          session_name = input_react_vals$session_name)
                
                filters = tags$div(
                    id = "all_filters_div",
                    fluidRow(
                        selectInput("platename",
                                    "platename",
                                    c("All",
                                      sort(unique(adam[["platename"]]))),
                                    width="275px"),
                        selectInput("plate_row",
                                    "plate_row",
                                    c("All",
                                      sort(unique(adam[["plate_row"]]))),
                                    width="275px"),
                        selectInput("plate_col",
                                    "plate_col",
                                    c("All",
                                      sort(unique(adam[["plate_col"]]))),
                                    width="275px"),
                        selectInput("target_spec_name",
                                    "target_spec_name",
                                    c("All",
                                      sort(unique(as.character(adam[["target_spec_name"]])))),
                                    width="275px"),
                        selectInput("probe_name",
                                    "probe_name",
                                    c("All",
                                      sort(as.character(unique(adam[["probe_name"]])))),
                                    width="275px"),
                        selectInput("duplicates",
                                    "duplicates",
                                    c("All",
                                      sort(unique(adam[["duplicates"]]))),
                                    width="275px")
                    )
                )
                
                # column1
                insert_me1 = tags$div(
                    tags$p(paste0("Analysis dataset -- sample size: ",
                                  nrow(adam), 
                                  " -- select rows to add notes"),
                           class="h5 text-primary fw-bold"),
                    
                    tags$div(
                        id="table_filter_div_top",
                    ),
                    
                    tags$div(
                        id="table_filter_div",
                        class = "col",
                        filters,
                        DT::DTOutput("adam_table"),
                        tags$div(
                            class="row",
                            tags$div(
                                class="col",
                                downloadButton("adam_download_csv",
                                               "Download Analysis Data as CSV")
                            ),
                            tags$div(
                                class="col",
                                ""
                            )
                        )
                    )
                )
                
                insert_me2 = tags$div(
                    id="annotation_pannel_div_top",
                    
                    tags$p("Annotation pannel",
                           class="h5 text-primary fw-bold"),
                    
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            radioButtons(
                                inputId = "note_radio",
                                label = "Label the selected probe(s)",
                                choices = list(
                                    "None" = "none",
                                    "Negative reference" = "neg_ref",
                                    "Benchmark" = "benchmark",
                                    "Drop" = "drop"
                                ),
                                inline = FALSE
                            )
                        ),
                        tags$div(
                            class="col",
                            tableOutput("notes_table"),
                        )
                    ),
                    
                    tags$div(
                        class="row",
                        tags$div(
                            class="col",
                            actionButton("annotate_smpl",
                                         "Note",
                                         class="btn-warning wt-50",
                                         width="100%")
                        ),
                        tags$div(
                            class="col",
                            actionButton("plot_table_selections",
                                         "Plot",
                                         class="btn-primary wt-50",
                                         width="100%")
                        ),
                        tags$div(
                            class="col",
                            actionButton("proceed_to_tlgs",
                                         "Proceed",
                                         class="btn-secondary",
                                         width="100%")
                        )
                    ),
                    tags$p(""),
                    
                    tags$div(id="adam_plot_div_top"),
                    
                    tags$div(
                        id="adam_plot_div",
                        
                        tags$div(
                            class="row",
                            tags$iframe(
                                src = "img/adam-graph-home.pdf",
                                width = "400px",
                                height = "320px",
                            )
                        ),
                        tags$p(""),
                        textInput(
                            "plot_name",
                            label = NULL,
                            placeholder = "(Optional) Enter Plot Name",
                            width="45%"
                        )
                    ),
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
                                tags$div(id="main_contents_col2_1"),
                                insert_me2
                            ),
                        )
                    )
                )
            }
            
        }
        
    }) |> bindEvent(input$proceed_to_annot_page)
    observe({
        row_indices = input$adam_table_rows_selected
    
        if (length(row_indices)==0) {
            showNotification("Select one or more rows.", 
                             type = "message", 
                             duration = 2)
        }else{
            
            adam = analysis_dataset()
            notes = adam$notes
            names(notes) = paste0(adam$platename, "|", adam$position)
            
            data = adam
            data$probe_name = as.character(data$probe_name)
            data$target_spec_name = as.character(data$target_spec_name)
            
            if (isTruthy(input$platename) && input$platename != "All")
                data = data[data$platename == input$platename,,drop = FALSE]
            if (isTruthy(input$plate_row) && input$plate_row != "All")
                data = data[data$plate_row == input$plate_row,,drop = FALSE]
            if (isTruthy(input$plate_col) && input$plate_col != "All")
                data = data[data$plate_col == input$plate_col,,drop = FALSE]
            if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
                data = data[data$target_spec_name == input$target_spec_name,,drop = FALSE]
            if (isTruthy(input$probe_name) && input$probe_name != "All")
                data = data[data$probe_name == input$probe_name,,drop = FALSE]
            if (isTruthy(input$duplicates) && input$duplicates != "All")
                data = data[data$duplicates == input$duplicates, , drop = FALSE]
            
            if (input$note_radio %in% c("neg_ref", "pos_ref", "benchmark")) {
                row_indices = min(as.numeric(row_indices))
                to_none = notes %in% input$note_radio
                if (sum(to_none) > 0) {
                    notes[to_none] = "none"
                }
            }
            
            sel = paste0(data$platename[row_indices], 
                         "|",
                        data$position[row_indices])
            notes[sel] = input$note_radio
            
            adam$notes = notes
            analysis_dataset(adam)
            
            hide_cols = c(1, 2, 5, 6, 11)
            output$adam_table = DT::renderDataTable(DT::datatable({
                data = adam
                data$probe_name = as.character(data$probe_name)
                data$target_spec_name = as.character(data$target_spec_name)
                if (isTruthy(input$platename) && input$platename != "All")
                    data = data[data$platename == input$platename, , drop = FALSE]
                if (isTruthy(input$plate_row) && input$plate_row != "All")
                    data = data[data$plate_row == input$plate_row, , drop = FALSE]
                if (isTruthy(input$plate_col) && input$plate_col != "All")
                    data = data[data$plate_col == input$plate_col, , drop = FALSE]
                if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
                    data = data[data$target_spec_name == input$target_spec_name, , drop = FALSE]
                if (isTruthy(input$probe_name) && input$probe_name != "All")
                    data = data[data$probe_name == input$probe_name, , drop = FALSE]
                if (isTruthy(input$duplicates) && input$duplicates != "All")
                    data = data[data$duplicates == input$duplicates, , drop = FALSE]
                data
            },
            selection = 'multiple',
            options = list(pageLength = 6,
                           dom = "tpf",
                           columnDefs = list(
                               list(className = 'dt-nowrap', targets = '_all'),
                               list(visible = FALSE, targets = hide_cols))
            )))
            
            output$notes_table = renderTable(
                notes_summary_func(adam$notes), 
                striped = TRUE,
                spacing = "xs")

        }
        
    }) |> bindEvent(input$annotate_smpl)
    observe({
        adam = analysis_dataset()
        data = adam
        data$probe_name = as.character(data$probe_name)
        data$target_spec_name = as.character(data$target_spec_name)
        if (isTruthy(input$platename) && input$platename != "All")
            data = data[data$platename == input$platename,,drop = FALSE]
        if (isTruthy(input$plate_row) && input$plate_row != "All")
            data = data[data$plate_row == input$plate_row,,drop = FALSE]
        if (isTruthy(input$plate_col) && input$plate_col != "All")
            data = data[data$plate_col == input$plate_col,,drop = FALSE]
        if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
            data = data[data$target_spec_name == input$target_spec_name,,drop = FALSE]
        if (isTruthy(input$probe_name) && input$probe_name != "All")
            data = data[data$probe_name == input$probe_name,,drop = FALSE]
        if (isTruthy(input$duplicates) && input$duplicates != "All")
            data = data[data$duplicates == input$duplicates, , drop = FALSE]
        
        hide_cols = c(1, 2, 5, 6, 11)
        output$adam_table = DT::renderDataTable(DT::datatable({
            data = adam
            data$probe_name = as.character(data$probe_name)
            data$target_spec_name = as.character(data$target_spec_name)
            if (isTruthy(input$platename) && input$platename != "All")
                data = data[data$platename == input$platename, , drop = FALSE]
            if (isTruthy(input$plate_row) && input$plate_row != "All")
                data = data[data$plate_row == input$plate_row, , drop = FALSE]
            if (isTruthy(input$plate_col) && input$plate_col != "All")
                data = data[data$plate_col == input$plate_col, , drop = FALSE]
            if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
                data = data[data$target_spec_name == input$target_spec_name, , drop = FALSE]
            if (isTruthy(input$probe_name) && input$probe_name != "All")
                data = data[data$probe_name == input$probe_name, , drop = FALSE]
            if (isTruthy(input$duplicates) && input$duplicates != "All")
                data = data[data$duplicates == input$duplicates, , drop = FALSE]
            data
        },
        selection = 'multiple',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'),
                           list(visible = FALSE, targets = hide_cols))
        )))
        
        
        row_indices = input$adam_table_rows_selected
        if (length(row_indices) == 0) {
            sample_names = NULL
        }else{
            sample_names = paste0(data$platename[row_indices], 
                                  "|",
                                  data$position[row_indices])
        }

        filename = paste0(app_dir, "www/img/adam-graph-home.pdf")
        
        if (input$plot_name=="" | is.null(input$plot_name)) {
            group_name = NULL
        }else{
            group_name = input$plot_name
        }

        adam_plot(adam, 
                  sample_names = sample_names, 
                  group_name = group_name, 
                  filename = filename, 
                  session_name = input_react_vals$session_name)

        removeUI("#adam_plot_div")
        
        insertUI(
            "#adam_plot_div_top",
            "afterEnd",
            ui = tags$div(
                id="adam_plot_div",
                
                tags$div(
                    class="row",
                    tags$iframe(
                        src = "img/adam-graph-home.pdf",
                        width = "400px",
                        height = "320px",
                    )
                ),
                tags$p(""),
                textInput(
                    "plot_name",
                    label = NULL,
                    placeholder = "(Optional) Enter Plot Name",
                    width="45%"
                )
            )
        )
        
    }) |> bindEvent(input$plot_table_selections)
    observe({
        if (isTruthy(input$go_back_to_notes)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
            bslib::toggle_sidebar(id="my_sidebar", open=FALSE)
        }
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "proceed_to_cell_conc",
                    "Go Back To Query Page",
                    class="wt-100"
                )
            )
        )
        
        adam = analysis_dataset()
        
        hide_cols = c(1, 2, 5, 6, 11)
        
        output$adam_table = DT::renderDataTable(DT::datatable({
            data = adam
            data$probe_name = as.character(data$probe_name)
            data$target_spec_name = as.character(data$target_spec_name)
            if (isTruthy(input$platename) && input$platename != "All")
                data = data[data$platename == input$platename, , drop = FALSE]
            if (isTruthy(input$plate_row) && input$plate_row != "All")
                data = data[data$plate_row == input$plate_row, , drop = FALSE]
            if (isTruthy(input$plate_col) && input$plate_col != "All")
                data = data[data$plate_col == input$plate_col, , drop = FALSE]
            if (isTruthy(input$target_spec_name) && input$target_spec_name != "All")
                data = data[data$target_spec_name == input$target_spec_name, , drop = FALSE]
            if (isTruthy(input$probe_name) && input$probe_name != "All")
                data = data[data$probe_name == input$probe_name, , drop = FALSE]
            if (isTruthy(input$duplicates) && input$duplicates != "All")
                data = data[data$duplicates == input$duplicates, , drop = FALSE]
            
            data
        },
        selection = 'multiple',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'),
                           list(visible = FALSE, targets = hide_cols))
        )))
        
        output$notes_table = renderTable(
            notes_summary_func(adam$notes), 
            striped = TRUE,
            spacing = "xs")
        
        adam_plot(adam, 
                  sample_names = NULL, 
                  group_name = NULL, 
                  filename = paste0(app_dir, 
                                    "www/img/adam-graph-home.pdf"), 
                  session_name = input_react_vals$session_name)
        
        filters = tags$div(
            id = "all_filters_div",
            fluidRow(
                selectInput("platename",
                            "platename",
                            c("All",
                              sort(unique(adam[["platename"]]))),
                            width="275px"),
                selectInput("plate_row",
                            "plate_row",
                            c("All",
                              sort(unique(adam[["plate_row"]]))),
                            width="275px"),
                selectInput("plate_col",
                            "plate_col",
                            c("All",
                              sort(unique(adam[["plate_col"]]))),
                            width="275px"),
                selectInput("target_spec_name",
                            "target_spec_name",
                            c("All",
                              sort(unique(as.character(adam[["target_spec_name"]])))),
                            width="275px"),
                selectInput("probe_name",
                            "probe_name",
                            c("All",
                              sort(as.character(unique(adam[["probe_name"]])))),
                            width="275px"),
                selectInput("duplicates",
                            "duplicates",
                            c("All",
                              sort(unique(adam[["duplicates"]]))),
                            width="275px")
            )
        )
        
        # column1
        insert_me1 = tags$div(
            tags$p(paste0("Analysis dataset -- sample size: ",
                          nrow(adam), 
                          " -- select rows to add notes"),
                   class="h5 text-primary fw-bold"),
            
            tags$div(
                id="table_filter_div_top",
            ),
            
            tags$div(
                id="table_filter_div",
                class = "col",
                filters,
                DT::DTOutput("adam_table"),
                tags$div(
                    class="row",
                    tags$div(
                        class="col",
                        downloadButton("adam_download_csv",
                                       "Download Analysis Data as CSV")
                    ),
                    tags$div(
                        class="col",
                        ""
                    )
                )
            )
            
        )
        
        insert_me2 = tags$div(
            id="annotation_pannel_div_top",
            
            tags$p("Annotation pannel",
                   class="h5 text-primary fw-bold"),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    radioButtons(
                        inputId = "note_radio",
                        label = "Label the selected probe(s)",
                        choices = list(
                            "None" = "none",
                            "Negative reference" = "neg_ref",
                            "Benchmark" = "benchmark",
                            "Drop" = "drop"
                        ),
                        inline = FALSE
                    )
                ),
                tags$div(
                    class="col",
                    tableOutput("notes_table"),
                )
            ),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    actionButton("annotate_smpl",
                                 "Note",
                                 class="btn-warning wt-50",
                                 width="100%")
                ),
                tags$div(
                    class="col",
                    actionButton("plot_table_selections",
                                 "Plot",
                                 class="btn-primary wt-50",
                                 width="100%")
                ),
                tags$div(
                    class="col",
                    actionButton("proceed_to_tlgs",
                                 "Proceed",
                                 class="btn-secondary",
                                 width="100%")
                )
            ),
            tags$p(""),
            
            tags$div(id="adam_plot_div_top"),
            
            tags$div(
                id="adam_plot_div",
                
                tags$div(
                    class="row",
                    tags$iframe(
                        src = "img/adam-graph-home.pdf",
                        width = "400px",
                        height = "320px",
                    )
                ),
                tags$p(""),
                textInput(
                    "plot_name",
                    label = NULL,
                    placeholder = "(Optional) Enter Plot Name",
                    width="45%"
                )
            ),
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
                        tags$div(id="main_contents_col2_1"),
                        insert_me2
                    )
                )
            )
        )
        
        
    }) |> bindEvent(input$go_back_to_notes)
    
    # 9. proceed to analysis
    res_table = reactiveVal()
    observe({
        if (isTruthy(input$proceed_to_tlgs)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#card_div")
        }
        
        insertUI(
            selector="#card_div_top",
            where = "afterEnd",
            ui = tags$div(
                id = "card_div",
                tags$p("Session name",
                       class="h6 text-primary fw-bold"),
                tags$p(input_react_vals$session_name,
                       class="h6 text-secondary"),
                tags$br(),
                actionButton(
                    "go_back_to_notes",
                    "Go Back To Notes",
                    class="wt-100"
                )
            )
        )

        insert_me1 = tags$div(
            id="select_analysis_type_top",
            
            tags$div(
                id = "select_analysis_type",
                
                tags$p("Analysis type",
                       class="h5 text-primary fw-bold"),
                
                radioButtons(
                    "analysis_type",
                    "Select the main type of analysis",
                    c("Probe x Conc"="Probe x Conc", 
                      "Probe x Cell Lines"="Probe x Cell Lines")
                ),
                
                actionButton(
                    "submit_analysis_type",
                    "Generate Table",
                    class="btn-warning",
                    width="50%"
                ),
                
                tags$div(
                    id = "results_download_div_top"
                ),
                
                tags$div(
                    id = "results_download_div"
                )
                
            )
        )
        
        insert_me2 = tags$div(
            tags$p("Preview table heatmap",
                   class="h5 text-secondary fw-bold"),
            
            tags$div(
                id = "heatmap_div_top"
            ),
            
            tags$div(
                class="row",
                id = "heatmap_div",
            )
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
                        tags$div(id="main_contents_col2_1"),
                        insert_me2
                    ),
                )
            )
        )
        
        

 
    }) |> bindEvent(input$proceed_to_tlgs)
    observe({
        adam = analysis_dataset()
        adam = adam[!adam$notes %in% "drop",,drop=F]
        
        cna = colnames(adam)
        cna = cna[grep("^conc", cna)]
        adam[,cna] = factor(adam[,cna])

        res = make_res_table(adam, input$analysis_type)
        
        res_table(res)
        
        filename = paste0(app_dir, "www/docs/tlgs/results-table-heatmap.pdf")
        pdf(filename, width=15, height=10.75)
        plot_res_table(res, input$analysis_type)
        dev.off()
        
        basic_heatmap = tags$div(
            style="height: 970px",
            tags$iframe(
                src = "docs/tlgs/results-table-heatmap.pdf#zoom=50",
                width="100%",
                height="60%"
            )
        )
        
        removeUI("#heatmap_div")
   
        insertUI(
            selector = "#heatmap_div_top",
            where    = "afterEnd",
            ui = tags$div(
                class="row",

                id = "heatmap_div",
                basic_heatmap
            )
        )
        
        removeUI("#results_download_div")
        
        insertUI(
            selector = "#results_download_div_top",
            where    = "afterEnd",
            ui = tags$div(
        
                id = "results_download_div",
                
                tags$p(""),
                tags$br(),
                tags$p("Results ready",
                       class="h5 text-secondary fw-bold"),
                
                downloadButton(
                    "res_table_download_csv",
                    "Download Results Table"
                )
            )
        )
        

    }) |> bindEvent(input$submit_analysis_type)
    
    
    
    #---------------------------- download handlers ---------------------------#
    # probe_dict_download_csv download handler
    output$probe_dict_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_probe-dict_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            write.csv(input_react_vals$probe_dict,
                      file, row.names = FALSE)
        }
    )
    
    # target_dict_download_csv download handler
    output$target_dict_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_target-dict_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            write.csv(input_react_vals$target_spec_dict,
                      file, row.names = FALSE)
        }
    )
    
    # probe_order_download_csv download handler
    output$probe_order_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_probe-order_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            write.csv(tlgs_react_vals$probe_order_table,
                      file, row.names = FALSE)
        }
    )
    
    # target_order_download_csv download handler
    output$target_order_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_target-order_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            write.csv(target_order_vals$target_dict_order,
                      file, row.names = FALSE)
        }
    )
    
    # probe_query_download_csv download handler
    output$probe_query_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_probe-query_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            probe_df = probe_query_react_value()
            write.csv(probe_df[,"probe_name",drop=FALSE], 
                      file, row.names = FALSE)
        }
    )
    
    #  probe_order_query_download_csv download handler
    output$probe_order_query_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_probe-order_", 
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            probe_order = probe_order_react_val()
            write.csv(probe_order, file, row.names = FALSE)
        }
    )
    
    # adam download
    output$adam_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_analysis-dataset_",
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            adam = analysis_dataset()
            write.csv(adam[,-c(1, 2),drop=FALSE],
                      file, row.names = FALSE)
        }
    )
    
    # res_table_download_csv download
    output$res_table_download_csv = downloadHandler(
        filename = function() {
            paste0(Sys.Date(),
                   "_result-table_",
                   input_react_vals$session_name,
                   ".csv")
        },
        content = function(file) {
            res = res_table()
            write.csv(res, file, row.names = FALSE)
        }
    )
    

}
