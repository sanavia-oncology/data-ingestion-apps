# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    # get app directory
    app_dir = paste0(getwd(), "/")
    
    # set path to fixed data source
    dotenv::load_dot_env("~/.env_data_ingestion_apps")
    
    # make folders in TLGS_DIR
    tlgs_path = Sys.getenv("TLGS_DIR")
    stamp = Sys.Date()
    tlgs_path_tmp = paste0(tlgs_path, "/", stamp, "/")
    
    if (!dir.exists(tlgs_path_tmp)) {
        dir.create(tlgs_path_tmp)
    }
    
    tlgs_path_tmp_dict = paste0(tlgs_path_tmp, "dict/")
    if (!dir.exists(tlgs_path_tmp_dict)) {
        dir.create(tlgs_path_tmp_dict)
    }
    
    tlgs_path_tmp_data = paste0(tlgs_path_tmp, "data/")
    if (!dir.exists(tlgs_path_tmp_data)) {
        dir.create(tlgs_path_tmp_data)
    }
    
    tlgs_path_tmp_tlgs = paste0(tlgs_path_tmp, "tlgs/")
    if (!dir.exists(tlgs_path_tmp_tlgs)) {
        dir.create(tlgs_path_tmp_tlgs)
    }
    
    # paths to results csvs
    folder_path = Sys.getenv("DATA_DIR")
    res_list = tryCatch(
        results_summary_list(folder_path),
        error = function(e) {
            error_msg = "'results_summary_list()' an error occurred (K.Okrah)"
            return(error_msg)
        }
    )
    
    # make front page table
    input_react_vals = reactiveValues()
    observe({
        table_front_page = tryCatch(
            res_list[["front_page_table"]],
            error = function(e) {
                error_msg = "'results_summary_list()' an error occurred (K.Okrah)"
                return(error_msg)
            }
        )
        
        input_react_vals$table_front_page = table_front_page
        
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
            tags$p("Select a Project (or Multiple Projects)",
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
            removeUI(selector = "#load_project")
            bslib::toggle_sidebar(id="my_sidebar", open=FALSE)
        }
        
        # filter to match front table
        table_front_page = input_react_vals$table_front_page
        
        sel = rep(TRUE, nrow(table_front_page))
        if (input$dose_type != "All") {
            sel = sel & table_front_page[["dose_type"]] == input$dose_type
        }
        if (input$proj_group != "All") {
            sel = sel & table_front_page[["project_group"]] == input$proj_group
        }
        table_front_page_sub = table_front_page[sel,]
        
        # get selected projects
        sel_projs = input$projects_table_rows_selected
        selected_project_paths = table_front_page_sub[["full_path"]][sel_projs]
        selected_proj_group = table_front_page_sub[["proj_group"]][sel_projs]
        selected_proj_name = table_front_page_sub[["project_name"]][sel_projs]
        
        # load data
        tmp = list()
        for (k in 1:length(selected_project_paths)) {
            tmp_dat = read.csv(selected_project_paths[k], 
                               header = TRUE, 
                               check.names = FALSE)
            tmp_dat$project_group = selected_proj_group[k]
            tmp_dat$project_name = selected_proj_name[k]
            tmp[[k]] = tmp_dat
        }
        dat = do.call(rbind, tmp)
        rownames(dat) = NULL
        
        # make dicts
        probe_id = sort(unique(dat[["Probe ID"]]))
        target_spec_id = sort(unique(dat[["Target Spec ID"]]))
        
        probe_dict = data.frame(probe_id = probe_id,
                                has_alias = "No",
                                probe_alias = "no_alias")
        
        target_spec_dict = data.frame(target_spec_id = target_spec_id,
                                      has_alias = "No",
                                      target_spec_alias = "no_alias")
        
        # update probe_dict
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
        
        # update target_dict
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
        
        # write dicts
        write.csv(probe_dict,
                  file=paste0(tlgs_path_tmp_dict, stamp, 
                              "_probe_dict.csv"),
                  row.names=FALSE)
        
        write.csv(target_spec_dict,
                  file=paste0(tlgs_path_tmp_dict, stamp, 
                              "_target_spec_dict.csv"),
                  row.names=FALSE)
        
        # probe dict table
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
                                 disabled=TRUE)
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
    
    # update probe_ids button
    observe({
        if (!is.null(input$file1)) {
            updateActionButton(session, "add_probe_alias", disabled=FALSE) 
        }
    }) |> bindEvent(input$file1)
    observe({
        probe_id_dict = read.csv(input$file1$datapath, header = TRUE)
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
                    tags$p("Probe ID Alias Table",
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
                               'probe_alias' columns (K.Okrah)."),
                type = "error",
                duration = NULL
            )
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
                                 disabled=TRUE)
                ),
                tags$div(
                    class="col",
                    id = "proceed_to_review",
                    actionButton("proceed_to_review",
                                 "Review and Annotate",
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
    
    # update target_ids button
    observe({
        if (!is.null(input$file2)) {
            updateActionButton(session, "add_target_alias", disabled=FALSE) 
        }
    }) |> bindEvent(input$file2)
    observe({
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
                        id = "proceed_to_review",
                        actionButton("proceed_to_review",
                                     "Review and Annotate",
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
    }) |> bindEvent(input$add_target_alias)
    
    # review and annotate
    annot_react_vals = reactiveValues()
    observe({
        if (isTruthy(input$proceed_to_review)) {
            removeUI(selector = "#main_contents1")
        }
        
        dat = input_react_vals$dat
        target_spec_dict = input_react_vals$target_spec_dict
        probe_dict = input_react_vals$probe_dict
        
        cleaned_dat = clean_dat(dat, probe_dict, target_spec_dict)
        input_react_vals$cleaned_dat = cleaned_dat
        
        scol = c("Platename",
                 "Plate Position",
                 "dose_type",
                 "target_spec_name",
                 "probe_name",
                 "Probe Quant Value",
                 "gMFI")
        
        cleaned_dat_sub = cleaned_dat[,scol,drop=F]
        colnames(cleaned_dat_sub) =  c("platename",
                                       "position",
                                       "dose_type",
                                       "target_spec_name",
                                       "probe_name",
                                       "conc (ug/ml)",
                                       "mfi")
        
        cleaned_dat_sub$mfi = round(cleaned_dat_sub$mfi)
        plate_col = as.numeric(gsub("[A-Z]", "", cleaned_dat_sub[["position"]]))
        plate_row = substr(cleaned_dat_sub[["position"]], 1, 1)
        cleaned_dat_sub = cbind(plate_row=plate_row,
                                plate_col=plate_col,
                                cleaned_dat_sub)
        input_react_vals$cleaned_dat_sub = cleaned_dat_sub
        results_table = cleaned_dat_sub
        
        # initialize annotation container
        annot_react_vals$annot_vector = c()
        annot_react_vals$display_table = results_table
        
        # column1
        insert_me1 = tags$div(
            tags$p("Select rows and add notes",
                   class="h5 text-primary fw-bold"),
            tags$p(paste0("Table size: ", nrow(cleaned_dat_sub), " rows"),
                   class="h6 text-secondary"),
            tags$div(
                id="table_filter_div_top",
            ),
            tags$div(
                class = "col",
                id = "table_page_col1",
                fluidRow(
                    selectInput("platename",
                                "platename",
                                c("All",
                                  sort(unique(results_table[["platename"]])))),
                    selectInput("plate_row",
                                "plate_row",
                                c("All",
                                  sort(unique(results_table[["plate_row"]])))),
                    selectInput("plate_col",
                                "plate_col",
                                c("All",
                                  sort(unique(results_table[["plate_col"]])))),
                    selectInput("target_spec_name",
                                "target_spec_name",
                                c("All",
                                  sort(unique(results_table[["target_spec_name"]])))),
                    selectInput("probe_name",
                                "probe_name",
                                c("All",
                                  sort(unique(results_table[["probe_name"]])))),
                    selectInput("conc",
                                "conc (ug/ml)",
                                c("All",
                                  sort(unique(results_table[["conc (ug/ml)"]]))))
                ),
                DT::DTOutput("table")
            )
        )
        
        notes = rep("None", nrow(results_table))
        Note = factor(notes, levels = c("None",
                                        "Negative Reference",
                                        "Positive Reference",
                                        "Benchmark",
                                        "Drop from TLGs"))
        
        output$samples_noted_ui = renderUI({
            tbl = as.data.frame(table(Note))
            tags$pre(paste0("Samples noted:\n",
                            paste(capture.output(print(tbl, row.names = FALSE)),
                                  collapse = "\n")))
        })
        
        insert_me2 = tags$div(
            tags$p("Annotation pannel",
                   class="h5 text-primary fw-bold"),
            fluidRow(
                radioButtons(
                    inputId = "note_radio",
                    label = "Label the selected probe(s)",
                    choices = list(
                        "None" = "None",
                        "Negative Reference" = "Negative Reference",
                        "Positive Reference" = "Positive Reference",
                        "Benchmark" = "Benchmark",
                        "Drop from TLGs" = "Drop from TLGs"
                    )
                )
            ),
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    actionButton("annotate_smpl",
                                 "Submit notes",
                                 class="btn-warning",
                                 width="100%")
                ),
                tags$div(
                    class="col",
                    actionButton("proceed_to_analysis",
                                 "Proceed",
                                 class="btn-secondary",
                                 width="100%")
                )
            ),
            tags$br(),
            tags$br(),
            tags$div(
                id="note_div_top",
            ),
            tags$div(
                id="note_div",
                uiOutput("samples_noted_ui")
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
                        tags$div(id = "main_contents_col2_1"),
                        insert_me2
                    )
                )
            )
        )
        
    }) |> bindEvent(input$proceed_to_review)
    
    filtered_table = reactive({
        data = annot_react_vals$display_table
        req(data)
        
        # add annotation status column
        av = annot_react_vals$annot_vector
        note = rep("None", nrow(data))
        names(note) = rownames(data)
        if (length(av) > 0) {
            shared = intersect(names(av), names(note))
            note[shared] = av[shared]
        }
        data$"annotation_column" = note
        
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
        if (isTruthy(input$conc) && input$conc != "All")
            data = data[data$"conc (ug/ml)" == input$conc, , drop = FALSE]
        data
    })
    output$table = DT::renderDT({
        dt = filtered_table()
        DT::datatable(dt,
                      rownames = TRUE,
                      options = list(pageLength = 7,
                                     dom = "tpf",
                                     columnDefs = list(
                                         list(visible = FALSE, targets = c(1, 2)),
                                         list(className = 'dt-nowrap', targets = '_all'))
                      ))
    }, server = FALSE)
    observe({
        full_table = input_react_vals$cleaned_dat_sub
        annot_vector = annot_react_vals$annot_vector
        
        sel_row_ind = input$table_rows_selected
        k = rownames(filtered_table())[sel_row_ind]
        
        notes = rep("None", nrow(full_table))
        names(notes) = rownames(full_table)
        
        if (input$note_radio != "None") {
            annot_vector[k] = input$note_radio
            if (!input$note_radio %in% "Drop from TLGs") {
                annot_vector[annot_vector %in% input$note_radio] = "None"
                annot_vector[k[1]] = input$note_radio
            }
        } else {
            annot_vector = annot_vector[names(annot_vector) != k]
        }
        
        annot_react_vals$annot_vector = annot_vector
        notes[names(annot_vector)] = annot_vector
        
        Note = factor(notes, levels = c("None",
                                        "Negative Reference",
                                        "Positive Reference",
                                        "Benchmark",
                                        "Drop from TLGs"))
        output$samples_noted_ui = renderUI({
            tbl = as.data.frame(table(Note))
            tags$pre(paste0("Samples noted:\n",
                            paste(capture.output(print(tbl, row.names = FALSE)),
                                  collapse = "\n")))
        })
        
    }) |> bindEvent(input$annotate_smpl)
    
    # proceed to analysis
    target_order_vals = reactiveValues()
    observe({
        if (isTruthy(input$proceed_to_analysis)) {
            removeUI(selector = "#main_contents1")
        }
        
        display_table = annot_react_vals$display_table
        annot_vector = annot_react_vals$annot_vector
        annot_vector_ = rep("None", nrow(display_table))
        
        names(annot_vector_) = as.character(1:length(annot_vector_))
        if (length(annot_vector) > 0) {
            annot_vector_[as.character(names(annot_vector))] = annot_vector
        }
        
        dat = input_react_vals$dat
        display_table$annotation = annot_vector_
        
        project_data = cbind(dat, display_table)
        
        write.csv(project_data,
                  file=paste0(tlgs_path_tmp_data, stamp, 
                              "_project_data.csv"),
                  row.names=FALSE)
        
        input_react_vals$project_data = project_data
        
        target_dict_download = input_react_vals$target_spec_dict
        target_dict_download$target_spec_name = target_dict_download$target_spec_alias
        target_dict_download$order = 1:nrow(target_dict_download)
        
        target_dict_download$target_spec_alias = NULL
        target_dict_download$target_spec_id = NULL
        target_dict_download$has_alias = NULL
        
        target_order_vals$target_dict_order = target_dict_download
        
        write.csv(target_dict_download,
                  file=paste0(tlgs_path_tmp_data, stamp,
                              "_target_spec_order.csv"),
                  row.names=FALSE)
        
        output$target_dict_download1 = DT::renderDataTable(DT::datatable({
            data = target_dict_download
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
                    id = "ref_probe_id_div2",
                    tags$p("Order Cell Lines",
                           class="h5 text-primary fw-bold")
                )
            ),
            
            tags$div(
                class="row",
                tags$p("Oder in which cell lines will be dispalyed"),
                tags$div(
                    class="col",
                    id = "add_target_alias",
                fileInput("targert_order_file", 
                          "Choose CSV File",
                          accept=".csv")
                ),
                tags$div(
                    class="col",
                    tags$p("")
                )
            ),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    id = "proceed_to_review",
                    actionButton("order_targets",
                                 "Update Target Spec ID Order",
                                 class="btn-warning",
                                 disabled = TRUE)
                ),
                tags$div(
                    class="col",
                    id = "add_target_alias",
                    tags$p("")
                )
            ),
            tags$br(),
            tags$div(
                class="row",
                tags$br(),
                tags$p("Current oder",
                       class="h5 text-secondary"),
                tags$div(
                    id="target_dict_download1_div_top",
                ),
                tags$div(
                    id="target_dict_download1_div",
                    DT::dataTableOutput("target_dict_download1")
                )
            )
        )

        insert_me2 = tags$div(
            id = "ref_probe_id_div",
            tags$div(
                class="row",
                tags$div(
                    id = "ref_probe_id_div2",
                    tags$p("Select Analysis Type",
                           class="h5 text-primary fw-bold")
                )
            ),
            
            tags$div(
                class="row",
                tags$p("Analysis type"),
                tags$div(
                    class="col",
                    id = "add_target_alias",
                    selectInput(
                        "analysis_type",
                        "Select Analysis Type",
                        c("None",
                          "Probe x Cell Lines",
                          "Probe x Conc")
                    )
                ),
                tags$div(
                    class="col",
                    tags$p("")
                )
            ),
            
            tags$div(
                class="row",
                tags$div(
                    class="col",
                    id = "proceed_to_TLGs",
                    actionButton("proceed_to_TLGs",
                                 "Generate to TLGs",
                                 class="btn-secondary",
                                 disabled = TRUE)
                ),
                tags$div(
                    class="col",
                    id = "add_target_alias",
                    tags$p("")
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

    }) |> bindEvent(input$proceed_to_analysis)

    # order_targets button
    observe({
        if(!is.null(input$targert_order_file)) {
            updateActionButton(session, "order_targets", disabled=FALSE) 
        }
    }) |> bindEvent(input$targert_order_file)
    observe({
        removeUI(selector = "#target_dict_download1_div")
        
        target_order = read.csv(input$targert_order_file$datapath,
                                header = T, check.names = F)
        
        o = order(target_order[,"order"])
        target_order = target_order[o,]
        rownames(target_order) = NULL
        
        target_order_vals$target_dict_order = target_order
        
        output$target_dict_download2 = DT::renderDataTable(DT::datatable({
            data = target_order
            data
        },
        selection = 'none',
        options = list(pageLength = 6,
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        insertUI(
            selector = "#target_dict_download1_div_top",
            where = "afterEnd",
            ui = tags$div(
                id="target_dict_download1_div",
                DT::dataTableOutput("target_dict_download2")
            )
        )
        
    }) |> bindEvent(input$order_targets)
    
    # process data and generate the PDFs first
    tlgs_react_vals = reactiveValues()
    observe({
        if (!is.null(input$analysis_type)) {
            if (input$analysis_type != "None") {
                updateActionButton(session, "proceed_to_TLGs", disabled=FALSE)    
            }else{
                updateActionButton(session, "proceed_to_TLGs", disabled=TRUE) 
            }
        }
    }) |> bindEvent(input$analysis_type)
    pdf_generation_status = eventReactive(input$proceed_to_TLGs, {
        project_data       = input_react_vals$project_data
        target_dict_order  = target_order_vals$target_dict_order
        
        print(target_dict_order)
        print(head(project_data))
        
        target_spec_levels = target_dict_order$target_spec_name
        
        to_drop      = project_data[, "annotation"] %in% "Drop from TLGs"
        project_data = project_data[!to_drop, , drop = FALSE]
        
        keep               = target_spec_levels %in% project_data[, "target_spec_name"]
        target_spec_levels = target_spec_levels[keep]
        
        project_data[, "target_spec_name"] = factor(
            as.character(project_data[, "target_spec_name"]),
            levels = target_spec_levels
        )
        
        project_data[, "conc (ug/ml)"] = factor(
            as.numeric(project_data[, "conc (ug/ml)"])
        )
        
        project_data[, "Probe Quant Value"] = project_data[, "conc (ug/ml)"]
        
        pa = get_probe_annotation(project_data)
        
        # generate PDFs synchronously 
        fig1_path = paste0(tlgs_path_tmp_tlgs, 
                           stamp,
                           "_01-fig_basic_heatmap.pdf")
        pdf(fig1_path, width = 8.25, height = 11)
        
        if (input$analysis_type == "Probe x Cell Lines") {
            res_s = prep_single_dose_data(project_data)
            tab = single_dose_tlgs(res_s, pa=pa)
        }
        
        if (input$analysis_type == "Probe x Conc") {
            res_t = prep_multi_dose_data(project_data)
            tab = titration_tlgs(res_t, pa=pa)
        }
        
        dev.off()

        tlgs_react_vals$project_data = project_data
        
        fig1_copy = paste0(app_dir, "www/docs/tlgs/01-fig_basic_heatmap.pdf")
        sys_call = paste0("cp ",  fig1_path, " ", fig1_copy)
        system(sys_call)
        
        tab[,"annotation"] = pa[tab[,"probe_name"]]
    
        tlgs_react_vals$tab = tab
        
        write.csv(tab,
                  file = paste0(tlgs_path_tmp_tlgs, 
                                stamp, "_01-tab_basic_table.csv"),
                  row.names = F)
   
        # return a success flag when everything above is finished
        return(TRUE)
    })
    
    # insert the UI button ONLY after the PDF step returns TRUE
    observeEvent(pdf_generation_status(), {
        # This block will not run until pdf_generation_status() evaluates and completes
        removeUI(selector="#view_tlgs_div")
        
        insertUI(
            selector = "#proceed_to_TLGs",
            where = "afterEnd",
            ui = tags$div(
                id="view_tlgs_div",
                class = "row",
                tags$p(""),
                tags$br(),
                tags$p("TLGs ready!"),
                tags$div(
                    class="row",
                    # tags$div(
                    #     class="col",
                    #     actionButton(
                    #         "view_tlgs",
                    #         "View TLGs",
                    #         class = "btn-primary",
                    #         width="100%"
                    #     )
                    # ),
                    tags$div(
                        class="col",
                        tags$p("")
                    )
                )
            )
        )
    })
    
    # # view tlgs page
    # observe({
    #     if (isTruthy(input$proceed_to_TLGs)) {
    #         removeUI(selector = "#main_contents1")
    #     }
    #     
    #     insert_me1 = tags$div(
    #         id = "ref_probe_id_div",
    #         tags$div(
    #             class="row",
    #             tags$div(
    #                 id = "ref_probe_id_div2",
    #                 tags$p(paste0(input$analysis_type),
    #                        class="h5 text-primary fw-bold")
    #             )
    #         ),
    #         "Done"
    #     )
    #     
    #     # basic_heatmap = tags$div(
    #     #     style="height: 970px",
    #     #     tags$iframe(
    #     #         src = "docs/tlgs/01-fig_basic_heatmap.pdf#zoom=50",
    #     #         width="80%",
    #     #         height="60%"
    #     #     )
    #     # )
    #     # 
    #     # insert_me2 = tags$div(
    #     #     class="row",
    #     #     id = "ref_probe_id_div",
    #     #     tags$div(
    #     #         basic_heatmap,
    #     #     ),
    #     # )
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
    #                     "insert_me2"
    #                 )
    #             )
    #         )
    #     )
    #     
    # }) |> bindEvent(input$view_tlgs)
}
