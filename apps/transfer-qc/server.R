# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    # step 1: get data file paths
    folder_path = "/Users/kwameokrah/data_depo"
    
    proj_paths = tryCatch(
        get_paths_by_project(folder_path),
        error = function(e) {
            error_msg = "'get_paths_by_project()' an error occurred (K.Okrah)"
            return(error_msg)
        }
    )
    
    
    # step 2: make front table
    input_files = reactiveValues()
    observe({
        if (isTruthy(input$selected_assay)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#selected_project_div")
        }

        #---------------------- welcome note
        if (input$selected_assay=="") {
            insert_me1 = tags$p("Welcome, please select your assay type.", 
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
                    output$projects_table = DT::renderDataTable(DT::datatable({
                        data = table_front_page
                        if (input$has_qcr != "All") {
                            data = data[data[["Has QC Report"]] == input$has_qcr,]
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
                    
                    # table insert
                    insert_me1 = tags$div(
                        tags$p("Select A Project", 
                               class="h3 text-primary fw-bold text-center"),
                        tags$p("Click on a row to select project and proceed to QC",
                               class="h6 text-secondary text-center"),
                        fluidRow(
                            selectInput("proj_group",
                                        "Project Group",
                                        c("All", sort(unique(table_front_page[["Project Group"]])))),
                            selectInput("has_qcr",
                                        "Has QC Report",
                                        c("No", "Yes", "All"))
                        ),
                        DT::dataTableOutput("projects_table")
                    )
                    
                    # update input_files
                    input_files$table_front_page = table_front_page
                    
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

            # table insert
            insert_me1 = tags$div(
                tags$p(paste0("(", input$selected_assay, ") coming soon!"),
                       class="h6 text-secondary")
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
    
    
    # step 3: select a project
    observe({
        if (isTruthy(input$projects_table_rows_selected)) {
            removeUI(selector = "#selected_project_div")
        }
        
        k = input$projects_table_rows_selected
        table_front_page = input_files$table_front_page
        
        sel = rep(TRUE, nrow(table_front_page))
        if (input$has_qcr!="All") {
            sel = sel & table_front_page[["Has QC Report"]] %in% input$has_qcr
        }
        if (input$proj_group!="All") {
            sel = sel & table_front_page[["Project Group"]] %in% input$proj_group
        }
        
        table_front_page_sub = table_front_page[sel,,drop=F]
        project_display = table_front_page_sub[k, "Project Name"]

        #---------------------- insert UI
        insertUI(
            selector = "#selected_assay_div",
            where    = "afterEnd",
            ui = tags$div(
                id = "selected_project_div",
                card(
                    card_image("www/img/data_loaded1.jpg"),
                    card_body(tags$p(
                        project_display
                    )),
                    actionButton(
                        inputId = "perform_checks",
                        label = "Perform QC",
                        class = "btn-primary")
                )
            )
        )
        
        # update input_files
        input_files$selected_project = project_display
        
    }) |> bindEvent(input$projects_table_rows_selected)
    
    
    # step 4: perform qc
    err_react_vals = reactiveValues()
    observe({
        if (isTruthy(input$perform_checks)) {
            removeUI(selector = "#main_contents1")
        }
        
        #------------------------- load pinfo_sheets & assay_data
        selected_project = input_files$selected_project
        selected_assay = input$selected_assay
 
        file_paths = proj_paths[[selected_project]]
        pinfo_csv_paths = file_paths[["pinfos"]]
        assay_file_paths = file_paths[["assay_data"]]
        
        pinfo_sheets = read_pinfo_csvs(pinfo_csv_paths)
        assay_data = read_assay_data(assay_file_paths, selected_assay)

        #------------------------- perform plate information checks
        res = perform_plateinfo_checks(pinfo_sheets)
        checks_summary = res[["checks_summary"]]
        checks_detailed = res[["checks_detailed"]]
        rm("res")

        key = paste0("PLATE_", sprintf(1:ncol(checks_summary), fmt="%02d"))
        names(key) = colnames(checks_summary)

        names(pinfo_sheets) = key[names(pinfo_sheets)]
        colnames(checks_summary) = key[colnames(checks_summary)]
        names(checks_detailed) = key[names(checks_detailed)]

        #------------------------- merge assay_data and pinfo_sheets
        res = merge_assay_data(assay_data, pinfo_sheets)
        merge_info = res[["merge_info"]]
        assay_position_check_summary = res[["assay_position_check_summary"]]
        assay_position_check_detailed = res[["assay_position_check_detailed"]]
        rm("res")  
        
        mcheck = sapply(merge_info, function(x) all(x$merge_checks))
        mcheck = ifelse(mcheck, "Pass", "Fail")
        
        checks_summary = rbind(checks_summary, "DATA MERGE"=mcheck)
        
        
        #------------------------- prep. qc table
        QC_CODE = c("QC_00"="Required Columns",
                    "QC_01"="Platename",
                    "QC_02"="Plate Position",
                    "QC_03"="Creator",
                    "QC_04"="Target Spec Type",
                    "QC_05"="Target Spec ID",
                    "QC_06"="Probe Type",
                    "QC_07"="Probe ID",
                    "QC_08"="Probe Quant Type",
                    "QC_09"="Probe Quant Value",
                    "QC_10"="Primary Role",
                    "QC_11"="Subrole",
                    "QC_12"="DATA MERGE")

        qc_code = tags$table(
            style = paste0("color: #343637ff; font-size: 13px;",
                           " border: 1px solid #a7a9aaff; width: 530px;"),
            tags$tbody(
                tags$tr(
                    tags$td("QC_01: Platename"),
                    tags$td("QC_05: Target Spec ID"),
                    tags$td("QC_09: Probe Quant Value")
                ),
                tags$tr(
                    tags$td("QC_02: Plate Position"),
                    tags$td("QC_06: Probe Type"),
                    tags$td("QC_10: Primary Role")
                ),
                tags$tr(
                    tags$td("QC_03: Creator"),
                    tags$td("QC_07: Probe ID"),
                    tags$td("QC_11: Subrole")
                ),
                tags$tr(
                    tags$td("QC_04: Target Spec Type"),
                    tags$td("QC_08: Probe Quant Type"),
                    tags$td("QC_12: DATA MERGE")
                )
            )
        )
        
        #------------------------- check errors conditions
        checks_summary1 = checks_summary[1,,drop=F]
        checks_summary2 = checks_summary[2:nrow(checks_summary),,drop=F]
        
        cond1 = !all(checks_summary1[1,] == "Pass")
        cond2a = !all(checks_summary2 == "Pass")
        cond2b = all(checks_summary2 == "Pass")
        
        # make inserts depending on error conditions
        if (cond1) {
            main_tp = tags$p(
                class = "h6 card-title text-danger",
                style = "margin-bottom:2px;",
                "Diabolical Error!")
            tp = tags$p("Please make sure all required columns exist; 
                         check for potential spelling mistakes
                         including extra spaces.")
            bn = NULL
            img_src = "img/fun/angry-sana-cat1.png"
            
            print(checks_summary1)
            insert_me1 = tags$div(
                tags$p("Required Columns Check", 
                       class="h5 text-primary fw-bold"),
                tags$p(
                    tags$strong("Click"),
                    "
                            each failed cell to get a detailed error description message.
                            ",
                    class="text-secondary"),
                tags$p("QC_00: Required Columns"),
                qc_table(checks_summary1),
                tags$br(),
                success_horiz_card(main_tp, tp, img_src, bn) 
            )
            
        } else {
            
            if (cond2a) {
                msg = paste0(round(mean(checks_summary2 == "Pass") * 100), 
                             "% Success Rate")
                
                main_tp = tags$p(
                    class = "h6 card-title text-danger",
                    style = "margin-bottom:2px;",
                    msg)
                tp = tags$p("Almost there you can do it! Please review and 
                             fix each error.")
                bn = NULL
                img_src = "img/fun/waiting-sana-cat1.png"
                
                insert_me1 = tags$div(
                    tags$p("Syntax Quality Control Checks", 
                           class="h5 text-primary fw-bold"),
                    tags$p(
                        tags$strong("Click"),
                        "
                                    each failed cell to get a detailed error 
                                    description message.
                                    ",
                        class="text-secondary"),
                    card(qc_table(checks_summary2)),
                    "QC Code",
                    qc_code,
                    tags$br(),
                    success_horiz_card(main_tp, tp, img_src, bn),
                )
                
            }
            
            if (cond2b) {
                main_tp = tags$p(
                    class = "h3 card-title text-primary",
                    style = "margin-bottom:2px;",
                    "100% success!")
                img_src = "img/fun/happy-sana-cat1.png"
                
                if (input$selected_assay == "fcs") {
                    tp = tags$p(id = "success_msg",
                                "Select an MFI channel and specify the 
                                 experiment type to continue.")
                    bn = NULL
                } else {
                    tp = tags$p(id = "success_msg",
                                "Review QC report carefully to ensure that there
                                are logical/semantic errors. This is critical for 
                                downstream data analysis.")
                    bn = actionButton("qc_report",
                                      "Go to Semantic QC Report",
                                      class="btn-primary")
                }
                
                insert_me1 = tags$div(
                    tags$p("Syntax Quality Control Checks",
                           class="h5 text-primary fw-bold"),
                    tags$p(
                        "
                                    No syntax errors found.
                                    ",
                        class="text-secondary"),
                    card(qc_table(checks_summary2)),
                    "QC Code",
                    qc_code,
                    tags$br(),
                    success_horiz_card(main_tp, tp, img_src, bn),
                )
            }
        }
        
        if (cond2b && input$selected_assay=="fcs") {
            
            # MFI selection
            PAR_STRING_L = strsplit(assay_data$PAR_STRING, ";")
            
            uchannels = unique(unlist(PAR_STRING_L))
            uchannels = uchannels[grep("^B|^R|^Y|^V", uchannels)]
        
            o = order(sapply(strsplit(uchannels, "-"), "[[", 2)!="H",
                      sapply(strsplit(uchannels, "-"), "[[", 1))
            
            uchannels = uchannels[o]
            
            ch_check_mat = t(sapply(PAR_STRING_L, function(x) uchannels %in% x))
            dim(ch_check_mat)
            ch_check_mat = ch_check_mat + 0
            
            colnames(ch_check_mat) = uchannels
            
            # update input_files
            input_files$ch_check_mat = ch_check_mat
            input_files$pinfo_sheets = pinfo_sheets
            input_files$assay_data = assay_data
            
            insert_me2 = tags$div(
                id = "mfi_ch_box_div",
                
                tags$p("Select MFI channel",
                       class="h5 text-primary fw-bold"),
                tags$div(id = "main_contents_col2_div"),
                tags$div(
                    id = "select_mfi_ch_div",
                    tags$p("Select the channel used to compute MFI.",
                           class="text-secondary"),
                    selectizeInput(
                        "select_mfi_ch",
                        "Select MFI channel",
                        choices = uchannels,
                        selected = character(0),
                        options = list(
                            placeholder = 'Opened channels',
                            onInitialize = I('function() { this.setValue(""); }')
                        )
                    )
                )
            )
        } else {
            insert_me2 = tags$div(
                id = "msg_box_div",
                
                tags$p("Message Box",
                       class="h5 text-secondary fw-bold"),
                tags$div(
                    id = "main_contents_col2_div",
                ),
                tags$div(
                    id = "detailed_message_box",
                    tags$p("None", class="text-secondary"),
                    card(
                        verbatimTextOutput("mesage_box", placeholder = TRUE)
                    )
                )
            )
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
        
        output$mesage_box = renderPrint({
            cat("Message here...")
        })
        
        rev_key = names(key)
        names(rev_key) = key
        
        
        path2_proj = sapply(strsplit(pinfo_csv_paths[1], 
                                     "plate_information_sheets"), 
                            "[[", 1)
        err_react_vals$path2_proj = path2_proj
        
        data_list = list(checks_detailed = checks_detailed,
                         merge_info = merge_info,
                         pinfo_sheets = pinfo_sheets,
                         assay_position_check_summary = assay_position_check_summary,
                         assay_position_check_detailed = assay_position_check_detailed,
                         rev_key = rev_key,
                         QC_CODE = QC_CODE)
        
        err_react_vals$data_list = data_list
        
    }) |> bindEvent(input$perform_checks)
    
    
    # step 5: message box
    observe({
        if (isTruthy(input$last_cell_clicked)) {
            removeUI(selector = "#detailed_message_box")
        }
        
        data_list = err_react_vals$data_list
        checks_detailed = data_list[["checks_detailed"]]
        assay_position_check_summary = data_list[["assay_position_check_summary"]]
        assay_position_check_detailed = data_list[["assay_position_check_detailed"]]
        merge_info = data_list[["merge_info"]]
        rev_key = data_list[["rev_key"]]
        QC_CODE = data_list[["QC_CODE"]]
        
        lc = input$last_cell_clicked
        lc_list = strsplit(lc, "-")
        pl_nam = lc_list[[1]][1]
        err_code_ = lc_list[[1]][2]
        err_code = QC_CODE[err_code_]
        
        selected_cell_msg = paste0(pl_nam, " | ", err_code_, ": ", err_code)
        
        if (err_code == "DATA MERGE") {
            
            output$mesage_box = renderPrint({
                cat("Filename:", rev_key[pl_nam], "\n\n")
                merge_info[[pl_nam]][["merge_checks"]]
                print_error_msg(plate_nam = pl_nam, 
                                pinfo_col = err_code,
                                merge_info = merge_info,
                                assay_position_check_summary = assay_position_check_summary,
                                assay_position_check_detailed = assay_position_check_detailed)
            })
            
        }else{
            
            output$mesage_box = renderPrint({
                cat("Filename:", rev_key[pl_nam], "\n\n")
                print_error_msg(plate_nam = pl_nam, 
                                pinfo_col = err_code,
                                checks_detailed = checks_detailed)
            })
            
        }
        
        insertUI(
            selector = "#main_contents_col2_div", 
            where = "afterEnd",
            
            ui = tags$div(
                id = "detailed_message_box",
                tags$p(
                    selected_cell_msg, 
                    class="text-primary"),
                card(
                    verbatimTextOutput("mesage_box", placeholder = TRUE)
                ),
            )
            
        )
        
    }) |> bindEvent(input$last_cell_clicked)
    
    
    # step 6: mfi channel info.
    observe({
        if (isTruthy(input$select_mfi_ch)) {
            removeUI(selector = "#mfi_channel_div")
        }

        ch_check_mat = input_files$ch_check_mat
        assay_data = input_files$assay_data
        select_mfi_ch = input$select_mfi_ch
        
        insert_me = select_mfi_ch
        
        if (select_mfi_ch=="") {
            insert_me = "No channel selected"
        }else{
            insert_me = select_mfi_ch
        }
        
        if (select_mfi_ch!="") {
            mia_check = ch_check_mat[,select_mfi_ch] == 0

            if (sum(mia_check) > 0) {
                msg1 = paste0(sum(mia_check),
                              " fcs file(s) do not have the selected channel.")

                msg2 = assay_data[mia_check, "Filename"][1]

                msg3 = "Select another channel or fix issue and try again."

                insert_me = tags$div(
                    tags$p(msg1,
                           class="h6 text-danger"),
                    tags$br(),
                    tags$p("Example:",
                           class="h6 text-danger"),
                    tags$p(msg2,
                           class="h6 text-danger"),
                    tags$br(),
                    tags$p(msg3,
                           class="h6 text-danger")
                )

            }else{

                insert_me = tags$div(
                    tags$br(),
                    tags$p("Proceed to annotation page",
                           class="h6 text-secondary"),
                    actionButton(
                        inputId = "add_notes",
                        label = "Add notes to your experiment",
                        class = "btn-secondary"
                    )
                )
            }
        }
        
        insertUI(
            selector = "#select_mfi_ch_div",
            where = "afterEnd",

            ui = tags$div(
                id = "mfi_channel_div",
                insert_me
            )
        )
        
        err_react_vals$select_mfi_ch = select_mfi_ch

    }) |> bindEvent(input$select_mfi_ch)
    
    
    # step 7: annotation page
    observe({
        if (isTruthy(input$add_notes)) {
            removeUI(selector = "#main_contents1")
        }

        insert_me1 = tags$div(
            tags$p("Select Experiment Type",
                   class="h5 text-primary fw-bold"),
            tags$div(
                tags$p("Specify the experiment type and dose type",
                       class="text-secondary"),
            ),

            tags$div(
                class="row",
                bslib::layout_column_wrap(
                    selectInput(
                        "experiment_type",
                        "Experiment type:",
                        list("None" = "none",
                             "Binding Experiment" = "binding_expr",
                             "Internalization Experiment" = "intern_expr",
                             "Other" = "other")
                    ),
                    selectInput(
                        "dose_type",
                        "Dose type:",
                        list("None" = "none",
                             "Single Dose" = "single_dose",
                             "Multi-Dose Titration" = "titration",
                             "Multi-Dose Non-titration"= "multi_dose",
                             "Other"="other")
                    )
                ),
                
                tags$p("Type in additional notes",
                       class="h5 text-primary fw-bold"),
                tags$div(
                    tags$p("Include any additional notes here that will
                           help to understand the context of this experiment.",
                           class="text-secondary"),
                    tags$div(
                        textAreaInput( 
                            "notes", 
                            "Text input", 
                            placeholder = "The quick brown fox jumped over the lazy dog...",
                            width="480px",
                        )
                    ),
                ),
            ),

            tags$br(),
            actionButton("qc_report",
                         "Proceed to Generate Semantic QC Report",
                         class="btn-warning",
                         disabled = TRUE)
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
                        # do nothing
                    )
                )
            )
        )
        
    }) |> bindEvent(input$add_notes)
    
    
    # step 8: enable "Proceed to Generate QC Report"
    observe({
        if (!is.null(input$experiment_type) & !is.null(input$dose_type)) {
            if (input$experiment_type!="none" & input$dose_type!="none") {
                updateActionButton(session, "qc_report", disabled = FALSE)
            }else{
                updateActionButton(session, "qc_report", disabled = TRUE)
            }
        }
    })
    
    
    # step 9: semantic qc report
    observe({
        data_list = err_react_vals$data_list
        merge_info = data_list[["merge_info"]]
        pinfo_sheets = data_list[["pinfo_sheets"]]
        
        app_dir = paste0(getwd(), "/")
        preview_path = file.path(app_dir, "www/docs/tlgs/qc-report-preview.pdf")
        make_qc_report(pinfo_sheets, merge_info, preview_path, approved = FALSE)
        
        insert_me = tags$div(
            id = "qc_report_div",
            
            tags$div(
                class="row",
                
                tags$p("Semantic QC Report",
                       class="h5 text-primary fw-bold"),
                
                tags$p(paste0("Analysis date: ", Sys.Date()),
                       class="text-secondary"),
                
                tags$div(
                    style="height: 450px",
                    tags$iframe(
                        src="docs/tlgs/qc-report-preview.pdf",
                        width="100%",
                        height="100%"
                    )
                ),
                
                tags$p("Review and accept for semantic accuracy.",
                       class="text-secondary"),
            ),
            
            tags$div(
                class="row",
                bslib::layout_column_wrap(
                    tags$a("Semantic Error Found (Please Fix & Re-start)",
                           href = "",
                           class="btn btn-secondary"),
                    actionButton("accept_report",
                                 "Accept QC Report",
                                 class="btn-primary")
                )
            )
        )
        
        insertUI(
            selector = "#main_contents_col2_topmark_div",
            where = "afterEnd",
            ui = insert_me
        )
        
        
    }) |> bindEvent(input$qc_report)
    
    
    # step 10: download report and exit
    observe({
        if (isTruthy(input$selected_assay)) {
            removeUI(selector = "#main_contents1")
            removeUI(selector = "#selected_project_div")
        }
        
        data_list = err_react_vals$data_list
        merge_info = data_list[["merge_info"]]
        pinfo_sheets = data_list[["pinfo_sheets"]]
        
        # clean qc_report folder
        data_path = gsub("/$", "", err_react_vals$path2_proj)
        data_path = paste0(data_path, "/", "qc_report")
        reset_dir(data_path)
        
        # save qc'd merged sheet
        approved_qc_report(data_path, 
                           pinfo_sheets, 
                           merge_info,
                           author_qc = paste0(Sys.info()[["user"]], 
                                              " | ",
                                              Sys.info()[["nodename"]]),
                           experiment_type=input$experiment_type,
                           dose_type=input$dose_type,
                           notes = input$notes)

        # write mfi channel csv for fcs assays
        if (input$selected_assay=="fcs" && isTruthy(input$select_mfi_ch)) {
            mfi_channel = data.frame(mfi_channel = input$select_mfi_ch)
            write.csv(mfi_channel,
                      file = paste0(data_path, "/", Sys.Date(), "-mfi-channel.csv"),
                      row.names = FALSE)
        }
        
        session$reload()
        
    }) |> bindEvent(input$accept_report)
    
}



