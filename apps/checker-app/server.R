# author: Kwame Okrah
# date: 2025-11-09

server = function(input, output, session) {
    # Step 1: Read in data
    input_react_vals = reactiveValues()
  
    observe({
        if (isTruthy(input$read_data)) {
            removeUI(selector = "#pass_fail_div")
        }
      
        file_paths = get_file_paths(input$data_path,
                                    input$selected_assay)
            
        dir_test = file_paths[["dir_test"]]

        if (dir_test == "fail") {            
            insertUI(
                selector = "#read_data", 
                where = "afterEnd",
                ui = tags$div(
                    id = "pass_fail_div",
                    tags$p(
                        class = "text-danger",
                        "Path entered is not correct
                         please check and try again.")
                )
            )
        }
            
        if (dir_test == "pass") {
            pinfo_csv_paths = file_paths[["pinfo_csv_paths"]]
            assay_file_paths = file_paths[["assay_file_paths"]]
          
            if (length(pinfo_csv_paths) > 0) {
                pinfos_sheets = read_pinfo_csvs(pinfo_csv_paths)
                
                if (length(assay_file_paths) > 0) {
                    assay_data = read_assay_data(assay_file_paths, input$selected_assay)
                }else{
                    assay_data = NULL    
                }
                
                n_pinfo = length(pinfos_sheets)
                n_assay = length(assay_file_paths)
            
                if (n_pinfo == 1) {
                    msg_pinfo = paste(n_pinfo, "plate information sheet")
                }else{
                    msg_pinfo = paste(n_pinfo, "plate information sheets")
                }

                if (input$selected_assay == "none") {
                    msg_assay = "No assay data files"
                }else{
                    if (n_assay == 1) {
                        msg_assay = paste(n_assay, input$selected_assay, "file")
                    }else{
                        msg_assay = paste(n_assay, input$selected_assay, "files")
                    }
                }

                insertUI(
                    selector = "#read_data", 
                    where = "afterEnd",
                    ui = tags$div(
                        id = "pass_fail_div",
                        tags$p(
                            class = "text-muted",
                            "Files detected!"),
                        card(
                            card_image("www/img/data_loaded1.jpg"),
                            card_body(tags$p(
                                class = "fs-6",
                                msg_pinfo,
                                tags$br(),
                                msg_assay)
                            ),
                            actionButton(
                                inputId = "perform_checks",
                                label = "Perform QC",
                                class = "btn-primary")
                        ),
                    )
                )

                data_list = list(pinfo_sheets = pinfos_sheets, 
                                 assay_data = assay_data)
            
                input_react_vals$data_list = data_list
              
            }else{

                insertUI(
                    selector = "#read_data", 
                    where = "afterEnd",
                    ui = tags$div(
                        id = "pass_fail_div",
                        tags$p(
                            class = "text-danger",
                            "No plate information sheets detected
                             please check and try again.")
                    )
                )
                
            }
        }
        
    }) |> bindEvent(input$read_data)

    # Step 2: Perform QC checks
    err_react_vals = reactiveValues()
  
    observeEvent(input$perform_checks, {
      
        if (isTruthy(input$perform_checks)) {
            removeUI(selector = "#main_contents1")
        }

        data_list = input_react_vals$data_list
        pinfo_sheets = data_list[["pinfo_sheets"]]
        assay_data = data_list[["assay_data"]]

        res = perform_plateinfo_checks(pinfo_sheets)
        checks_summary = res[["checks_summary"]]
        checks_detailed = res[["checks_detailed"]]
        rm("res")

        key = paste0("PLATE_", sprintf(1:ncol(checks_summary), fmt="%02d"))
        names(key) = colnames(checks_summary)

        names(pinfo_sheets) = key[names(pinfo_sheets)]
        colnames(checks_summary) = key[colnames(checks_summary)]
        names(checks_detailed) = key[names(checks_detailed)]
           
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
                        style = "color: #343637ff; font-size: 12px; border: 1px solid #a7a9aaff; width: 500px;",
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

        if (is.null(assay_data)) {
            merge_info = NULL
            assay_position_check_summary = NULL
            assay_position_check_detailed = NULL
        }else{
            res = merge_assay_data(assay_data, pinfo_sheets)
            merge_info = res[["merge_info"]]
            assay_position_check_summary = res[["assay_position_check_summary"]]
            assay_position_check_detailed = res[["assay_position_check_detailed"]]
            rm("res")  
          
            mcheck = sapply(merge_info, function(x) all(x$merge_checks))
            mcheck = ifelse(mcheck, "Pass", "Fail")
    
            checks_summary = rbind(checks_summary, "DATA MERGE"=mcheck)
        }
    
        checks_summary1 = checks_summary[1,,drop=F]
        checks_summary2 = checks_summary[2:nrow(checks_summary),,drop=F]
        
        cond1 = !all(checks_summary1[1,] == "Pass")
        cond2a = !all(checks_summary2 == "Pass")
        cond2b = all(checks_summary2 == "Pass")
        
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
                            each cell to get a detailed error description message.
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
                    tp = tags$p("Almost there you can do it! Please review and fix each error.")
                    bn = NULL
                    img_src = "img/fun/waiting-sana-cat1.png"
              
                    insert_me1 = tags$div(
                                    tags$p("Syntax Quality Control Checks", 
                                    class="h5 text-primary fw-bold"),
                                    tags$p(
                                    tags$strong("Click"),
                                    "
                                    a cell to get a detailed error description message.
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
                        class = "h6 card-title text-primary",
                        style = "margin-bottom:2px;",
                        "100% success!")
                    tp = tags$p("Review QC report for logical/semantic errors.")
                    bn = actionButton("qc_report", 
                                      "Go to Semantic QC Report", 
                                      class="btn-primary")
                    img_src = "img/fun/happy-sana-cat1.png"
              
                    file_path = paste0(app_dir, "www/docs/tlgs/qc-report.pdf")
                    make_qc_report(pinfo_sheets, merge_info, file_path, approved = FALSE)

                    insert_me1 = tags$div(
                                    tags$p("Syntax Quality Control Checks", 
                                        class="h5 text-primary fw-bold"),
                                    tags$p(
                                    tags$strong("Click"),
                                    "
                                    a cell to get a detailed summary of the column contents.
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

        err_react_vals$data_list = list(checks_detailed = checks_detailed,
                                        merge_info = merge_info,
                                        pinfo_sheets = pinfo_sheets,
                                        assay_position_check_summary = assay_position_check_summary,
                                        assay_position_check_detailed = assay_position_check_detailed,
                                        rev_key = rev_key,
                                        QC_CODE = QC_CODE)
    })
  
    # Step 3: Message Box
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

    # Step 4: Semantic QC Report
    observeEvent(input$qc_report, {
        if (isTruthy(input$qc_report)) {
            removeUI(selector = "#msg_box_div")
        }

#------------------------------ Modification starts here--#    
        if (input$selected_assay=="fcs") {
            insert_me = tags$div(
                id = "qc_report_div",

                tags$div(
                    class="row",

                    tags$p("Semantic QC Report", 
                        class="h5 text-primary fw-bold"),

                    tags$p(paste0("Analysis date: ", Sys.Date()),
                        class="text-secondary"),

                    tags$div(
                        style="height: 435px",
                        tags$iframe(
                            src="docs/tlgs/qc-report.pdf",
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
                        actionButton("back2_msg",
                                    "Back to Message Box", 
                                    class="btn-secondary"),
                        actionButton("mfi_gating_ch", 
                                    "Select Antibody MFI Channel",
                                    class="btn-primary")
                    )
                )
            )
        }else{
            insert_me = tags$div(
                id = "qc_report_div",

                tags$div(
                    class="row",

                    tags$p("Semantic QC Report", 
                        class="h5 text-primary fw-bold"),

                    tags$p(paste0("Analysis date: ", Sys.Date()),
                        class="text-secondary"),

                    tags$div(
                        style="height: 477px",
                        tags$iframe(
                            src="docs/tlgs/qc-report.pdf",
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
                        actionButton("back2_msg",
                                    "Back to Message Box", 
                                    class="btn-secondary"),
                        actionButton("download_report", 
                                    "Accept & Download QC Report",
                                    class="btn-warning")
                    )
                )
            )
        }
#------------------------------ Modification ends here--#
        
        insertUI(
            selector = "#main_contents_col2_topmark_div", 
            where = "afterEnd",
            ui = insert_me
        )
    })

    # Go back to Box Message
    observeEvent(input$back2_msg, {
        if (isTruthy(input$back2_msg)) {
            removeUI(selector = "#qc_report_div")
        }

        insert_me = tags$div(
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

        insertUI(
            selector = "#main_contents_col2_topmark_div", 
            where = "afterEnd",
            ui=insert_me)
    })

    # Download QC report
    observeEvent(input$download_report, {
        if (isTruthy(input$download_report)) {
            removeUI(selector = "#qc_report_div")
        }
        #-------------------- new insert start ---------------------------------------#
        if (isTruthy(input$download_report)) {
            removeUI(selector = "#mfi_ch_box_div")
        }
        #-------------------- new insert end -----------------------------------------#

        data_list = err_react_vals$data_list
      
        merge_info = data_list[["merge_info"]]
        pinfo_sheets = data_list[["pinfo_sheets"]]                                 
        data_path = input$data_path
      
        approved_qc_report(data_path, pinfo_sheets, merge_info)

        #-------------------- new insert start ---------------------------------------#

        if (!is.null(input$select_mfi_ch)) {

            if (input$select_mfi_ch!="None") {
                report_path = paste0(gsub("/$", "", data_path), "/", "qc_report")
                table_path = paste0(report_path, "/", Sys.Date(), "-mfi-channel.csv")
                mfi_channel = data.frame(mfi_channel=input$select_mfi_ch)
                write.csv(mfi_channel, file=table_path, row.names=FALSE)

                                insert_me = tags$div(
                    id = "msg_box_div",

                    tags$p("Done", 
                        class="h5 text-primary fw-bold"),
                    tags$div(
                        id = "main_contents_col2_div",
                    ),
                    tags$div(
                        id = "done_message",
                        card(tags$p("An approved qc-report and a merged data
                                    file have been saved in your project 
                                    folder. Please upload the approved project 
                                    folder to the required location.", 
                                    class="text-secondary")),       
                        tags$p("You may exit the app.", class="text-secondary")
                    )
                )

            }else{
                insert_me = tags$div(
                    id = "msg_box_div",

                    tags$p("MFI Channel Info. Missing", 
                        class="h5 text-danger fw-bold"),
                    tags$div(
                        id = "main_contents_col2_div",
                    ),
                    tags$div(
                        card(tags$p("Please go back and add the MFI channel.", 
                                    class="text-secondary")),       
                            actionButton("qc_report", 
                                         "Go Back",
                                         class="btn-warning")
                    )
                )
            }

        }else{
            if (input$selected_assay=="fcs") {
                insert_me = tags$div(
                    id = "msg_box_div",

                    tags$p("MFI Channel Info. Missing", 
                        class="h5 text-danger fw-bold"),
                    tags$div(
                        id = "main_contents_col2_div",
                    ),
                    tags$div(
                        card(tags$p("Please go back and add the MFI channel.", 
                                    class="text-secondary")),       
                            actionButton("qc_report", 
                                         "Go Back",
                                         class="btn-warning")
                    )
                )
            }else{
                insert_me = tags$div(
                    id = "msg_box_div",

                    tags$p("Done", 
                        class="h5 text-primary fw-bold"),
                    tags$div(
                        id = "main_contents_col2_div",
                    ),
                    tags$div(
                        id = "done_message",
                        card(tags$p("An approved qc-report and a merged data
                                    file have been saved in your project 
                                    folder. Please upload the approved project 
                                    folder to the required location.", 
                                    class="text-secondary")),       
                        tags$p("You may exit the app.", class="text-secondary")
                    )
                )                
            }
        }

        #-------------------- new insert end -----------------------------------------#

        insertUI(
            selector = "#main_contents_col2_topmark_div", 
            where = "afterEnd",
            ui=insert_me)
      
          session$sendCustomMessage(type = 'testmessage',
                message = 'Thank you for clicking')
    })
  
    # Others
    observeEvent(input$read_data, {
        removeUI(selector = "#main_contents1")
    })

    #--------- (new) Add MFI gating channel-------#
    observeEvent(input$mfi_gating_ch, {
        if (isTruthy(input$mfi_gating_ch)) {
            removeUI(selector = "#qc_report_div")
        }
        
        insert_me = tags$div(
            id = "mfi_ch_box_div",
            
            tags$p("Select MFI signal channel", 
                class="h5 text-primary fw-bold"),       
            tags$div(
                id = "main_contents_col2_div",
            ),
            tags$div(
                tags$p("Select the channel that should be used to compute MFI.", 
                    class="text-secondary"),
                selectizeInput( 
                    "select_mfi_ch", 
                    "Select options below:", 
                    list("None"="None",
                         `RL1-H` = "<RL1-H> RL1-H", `YL1-H` = "<YL1-H> YL1-H",
                         `BL1-A` = "<BL1-A> BL1-A", `BL2-A` = "<BL2-A> BL2-A",
                         `YL1-A` = "<YL1-A> YL1-A", `YL2-A` = "<YL2-A> YL2-A", `YL3-A` = "<YL3-A> YL3-A",
                         `RL1-A` = "<RL1-A> RL1-A", `RL2-A` = "<RL2-A> RL2-A", `RL3-A` = "<RL3-A> RL3-A",
                         `VL1-A` = "<VL1-A> VL1-A", `VL2-A` = "<VL2-A> VL2-A", `VL3-A` = "<VL3-A> VL3-A",
                         `VL4-A` = "<VL4-A> VL4-A", `VL5-A` = "<VL5-A> VL5-A", `VL6-A` = "<VL6-A> VL6-A"
                    )
                ),
                textOutput("mfi_ch_value"),
                tags$br(),
                tags$div(      
                    actionButton("download_report", 
                                 "Accept & Download QC Report",
                                 class="btn-warning"))         
            ))
        
        output$mfi_ch_value <- renderText({input$select_mfi_ch})
        
        insertUI(
            selector = "#main_contents_col2_topmark_div", 
            where = "afterEnd",
            ui=insert_me)
        
    })
  
}
