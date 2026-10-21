# author: Kwame Okrah
# date: 2026-03-05

main_contents = tags$div(
    tags$div(
        id = "main_contents"
    )
)

app_card = tags$div(
    style="height: 660px; font-size: 14px;",
    
    layout_sidebar(
        height = "100%",
        border_color = "rgba(227, 227, 227, 1)",
        bg = "rgba(255, 255, 255, 1)",
        
        sidebar = sidebar(
            width = 250,
            open = "open",
            bg = "rgba(238, 238, 238, 1)",
            
            tags$p(id="current_date",
                   paste0("Today's date: ", Sys.Date()),
                   class="text-secondary"),
            
            tags$div(
                id = "selected_assay_div",
                selectizeInput(
                    inputId = "selected_assay",
                    label = "Select assay type",
                    choices = c("Flow Cytometry" = "fcs",
                                "Derived Results" = "derived-results",
                                "Elisa" = "varioskan-skax",
                                "Octet Kinetics"="frd"),
                    selected = character(0),
                    options = list(
                        placeholder = 'Select assay type',
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                )  
            )
        ),
        
        main_contents
    )
)

application_page = bslib::nav_panel(
    tags$style(
        HTML("
            textarea.form-control {
            font-size: 13px;
            }
    
            .btn {
            padding: 6px 10px;
            font-size: 14px;
            }
    
            .cell-btn:hover {
            color: white;
            font-weight: bold;
            cursor: pointer;
            }
    
            #data_path { 
            color: #017BC2; 
            }

            .sana-consts {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
            margin-bottom: 14px;
            }

            .sana-const {
            display: inline-flex;
            font-size: 13px;
            line-height: 1;
            border: 1px solid #cfd4e6;
            border-radius: 4px;
            overflow: hidden;
            }

            .sana-const-key {
            padding: 5px 8px;
            background: #eef0f8;
            color: #5c6370;
            }

            .sana-const-val {
            padding: 5px 8px;
            background: #ffffff;
            color: #212529;
            font-weight: 600;
            }

            #sana_well_table th {
            white-space: normal;
            }

            #sana_well_table th.sorting,
            #sana_well_table th.sorting_asc,
            #sana_well_table th.sorting_desc {
            padding-right: 18px;
            }
        ")
    ),
    
    # The QC cell buttons in qc_table.R carry onclick="sendButtonID(this.id)".
    # Without this, that call is a ReferenceError and input$last_cell_clicked
    # never fires, so clicking a failed cell does nothing and the message box
    # keeps its placeholder. Restored from checker-app (commit 0c62be04); it
    # arrived here commented out and was later dropped entirely.
    tags$script(
        HTML("
            function sendButtonID(id) {
            Shiny.setInputValue('last_cell_clicked', id, {priority: 'event'});
            }
        ")
    ),

    title = "Return to home page",
    page_banner("Experiment Data Transfer QC"),

    app_card
)
