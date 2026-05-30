# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    path_to_app = "/Users/kwameokrah/sanavia_apps/checkR/apps/order-recvd"
    
    # step 0
    # project_fldr
    project_fldr = "/Users/kwameokrah/data_depo/genscript-recvd"
    
    # organize file paths by order
    order_files_paths = get_paths_by_order(project_fldr)
    
    # make merged_sheet.csv if it does not exist
    for (order_folder in names(order_files_paths)) {
        fls = order_files_paths[[order_folder]]
        
        if (!length(grep("merged_sheets.csv$", fls)) > 0) {
            order_sheets = read_order_sheets(fls)
            merged_sheets = merge_order_sheets(order_sheets)
            merged_sheets[["Merge Date"]] = Sys.Date()
            
            # add no_seqs
            cn = colnames(merged_sheets)
            seq_cols = merged_sheets[,grep("^sequence", cn),drop=F]
            no_seqs = rowSums(!is.na(seq_cols))
            merged_sheets[["no_of_seqs"]] = no_seqs
            
            # drop duplicate columns
            merged_sheets = merged_sheets[,-grep("\\.[1-9]$", cn), drop=F]
            
            # add keys (if available)
            fls_keys = fls[grep("\\/keys\\/", fls)]
            if (length(fls_keys) > 0) {
                KEY = read.csv(fls_keys, header=T, check.names=F) 
                rownames(KEY) = KEY$assembly_id
                print(all(rownames(KEY) %in% merged_sheets$Name))
                print(all(merged_sheets$Name %in% rownames(KEY)))
                
                KEY_Match = KEY[merged_sheets$Name,,drop=F]
                merged_sheets = cbind(merged_sheets, KEY_Match)
            }
            
            save_dir = sapply(strsplit(fls[1], order_folder), "[[", 1)
            save_dir = paste0(save_dir, order_folder, "/00_merged_sheets")
            
            if (!dir.exists(save_dir)) {
                dir.create(save_dir)
            }
            
            file = paste0(save_dir, "/", order_folder, "-merged_sheets.csv")
            write.csv(merged_sheets, file=file, row.names=FALSE)    
        }
    }
    
    # read in all merged_sheet.csv
    order_files_paths = get_paths_by_order(project_fldr)
    merged_sheets_list = list()
    for (order_folder in names(order_files_paths)) {
        fls = order_files_paths[[order_folder]]
        
        dat = read.csv(fls[grep("merged_sheets.csv$", fls)], 
                       header = TRUE,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

        dat_oid = sapply(strsplit(dat[["Order ID"]][1], "-"), "[[", 1)
        folder_oid = sapply(strsplit(order_folder, "_Order_"), "[[", 2)
        
        if (dat_oid!=folder_oid) {
            stop("dat_oid is not equal to folder_oid (K.Okrah)")
        }
        
        merged_sheets_list[[order_folder]] = dat
    }
    
    table_front_page = front_page_table(merged_sheets_list)
    
    names(merged_sheets_list) = sapply(strsplit(names(merged_sheets_list), 
                                                "_Order_"), "[[", 2)

    # load_data
    observe({
        if (isTruthy(input$load_data)) {
            removeUI(selector = "#load_data")
        }
        
        # column 1
        output$table = DT::renderDataTable(DT::datatable({
            table_front_page
        }, 
        selection = "single",
        options = list(pageLength = 5,  
                       dom = "tpf",
                       columnDefs = list(
                           list(className = 'dt-nowrap', targets = '_all'))
        )))
        
        insert_me1 = tags$div(
            tags$p("Table of Orders", 
                   class="h5 text-primary fw-bold"),
            tags$p("Click on row to get order information.",
                   class="text-secondary"),
            DT::dataTableOutput("table"),
            tags$br(),
            tags$div(
                tags$p("Upload/Sync Orders", 
                       class="h5 text-secondary fw-bold"),
                tags$p("Click below to push order information to AWS and Benchling.",
                       class="text-secondary"),
                checkboxInput("order_recvd", "Order Received?", FALSE),
                tags$div(                
                    actionButton("upload", 
                                 "Upload/Sync to AWS and Benchling",
                                 class="btn-warning")
                )
            )
        )
        
        # column 2
        insert_me2 = tags$div(
            tags$p("Order Summary", 
                   class="h5 text-primary fw-bold"),
            tags$p(
                "View order summary tables and graphs.",
                   class="text-secondary"),
            tags$div(
                id="order_sheet_div"
            )
        )
        
        # inset UI
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
        
        insertUI(
            selector = "#current_date",
            where = "afterEnd",
            ui = tags$div(
                actionButton(inputId = "exit", 
                             label = "Exit Back to Homepage")
            )
        )

    }) |> bindEvent(input$load_data)

    # table_rows_selected
    observe({
        if (isTruthy(input$table_rows_selected)) {
            removeUI(selector = "#selected_order_sheet_div")
            removeUI(selector = "#download_merged_sheet")
        }

        tlg_path = paste0(path_to_app, "/www/docs/tlgs/")
        reset_dir(tlg_path)
        
        order_id = table_front_page[input$table_rows_selected, "Order ID"]
        ms = merged_sheets_list[[order_id]]
        
        fig_path = paste0(tlg_path, "merged-order-sheets-figs.pdf")
        plot_merged_sheet(ms, fig_path=fig_path)

        # insert UI
        insertUI(
            selector = "#order_sheet_div",
            where    = "afterEnd",
            ui = tags$div(
                id = "selected_order_sheet_div",
                tags$div(
                    
                    tags$p(order_id,
                           class="h6 text-primary"),
                    tags$div(
                        style="height: 350px",
                        tags$iframe(
                            src=paste0("docs/tlgs/merged-order-sheets-figs.pdf"),
                            width="100%",
                            height="100%"
                        )
                    )
                ),
                tags$br(),
                tags$div(
                    tags$p("Download Merged Order Sheets", 
                           class="h5 text-secondary fw-bold"),
                    tags$p(
                        "Download merged order sheets. Includes protein name key if applicable.",
                        class="text-secondary"),
                    tags$div(
                        actionButton("download_merged_sheet", 
                                     "Download Merged Sheets w/ Key",
                                     class="btn-primary")
                    )
                )
            )
        )

    }) |> bindEvent(input$table_rows_selected)

    # download_merged_sheet
    observe({
        order_id = table_front_page[input$table_rows_selected, "Order ID"]
        ms = merged_sheets_list[[order_id]]
        print(head(ms))
    }) |> bindEvent(input$download_merged_sheet)
}