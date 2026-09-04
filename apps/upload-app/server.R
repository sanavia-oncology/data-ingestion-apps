# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    #------------------------------ step 0: on start
    # Paths, bucket and the background-sync settings, read once per session
    # from ~/.env_data_ingestion_apps. build_cfg() never fails on a missing
    # key - cfg_problems() reports what is blank and the sidebar shows it,
    # which beats dying at startup on a laptop that is half configured.
    cfg = build_cfg()

    rv = reactiveValues(
        folders   = read_folders(cfg),
        projects  = NULL,
        manifest  = read_manifest(manifest_path(cfg, read_folders(cfg))),
        refreshed = Sys.time()
    )


    rescan = function() {
        rv$folders  = read_folders(cfg)
        rv$projects = discover_projects(rv$folders)
        rv$manifest = read_manifest(manifest_path(cfg, rv$folders))
        rv$refreshed = Sys.time()
    }

    observe({ rescan() })

    # The table's own view of the world: discovery plus the two things that
    # live outside the file tree - what the last sync pass did, and whether
    # the project is flagged for the web.
    projects_view = reactive({
        df = rv$projects
        if (is.null(df) || nrow(df) == 0) return(NULL)

        df$Status = status_flags(rv$manifest, df)
        df
    })

    # Which rows the user has actually got selected, as full project rows.
    selected_projects = reactive({
        df = projects_view()
        if (is.null(df)) return(NULL)

        keep = rep(TRUE, nrow(df))
        if (isTruthy(input$status) && input$status != "All") {
            keep = keep & df[["Status"]] == input$status
        }
        shown = df[keep, , drop = FALSE]

        sel = input$projects_table_rows_selected
        if (is.null(sel) || length(sel) == 0) return(NULL)
        shown[sel, , drop = FALSE]
    })

    #------------------------------ step 1: the projects table
    observe({
        table_front_page = projects_view()

        if (is.null(table_front_page)) {
            insert_me1 = tags$p("No folders chosen yet. Run
                                 apps/upload-app/scripts/fc_sync_start.command",
                                class="h6 text-secondary")
            insert_me2 = NULL
        } else {
            output$projects_table = DT::renderDataTable(DT::datatable({
                data = table_front_page
                if (input$status != "All") {
                    data = data[data[["Status"]] == input$status,]
                }
                data[, PROJECT_TABLE_COLS, drop = FALSE]
            },
            selection = "multiple",
            options = list(pageLength = 7,
                           dom = "tpf",
                           # dt-nowrap everywhere but Project Name (col 1, the
                           # rowname column being 0): these names run to 50
                           # characters and would push Status off the edge.
                           columnDefs = list(
                               list(className = 'dt-nowrap', targets = c(0, 2)))
            )))

            insert_me1 = tags$div(
                tags$p("Uploaded Projects",
                       class="h3 text-primary fw-bold text-center"),
                tags$p("Select one or more rows, then Add or Remove",
                       class="h6 text-secondary text-center"),
                fluidRow(
                    selectInput("status",
                                "Status",
                                c("All", "Waiting", "Added", "Removed"))
                ),
                DT::dataTableOutput("projects_table")
            )

            insert_me2 = tags$div(
                tags$p("Publish", class="h5 text-primary fw-bold"),
                tags$p("Add lists a project on the web. Remove withdraws it.",
                       class="h6 text-secondary"),
                actionButton("add_selected", "Add",
                             class="btn-secondary w-100"),
                tags$br(), tags$br(),
                actionButton("remove_selected", "Remove",
                             class="btn-secondary w-100")
            )
        }

        removeUI(selector = "#main_contents1")

        insertUI(
            selector = "#main_contents",
            where    = "afterEnd",
            ui = tags$div(
                id = "main_contents1",
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col-10",
                        id    = "main_contents_col1",
                        insert_me1
                    ),
                    tags$div(
                        class = "col-2",
                        id    = "main_contents_col2",
                        insert_me2
                    )
                )
            )
        )
    }) |> bindEvent(rv$refreshed)


    #------------------------------ step 2: publish flags
    write_flags = function(display) {
        sel = selected_projects()
        if (is.null(sel) || nrow(sel) == 0) {
            showNotification("Select one or more projects first.",
                             type = "warning", duration = 3)
            return(invisible(NULL))
        }

        ok = tryCatch({
            set_display(cfg, manifest_path(cfg, rv$folders), sel, display)
            TRUE
        }, error = function(e) {
            showNotification(paste("Could not write the manifest:",
                                   conditionMessage(e)),
                             type = "error", duration = 6)
            FALSE
        })
        if (!ok) return(invisible(NULL))

        showNotification(sprintf("%s %d project(s)",
                                 if (display == "yes") "Added" else "Removed",
                                 nrow(sel)),
                         type = "message", duration = 4)
        rescan()
    }

    observe({ write_flags("yes") }) |> bindEvent(input$add_selected)
    observe({ write_flags("no")  }) |> bindEvent(input$remove_selected)





}
