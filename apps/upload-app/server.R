# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {
    #------------------------------ step 0: on start
    # Bucket and paths, read once per session from ~/.env_data_ingestion_apps.
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
        if (isTruthy(input$proj_group) && input$proj_group != "All") {
            keep = keep & df[["Project Group"]] == input$proj_group
        }
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
            insert_me1 = tags$p("No folder set. Add DATA_DIR to
                                 ~/.env_data_ingestion_apps",
                                class="h6 text-secondary")
            insert_me2 = NULL
        } else {
            output$projects_table = DT::renderDataTable(DT::datatable({
                data = table_front_page
                if (input$proj_group != "All") {
                    data = data[data[["Project Group"]] == input$proj_group,]
                }
                if (input$status != "All") {
                    data = data[data[["Status"]] == input$status,]
                }
                data[, PROJECT_TABLE_COLS, drop = FALSE]
            },
            selection = "multiple",
            options = list(pageLength = 7,
                           dom = "tpf",
                           # dt-nowrap everywhere but Project Name (col 3, the
                           # rowname column being 0): these names run to 50
                           # characters and would push Status off the edge.
                           columnDefs = list(
                               list(className = 'dt-nowrap', targets = c(0, 1, 2, 4)))
            )))

            insert_me1 = tags$div(
                tags$p("Uploaded Projects",
                       class="h3 text-primary fw-bold text-center"),
                tags$p("Select one or more rows, then Add or Remove",
                       class="h6 text-secondary text-center"),
                fluidRow(
                    selectInput("proj_group",
                                "Project Group",
                                c("All", sort(unique(table_front_page[["Project Group"]])))),
                    selectInput("status",
                                "Status",
                                c("All", "Published", "Not Published"))
                ),
                DT::dataTableOutput("projects_table")
            )

            insert_me2 = tags$div(
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

        # The two cases where nothing visibly happens: no rows picked, and the
        # write failing. Success needs no toast - the Status column shows it.
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
                             type = "error", duration = 8)
            FALSE
        })
        if (!ok) return(invisible(NULL))

        rescan()
    }

    observe({ write_flags("yes") }) |> bindEvent(input$add_selected)
    observe({ write_flags("no")  }) |> bindEvent(input$remove_selected)





}
