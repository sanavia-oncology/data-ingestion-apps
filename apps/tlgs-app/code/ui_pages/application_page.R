# author: Kwame Okrah
# date: 2026-03-05

main_contents = tags$div(
    tags$div(id = "main_contents"),
)

app_card = tags$div(
    style="height: 860px; font-size: 14px;",

    layout_sidebar(
      height = "100%",
      border_color = "rgba(227, 227, 227, 1)",
      bg = "rgba(255, 255, 255, 1)",

      sidebar = sidebar(
                    id = "my_sidebar",
                    width = 250,
                    open = "open",
                    bg = "rgba(238, 238, 238, 1)",

                    tags$p(paste0("Today's date: ", Sys.Date()),
                           class="text-secondary"),
                    
                    tags$div(id = "card_div_top")),
      
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

            /* Sidebar select dropdown */
            .bslib-sidebar-layout > .sidebar select.form-select {
                font-size: 13px;
            }

            /* Per-column filter inputs (selectize) above the projects
               table. Active-filter state tints the whole input blue so
               it stands out from inactive filters at a glance. */
            #projects_filter_ui .selectize-input.focus {
                border-color: #86b7fe !important;
                box-shadow: 0 0 0 0.2rem rgba(13,110,253,.15) !important;
            }
            #projects_filter_ui .selectize-input.has-items {
                border-color: #0d6efd !important;
                background-color: #e7f1ff !important;
                box-shadow: 0 0 0 0.15rem rgba(13,110,253,.15) !important;
            }
        ")
    ),

    title = "Flow Cytometry TLGs",
    page_banner("Make flow cytometry tables, graphs, and listings"),

    app_card
)
