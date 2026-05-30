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
            shinyDirButton(id = "folder", 
                           label = "Choose Project Folder",
                           title = "Select a folder")
        ),
        
        main_contents
    )
)

selectizeInput(
    "x", "Label",
    choices = c("A", "B", "C"),
    selected = character(0),
    options = list(
        placeholder = 'Select an option',
        onInitialize = I('function() { this.setValue(""); }')
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
        ")
    ),
    
    title = "Data Transfer QC",
    page_banner("Experiment Data Transfer QC"),
    
    app_card
)



# # Landing block: when no assay is selected, show a "select assay" hint;
# # once an assay is picked the projects table replaces it. Both states live
# # inside #main_contents so the perform_checks observer can drop this whole
# # block by removing #projects_landing. Wrapped in a function so server.R
# # can re-insert it if the user changes assay after starting QC.
# make_projects_landing = function() {
#     tags$div(
#         id = "projects_landing",
#         class = "row",
#         tags$div(
#             class = "col",
#             id = "main_contents_col1",
#             conditionalPanel(
#                 condition = "!input.selected_assay",
#                 tags$p("Select an assay type",
#                        class = "h3 text-center fw-bold"),
#                 tags$p("Choose an assay type from the sidebar to see matching projects.",
#                        class = "text-secondary text-center")
#             ),
#             conditionalPanel(
#                 condition = "!!input.selected_assay",
#                 tags$div(
#                     id = "projects_select_area",
#                     tags$p("Select a project",
#                            class = "h3 text-center fw-bold"),
#                     tags$p("Click on a row to load the project, then press 'Perform QC'.",
#                            class = "text-secondary text-center"),
#                     DT::DTOutput("projects_table")
#                 )
#             )
#         ),
#         tags$div(
#             class = "col",
#             id = "main_contents_col2"
#         )
#     )
# }
# 
# main_contents = tags$div(
#     tags$div(
#         id = "main_contents",
#         make_projects_landing()
#     )
# )
# 
# selected_assay_label = "Select assay type"
# 
# app_card = tags$div(
#     style="height: 660px; font-size: 14px;",
# 
#     layout_sidebar(
#       height = "100%",
#       border_color = "rgba(227, 227, 227, 1)",
#       bg = "rgba(255, 255, 255, 1)",
# 
#       sidebar = sidebar(
#                     width = 290,
#                     open = "always",
#                     bg = "rgba(238, 238, 238, 1)",
# 
#                     selectizeInput(
#                         inputId = "selected_assay",
#                         label = selected_assay_label,
#                         choices = c("Flow Cytometry" = "fcs",
#                                     # "Elisa" = "varioskan-skax",
#                                     # "Octet Kinetics"="frd",
#                                     "Derived Results" = "derived-results"),
#                         selected = "",
#                         options = list(
#                             placeholder = "Select assay type...",
#                             onInitialize = I("function() { this.setValue(''); }"))),
# 
#                     tags$div(id = "upload_anchor")),
# 
#       main_contents
#     )
# )
# 
# application_page = bslib::nav_panel(
#     tags$style(
#         HTML("
#             textarea.form-control {
#             font-size: 13px;
#             }
# 
#             .btn {
#             padding: 6px 10px;
#             font-size: 14px;
#             }
# 
#             .cell-btn:hover {
#             color: white;
#             font-weight: bold;
#             cursor: pointer;
#             }
# 
#             /* Heading + table appear together once DT's first draw fires.
#                Paired with `initComplete` in server.R that adds .dt-loaded
#                on the wrapping #projects_select_area. */
#             #projects_select_area { opacity: 0; transition: opacity 0.15s ease; }
#             #projects_select_area.dt-loaded { opacity: 1; }
#         ")
#     ),
# 
#     tags$script(
#         HTML("
#             function sendButtonID(id) {
#             Shiny.setInputValue('last_cell_clicked', id);
#             }
# 
#             Shiny.addCustomMessageHandler('toggleBtn', function(msg) {
#                 var btn = document.getElementById(msg.id);
#                 if (btn) btn.disabled = msg.disable;
#             });
#         ")
#     ),
# 
#     title = "Data Transfer QC",
#     page_banner("Experiment Data Transfer QC"),
# 
#     app_card
# )
