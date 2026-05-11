# author: Kwame Okrah
# date: 2025-11-09

main_contents = tags$div(
    tags$div(
        id = "main_contents",
    )
)

data_path_label = "Enter path to project folder."

selected_assay_label = "Select assay type. Leave as None if no assay data."

app_card = tags$div(
    style="height: 660px; font-size: 14px;",

    layout_sidebar(
      height = "100%",
      border_color = "rgba(227, 227, 227, 1)",
      bg = "rgba(255, 255, 255, 1)",

      sidebar = sidebar(
                    width = 290,
                    open = "always",
                    bg = "rgba(238, 238, 238, 1)",

                    selectInput(
                        inputId = "selected_assay", 
                        label = selected_assay_label,
                        choices = c("None" = "none", 
                                    "Flow Cytometry" = "fcs",
                                    "Elisa" = "varioskan-skax",
                                    "Octet Kinetics"="frd",
                                    "Derived Results" = "derived-results")),
                                    
                    textAreaInput( 
                        inputId = "data_path", 
                        label = data_path_label,
                        value = "",
                        height = 75),

                    actionButton(
                             inputId = "read_data",
                             label = "Read in data",
                             class = "btn-secondary")),
      
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
        ")
    ),
    
    tags$script(
        HTML("
            function sendButtonID(id) {
            Shiny.setInputValue('last_cell_clicked', id);
            }
        ")
    ),
      
    title = "Data Transfer QC",
    page_banner("Experiment Data Transfer QC"),
    
    app_card
)
