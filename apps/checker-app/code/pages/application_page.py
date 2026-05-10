from shiny import ui
from code.helpers.page_banner import page_banner

_css = """
    textarea.form-control { font-size: 13px; }
    .btn { padding: 6px 10px; font-size: 14px; }
    .cell-btn:hover { color: white; font-weight: bold; cursor: pointer; }
    #data_path { color: #017BC2; }
"""

_js = """
    function sendButtonID(id) {
        Shiny.setInputValue('last_cell_clicked', id);
    }
"""

_main_contents = ui.div(ui.div(id="main_contents"))

_app_card = ui.div(
    ui.layout_sidebar(
        ui.sidebar(
            ui.input_select(
                "selected_assay",
                "Select assay type. Leave as None if no assay data.",
                choices={
                    "none": "None",
                    "fcs": "Flow Cytometry",
                    "derived-results": "Derived Results",
                },
            ),
            ui.input_text_area(
                "data_path",
                "Enter path to project folder.",
                value="",
                height="75px",
            ),
            ui.input_action_button(
                "read_data", "Read in data", class_="btn-secondary"
            ),
            width=290,
            open="always",
            bg="rgba(238, 238, 238, 1)",
        ),
        _main_contents,
        height="100%",
        border_color="rgba(227, 227, 227, 1)",
        bg="rgba(255, 255, 255, 1)",
    ),
    style="flex: 1; min-height: 0; font-size: 14px;",
)

application_page = ui.nav_panel(
    "Data Transfer QC",
    ui.tags.style(ui.HTML(_css)),
    ui.tags.script(ui.HTML(_js)),
    page_banner("Experiment Data Transfer QC"),
    _app_card,
)
