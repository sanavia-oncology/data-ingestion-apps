from shiny import ui

_CSS = """
body { display: flex; flex-direction: column; min-height: 100vh; }
.bslib-page-navbar > .tab-content { flex: 1; min-height: 0; padding: 0; margin: 0; }
textarea#data_path { font-family: monospace; font-size: 12px; color: #31387d; }
.btn { padding: 6px 10px; }
"""

_JS = """
function sendButtonID(id) {
    Shiny.setInputValue('last_cell_clicked', id, {priority: 'event'});
}
"""


def application_page():
    return ui.nav_panel(
        "Flow Cytometry Analysis",
        ui.tags.style(_CSS),
        ui.tags.script(_JS),
        ui.layout_sidebar(
            ui.sidebar(
                ui.p(ui.tags.small("Flow Cytometry Analysis App"), class_="text-muted"),
                ui.input_text_area(
                    "data_path",
                    "Project Folder Path",
                    placeholder="/path/to/project",
                    rows=4,
                ),
                ui.input_action_button(
                    "read_data",
                    "Read Flow Cytometry Data",
                    class_="btn-secondary w-100",
                ),
                width=290,
                open="always",
                style="background-color: rgba(238,238,238,1);",
            ),
            ui.div(
                ui.div(id="main_contents"),
                ui.div(id="main_contents1"),
                style="padding: 12px;",
            ),
        ),
    )
