from datetime import date
from shiny import ui
from code.helpers.page_banner import page_banner

_css = """
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
"""

_main_contents = ui.div(
    ui.div(id="main_contents")
)

_app_card = ui.div(
    ui.layout_sidebar(
        ui.sidebar(
            ui.p(f"Today's date: {date.today()}", class_="text-secondary"),
            ui.input_action_button("click_me1", "Click Me"),
            width=250,
            open="always",
            resizable=False,
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
    "Demo app structure",
    ui.tags.style(ui.HTML(_css)),
    page_banner("Demonstration of app layout"),
    _app_card,
)
