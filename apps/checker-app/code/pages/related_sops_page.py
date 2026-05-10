from shiny import ui
from code.helpers.page_banner import page_banner

related_sops_page = ui.nav_panel(
    "Related SOPs",
    page_banner("Related SOPs"),
    ui.div(
        ui.tags.iframe(
            src="docs/sops/251010_SOP_template.pdf",
            width="100%",
            height="100%",
        ),
        class_="container",
        style="height: 660px;",
    ),
)
