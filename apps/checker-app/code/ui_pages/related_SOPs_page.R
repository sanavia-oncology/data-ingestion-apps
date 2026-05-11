# author: Kwame Okrah
# date: 2025-11-09

related_SOPs = tags$div(
    class="container",
    style="height: 660px",
    tags$iframe(
        src="docs/sops/251010_SOP_template.pdf",
        width="100%",
        height="100%"
    )
)

related_SOPs_page = bslib::nav_panel(
    title="Related SOPs",
    page_banner("Related SOPs"),
    related_SOPs
)