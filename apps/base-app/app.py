# author: Kwame Okrah

from pathlib import Path

from shiny import App, reactive, ui
from code.helpers.footer_section import footer_section
from code.pages.application_page import application_page

_navbar_css = """
    .navbar {
        background-color: rgba(0, 11, 140, 1) !important;
    }
    .navbar .nav-link,
    .navbar-brand {
        font-size: 16px !important;
        color: white !important;
    }
    body {
        height: 100vh;
        display: flex;
        flex-direction: column;
    }
    body.bslib-page-navbar > .container-fluid {
        flex: 1;
        min-height: 0;
        display: flex;
        flex-direction: column;
        padding-left: 0 !important;
        padding-right: 0 !important;
    }
    .tab-content {
        flex: 1;
        min-height: 0;
        padding: 0 !important;
        margin: 0 !important;
    }
    .tab-content > .tab-pane {
        height: 100%;
        padding: 0 !important;
        margin: 0 !important;
        display: flex;
        flex-direction: column;
    }
"""

app_ui = ui.page_navbar(
    application_page,

    title=ui.tags.span(
        ui.img(src="img/sanavia_cream.png", height="40px", style="margin-right:5px;"),
        "",
    ),
    padding=0,
    navbar_options=ui.navbar_options(underline=False),
    footer=footer_section(),
    header=ui.tags.style(ui.HTML(_navbar_css)),
)


def server(input, output, session):

    # step 0 — "Click Me" clicked
    @reactive.effect
    @reactive.event(input.click_me1)
    def handle_click_me():
        ui.insert_ui(
            ui.div(
                ui.p("Thank you!", class_="text-muted"),
                ui.card(
                    ui.card_body(
                        ui.img(
                            src="img/data_loaded1.jpg",
                            style="width:100%; margin-bottom:8px;",
                        ),
                        ui.p(
                            "Lorem ipsum dolor",
                            ui.br(),
                            "Hello World!",
                            class_="fs-6",
                        ),
                    ),
                    ui.input_action_button(
                        "proceed_button", "Proceed", class_="btn-primary"
                    ),
                ),
                id="after_click_me",
            ),
            selector="#click_me1",
            where="afterEnd",
        )

    # step 1 — "Proceed" clicked
    @reactive.effect
    @reactive.event(input.proceed_button)
    def handle_proceed():
        col1 = ui.div(
            ui.div(
                ui.p("Column One", class_="h5 text-primary fw-bold"),
                ui.p("This is column 1.", class_="text-secondary"),
                id="insert_me1_div2",
            ),
            ui.div(
                ui.p(
                    """The reconstruction of electron density from complex
                     structure factors and its inversion, the computation
                     of complex structure factors from electron density,
                     are amongst the most fundamental and frequent tasks
                     in the course of crystallographic structure
                     determination.

                     Rupp, Bernhard. Biomolecular Crystallography: Principles,
                     Practice, and Application to Structural Biology (p. 439).
                     CRC Press""",
                    class_="text-primary",
                )
            ),
            ui.div(
                ui.card(
                    ui.card_header("My Card"),
                    ui.card_body(
                        ui.p(
                            """The reconstruction of electron density from complex
                             structure factors and its inversion, the computation
                             of complex structure factors from electron density,
                             are amongst the most fundamental and frequent tasks
                             in the course of crystallographic structure
                             determination.
                             In practice, the experimental structure factor
                             amplitudes and separately supplied phases from a phasing
                             experiment are needed—a consequence of phase
                             information being lost in the physical detection
                             of the diffracted photons, fittingly termed the phase problem
                             in crystallography. We will lay out in this short but important
                             chapter the mathematical principles of Fourier transforms as
                             far as they are needed to derive the equations used in
                             practical crystallography.
                             Rupp, Bernhard. Biomolecular Crystallography: Principles,
                             Practice, and Application to Structural Biology (p. 439).
                             CRC Press""",
                            class_="text-primary",
                        ),
                        ui.tags.b("Bold text here."),
                        ui.br(),
                        "You can also write strings directly.",
                    ),
                )
            ),
            ui.div(
                ui.input_action_button(
                    "column1_button1", "Column1 button", class_="btn-warning"
                )
            ),
            id="insert_me1_div1",
        )

        col2 = ui.div(
            ui.div(
                ui.p("Column Two (A)", class_="h5 text-primary fw-bold"),
                ui.p("This is column 2.", class_="text-secondary"),
                ui.div(
                    ui.tags.iframe(
                        src="docs/tlgs/2026-04-02_plate-events-fig.pdf",
                        width="100%",
                        height="100%",
                    ),
                    style="height: 340px;",
                ),
                ui.p(
                    "Each page corresponds to a single plate.",
                    class_="text-secondary",
                ),
                id="insert_me2_div2a",
                class_="row",
            ),
            ui.div(
                ui.div(
                    ui.div(
                        ui.p("Column Two (B)", class_="h5 text-primary fw-bold"),
                        ui.p("Some stuff here"),
                    ),
                    ui.div(
                        ui.div(
                            ui.p("Column Two (C)", class_="h5 text-secondary fw-bold"),
                            ui.p("Some stuff here"),
                        ),
                        class_="row",
                    ),
                    ui.div(
                        ui.layout_column_wrap(
                            ui.input_action_button(
                                "back2_click_me",
                                "Back to Click Me",
                                class_="btn-secondary",
                            ),
                            ui.input_action_button(
                                "download_report",
                                "Accept & Download Report",
                                class_="btn-warning",
                            ),
                        ),
                        class_="row",
                    ),
                    class_="row",
                ),
                id="insert_me2_div2b",
                class_="row",
            ),
            id="insert_me2_div1",
        )

        ui.insert_ui(
            ui.div(
                ui.div(
                    ui.div(col1, class_="col", id="main_contents_col1"),
                    ui.div(
                        ui.div(id="main_contents_col2_1"),
                        col2,
                        class_="col",
                        id="main_contents_col2",
                    ),
                    class_="row",
                ),
                id="main_contents1",
            ),
            selector="#main_contents",
            where="afterEnd",
        )


app = App(app_ui, server, static_assets=Path(__file__).parent / "www")
