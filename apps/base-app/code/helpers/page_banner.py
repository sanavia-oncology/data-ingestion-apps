from shiny import ui


def page_banner(title="Main page title"):
    return ui.div(
        ui.div(
            ui.h1(title, class_="display-5 fw-lighter"),
            class_="container",
        ),
        style=(
            "background-position: center 10%;"
            "background-size: cover;"
            "background: #31387d;"
            "padding: 15px 0;"
            "color: white;"
        ),
    )
