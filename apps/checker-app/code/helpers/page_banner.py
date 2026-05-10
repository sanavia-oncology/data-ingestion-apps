from shiny import ui


def page_banner(title="Main page title"):
    return ui.div(
        ui.h1(title, class_="display-5 fw-light text-white text-center"),
        style="background-color: #31387d; padding: 6px 0;",
    )
