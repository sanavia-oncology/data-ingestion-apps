from shiny import ui


def footer_section(text="© 2026 Sanavia Oncology Inc."):
    return ui.div(
        ui.p(text, class_="text-center text-muted"),
        style=(
            "position: fixed; bottom: 0; width: 100%; z-index: 9999;"
            "background-color: rgb(214, 209, 196); padding: 4px 0;"
        ),
    )
