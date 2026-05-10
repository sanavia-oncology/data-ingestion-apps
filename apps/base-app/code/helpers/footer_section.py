from shiny import ui


def footer_section(text="© 2026 Sanavia Oncology Inc."):
    return ui.tags.footer(
        ui.div(
            text,
            class_="text-center text-muted p-2",
            style=(
                "position: fixed;"
                "bottom: 0;"
                "width: 100%;"
                "background-color: rgb(214, 209, 196);"
                "z-index: 9999;"
            ),
        )
    )
