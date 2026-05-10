from shiny import ui


def success_horiz_card(main_tp, tp, img_src, bn=None):
    """Horizontal card: image on left (25%), text + optional button on right (75%)."""
    right_content = [main_tp, tp]
    if bn is not None:
        right_content.append(bn)

    return ui.card(
        ui.div(
            ui.div(
                ui.img(
                    src=img_src,
                    style="width:100%; border-radius:6px;",
                ),
                class_="col-3",
            ),
            ui.div(
                *right_content,
                class_="col-9",
            ),
            class_="row g-0",
            style="height:110px; max-width:420px;",
        )
    )
