from shiny import ui


def qc_table(checks_summary, show_rownames=True):
    """Render interactive Pass/Fail QC table with clickable cells."""
    # checks_summary: dict[col_name -> dict[qc_name -> "Pass"/"Fail"]]
    # rows = plates, cols = QC checks  (same shape as R checks_summary)
    # We transpose: rows = plates, cols = QC codes

    plates = list(checks_summary.keys())
    if not plates:
        return ui.div()

    qc_names = list(checks_summary[plates[0]].keys())
    n_qc = len(qc_names)

    # Build QC column codes: QC_00 for single col, QC_01..QC_11 otherwise
    if n_qc == 1:
        qc_codes = ["QC_00"]
    else:
        qc_codes = [f"QC_{i:02d}" for i in range(1, n_qc + 1)]

    cell_style = (
        "width:45px; height:25px; border:1px solid #f0efefff;"
        "vertical-align:middle; text-align:center; margin:0;"
    )
    btn_base = (
        "width:45px; height:25px; font-size:11px;"
        "border-radius:0px; border:0px solid #ffffff;"
    )
    font_size = "font-size:10px;"

    # Header row
    header_cells = [ui.tags.th("")]
    for code in qc_codes:
        header_cells.append(
            ui.tags.th(code, style=f"text-align:center; {font_size}")
        )
    header_row = ui.tags.tr(*header_cells)

    # Data rows (one per plate)
    body_rows = []
    for plate in plates:
        cells = []
        if show_rownames:
            cells.append(
                ui.tags.th(plate, style=f"width:50px; text-align:right; {font_size}")
            )
        else:
            cells.append(ui.tags.th(""))

        for qc_name, code in zip(qc_names, qc_codes):
            result = checks_summary[plate][qc_name]
            color = "#51b551ff" if result == "Pass" else "#e54c4cff"
            cell_id = f"{plate}-{code}"
            btn = ui.tags.button(
                result,
                style=f"{btn_base} background-color:{color};",
                class_="cell-btn",
                id=cell_id,
                onclick="sendButtonID(this.id)",
            )
            cells.append(ui.tags.td(btn, style=cell_style))

        body_rows.append(ui.tags.tr(*cells))

    return ui.div(
        ui.tags.table(
            ui.tags.thead(header_row),
            ui.tags.tbody(*body_rows),
        )
    )
