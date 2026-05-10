# author: Kwame Okrah

from pathlib import Path
from shiny import App, reactive, ui, render

from code.helpers.footer_section import footer_section
from code.helpers.qc_table import qc_table
from code.helpers.success_horiz_card import success_horiz_card
from code.pages.application_page import application_page
from code.pages.related_sops_page import related_sops_page

from code.data.file_utils import get_file_paths, read_pinfo_csvs, read_assay_data
from code.data.merge_utils import merge_assay_data
from code.qc.checks import perform_plateinfo_checks
from code.qc.error_messages import print_error_msg
from code.report.qc_report import make_qc_report, approved_qc_report

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

_QC_CODE = {
    "Required Columns": "QC_00",
    "Platename": "QC_01",
    "Plate Position": "QC_02",
    "Creator": "QC_03",
    "Target Spec Type": "QC_04",
    "Target Spec ID": "QC_05",
    "Probe Type": "QC_06",
    "Probe ID": "QC_07",
    "Probe Quant Type": "QC_08",
    "Probe Quant Value": "QC_09",
    "Primary Role": "QC_10",
    "Subrole": "QC_11",
    "DATA MERGE": "QC_12",
}
_QC_CODE_REV = {v: k for k, v in _QC_CODE.items()}

_QC_CODE_TABLE = ui.tags.table(
    ui.tags.tbody(
        ui.tags.tr(
            ui.tags.td("QC_01: Platename"),
            ui.tags.td("QC_05: Target Spec ID"),
            ui.tags.td("QC_09: Probe Quant Value"),
        ),
        ui.tags.tr(
            ui.tags.td("QC_02: Plate Position"),
            ui.tags.td("QC_06: Probe Type"),
            ui.tags.td("QC_10: Primary Role"),
        ),
        ui.tags.tr(
            ui.tags.td("QC_03: Creator"),
            ui.tags.td("QC_07: Probe ID"),
            ui.tags.td("QC_11: Subrole"),
        ),
        ui.tags.tr(
            ui.tags.td("QC_04: Target Spec Type"),
            ui.tags.td("QC_08: Probe Quant Type"),
            ui.tags.td("QC_12: DATA MERGE"),
        ),
    ),
    style="color:#343637ff; font-size:12px; border:1px solid #a7a9aaff; width:500px;",
)

app_ui = ui.page_navbar(
    application_page,
    related_sops_page,
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
    _input_data = reactive.Value(None)   # {"pinfo_sheets": ..., "assay_data": ...}
    _err_data = reactive.Value(None)     # checks_detailed, merge_info, etc.
    _msg_text = reactive.Value("Message here...")

    # ── Step 1: Read data ──────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.read_data)
    def handle_read_data():
        ui.remove_ui("#pass_fail_div")

        data_path = input.data_path().strip()
        selected_assay = input.selected_assay()

        file_paths = get_file_paths(data_path, selected_assay)

        if file_paths["dir_test"] == "fail":
            ui.insert_ui(
                ui.div(
                    ui.p("Path entered is not correct. Please check and try again.",
                         class_="text-danger"),
                    id="pass_fail_div",
                ),
                selector="#read_data",
                where="afterEnd",
            )
            return

        pinfo_paths = file_paths["pinfo_csv_paths"]
        assay_paths = file_paths["assay_file_paths"]

        if not pinfo_paths:
            ui.insert_ui(
                ui.div(
                    ui.p("No plate information sheets detected. Please check and try again.",
                         class_="text-danger"),
                    id="pass_fail_div",
                ),
                selector="#read_data",
                where="afterEnd",
            )
            return

        pinfo_sheets = read_pinfo_csvs(pinfo_paths)
        assay_data = read_assay_data(assay_paths, selected_assay) if assay_paths else None

        n_pinfo = len(pinfo_sheets)
        msg_pinfo = f"{n_pinfo} plate information {'sheet' if n_pinfo == 1 else 'sheets'}"

        if selected_assay == "none" or assay_data is None or assay_data.empty:
            n_assay = 0
            msg_assay = "No assay data files"
        else:
            n_assay = len(assay_paths)
            msg_assay = f"{n_assay} {selected_assay} {'file' if n_assay == 1 else 'files'}"

        _input_data.set({"pinfo_sheets": pinfo_sheets, "assay_data": assay_data})

        ui.insert_ui(
            ui.div(
                ui.p("Files detected!", class_="text-muted"),
                ui.card(
                    ui.img(src="img/data_loaded1.jpg", style="width:100%;"),
                    ui.card_body(
                        ui.p(msg_pinfo, ui.br(), msg_assay, class_="fs-6")
                    ),
                    ui.input_action_button(
                        "perform_checks", "Perform QC", class_="btn-primary"
                    ),
                ),
                id="pass_fail_div",
            ),
            selector="#read_data",
            where="afterEnd",
        )

    # ── Step 2: Perform QC checks ──────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.perform_checks)
    def handle_perform_checks():
        ui.remove_ui("#main_contents1")

        d = _input_data.get()
        pinfo_sheets = d["pinfo_sheets"]
        assay_data = d["assay_data"]

        res = perform_plateinfo_checks(pinfo_sheets)
        checks_summary = res["checks_summary"]
        checks_detailed = res["checks_detailed"]

        # Rename plate keys to PLATE_01, PLATE_02, ...
        key = {orig: f"PLATE_{i+1:02d}" for i, orig in enumerate(pinfo_sheets)}
        rev_key = {v: k for k, v in key.items()}

        pinfo_sheets_keyed = {key[k]: v for k, v in pinfo_sheets.items()}
        checks_summary_keyed = {key[k]: v for k, v in checks_summary.items()}
        checks_detailed_keyed = {key[k]: v for k, v in checks_detailed.items()}

        merge_info = None
        assay_position_check_summary = None
        assay_position_check_detailed = None

        if assay_data is not None and not assay_data.empty:
            mres = merge_assay_data(assay_data, pinfo_sheets)
            merge_info_raw = mres["merge_info"]
            assay_position_check_summary = mres["assay_position_check_summary"]
            assay_position_check_detailed = mres["assay_position_check_detailed"]

            merge_info = {key[k]: v for k, v in merge_info_raw.items()}

            mcheck = {k: all(v["merge_checks"].values()) for k, v in merge_info.items()}
            for k, passed in mcheck.items():
                checks_summary_keyed[k]["DATA MERGE"] = "Pass" if passed else "Fail"

        _err_data.set({
            "checks_detailed": checks_detailed_keyed,
            "merge_info": merge_info,
            "pinfo_sheets": pinfo_sheets_keyed,
            "assay_position_check_summary": assay_position_check_summary,
            "assay_position_check_detailed": assay_position_check_detailed,
            "rev_key": rev_key,
        })

        # Determine display condition
        req_col_results = {k: v["Required Columns"] for k, v in checks_summary_keyed.items()}
        cond1 = any(r != "Pass" for r in req_col_results.values())

        all_other = [
            v for plate_summary in checks_summary_keyed.values()
            for qc_name, v in plate_summary.items()
            if qc_name != "Required Columns"
        ]
        cond2a = any(v != "Pass" for v in all_other)
        cond2b = all(v == "Pass" for v in all_other) and not cond1

        if cond1:
            summary_for_table = {k: {"Required Columns": v["Required Columns"]}
                                  for k, v in checks_summary_keyed.items()}
            main_tp = ui.p("Diabolical Error!", class_="h6 card-title text-danger",
                           style="margin-bottom:2px;")
            tp = ui.p("Please make sure all required columns exist; check for potential spelling mistakes including extra spaces.")
            img_src = "img/fun/angry-sana-cat1.png"

            insert_me1 = ui.div(
                ui.p("Required Columns Check", class_="h5 text-primary fw-bold"),
                ui.p(ui.tags.strong("Click"), " each cell to get a detailed error description message.",
                     class_="text-secondary"),
                ui.p("QC_00: Required Columns"),
                qc_table(summary_for_table),
                ui.br(),
                success_horiz_card(main_tp, tp, img_src),
            )

        else:
            summary_no_req = {
                k: {qc: v for qc, v in plate_summary.items() if qc != "Required Columns"}
                for k, plate_summary in checks_summary_keyed.items()
            }

            if cond2a:
                n_pass = sum(v == "Pass" for vals in summary_no_req.values() for v in vals.values())
                n_total = sum(len(vals) for vals in summary_no_req.values())
                pct = round(100 * n_pass / n_total) if n_total else 0
                main_tp = ui.p(f"{pct}% Success Rate", class_="h6 card-title text-danger",
                               style="margin-bottom:2px;")
                tp = ui.p("Almost there you can do it! Please review and fix each error.")
                img_src = "img/fun/waiting-sana-cat1.png"

                insert_me1 = ui.div(
                    ui.p("Syntax Quality Control Checks", class_="h5 text-primary fw-bold"),
                    ui.p(ui.tags.strong("Click"), " a cell to get a detailed error description message.",
                         class_="text-secondary"),
                    ui.card(qc_table(summary_no_req)),
                    "QC Code", _QC_CODE_TABLE,
                    ui.br(),
                    success_horiz_card(main_tp, tp, img_src),
                )

            else:  # cond2b
                main_tp = ui.p("100% success!", class_="h6 card-title text-primary",
                               style="margin-bottom:2px;")
                tp = ui.p("Review QC report for logical/semantic errors.")
                bn = ui.input_action_button("qc_report", "Go to Semantic QC Report",
                                            class_="btn-primary")
                img_src = "img/fun/happy-sana-cat1.png"

                fig_path = str(Path(__file__).parent / "www" / "docs" / "tlgs" / "qc-report.pdf")
                make_qc_report(pinfo_sheets_keyed, merge_info, fig_path, approved=False)

                insert_me1 = ui.div(
                    ui.p("Syntax Quality Control Checks", class_="h5 text-primary fw-bold"),
                    ui.p(ui.tags.strong("Click"), " a cell to get a detailed summary of the column contents.",
                         class_="text-secondary"),
                    ui.card(qc_table(summary_no_req)),
                    "QC Code", _QC_CODE_TABLE,
                    ui.br(),
                    success_horiz_card(main_tp, tp, img_src, bn),
                )

        insert_me2 = ui.div(
            ui.p("Message Box", class_="h5 text-secondary fw-bold"),
            ui.div(id="main_contents_col2_div"),
            ui.div(
                ui.p("None", class_="text-secondary"),
                ui.card(ui.output_text_verbatim("message_box", placeholder=True)),
                id="detailed_message_box",
            ),
            id="msg_box_div",
        )

        ui.insert_ui(
            ui.div(
                ui.div(
                    ui.div(insert_me1, class_="col", id="main_contents_col1"),
                    ui.div(
                        ui.div(id="main_contents_col2_topmark_div"),
                        insert_me2,
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

        @output
        @render.text
        def message_box():
            return _msg_text.get()

    # ── Step 3: Message box on cell click ──────────────────────────────────
    @reactive.effect
    @reactive.event(input.last_cell_clicked)
    def handle_cell_click():
        ui.remove_ui("#detailed_message_box")

        d = _err_data.get()
        if d is None:
            return

        cell_id = input.last_cell_clicked()
        parts = cell_id.split("-")
        if len(parts) < 2:
            return

        plate_nam = parts[0]
        qc_code = parts[1].replace("_", " ")  # e.g. "QC 01"
        qc_code_dash = parts[1]               # e.g. "QC_01"
        pinfo_col = _QC_CODE_REV.get(qc_code_dash, qc_code_dash)

        selected_cell_msg = f"{plate_nam} | {qc_code_dash}: {pinfo_col}"

        msg = print_error_msg(
            plate_nam=plate_nam,
            pinfo_col=pinfo_col,
            checks_detailed=d["checks_detailed"],
            merge_info=d["merge_info"],
            assay_position_check_summary=d["assay_position_check_summary"],
            assay_position_check_detailed=d["assay_position_check_detailed"],
        )
        _msg_text.set(msg)

        ui.insert_ui(
            ui.div(
                ui.p(selected_cell_msg, class_="text-primary"),
                ui.card(ui.output_text_verbatim("message_box", placeholder=True)),
                id="detailed_message_box",
            ),
            selector="#main_contents_col2_div",
            where="afterEnd",
        )

    # ── Step 4: Semantic QC Report viewer ─────────────────────────────────
    @reactive.effect
    @reactive.event(input.qc_report)
    def handle_qc_report():
        ui.remove_ui("#msg_box_div")

        selected_assay = input.selected_assay()

        if selected_assay == "fcs":
            action_btn = ui.input_action_button(
                "mfi_gating_ch", "Select Antibody MFI Channel", class_="btn-primary"
            )
            iframe_height = "435px"
        else:
            action_btn = ui.input_action_button(
                "download_report", "Accept & Download QC Report", class_="btn-warning"
            )
            iframe_height = "477px"

        insert_me = ui.div(
            ui.div(
                ui.p("Semantic QC Report", class_="h5 text-primary fw-bold"),
                ui.p(f"Analysis date: {__import__('datetime').date.today()}", class_="text-secondary"),
                ui.div(
                    ui.tags.iframe(src="docs/tlgs/qc-report.pdf", width="100%", height="100%"),
                    style=f"height:{iframe_height};",
                ),
                ui.p("Review and accept for semantic accuracy.", class_="text-secondary"),
                class_="row",
            ),
            ui.div(
                ui.layout_column_wrap(
                    ui.input_action_button("back2_msg", "Back to Message Box", class_="btn-secondary"),
                    action_btn,
                ),
                class_="row",
            ),
            id="qc_report_div",
        )

        ui.insert_ui(insert_me, selector="#main_contents_col2_topmark_div", where="afterEnd")

    # ── Back to message box ────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.back2_msg)
    def handle_back2_msg():
        ui.remove_ui("#qc_report_div")
        ui.insert_ui(
            ui.div(
                ui.p("Message Box", class_="h5 text-secondary fw-bold"),
                ui.div(id="main_contents_col2_div"),
                ui.div(
                    ui.p("None", class_="text-secondary"),
                    ui.card(ui.output_text_verbatim("message_box", placeholder=True)),
                    id="detailed_message_box",
                ),
                id="msg_box_div",
            ),
            selector="#main_contents_col2_topmark_div",
            where="afterEnd",
        )

    # ── MFI channel selection (FCS only) ───────────────────────────────────
    @reactive.effect
    @reactive.event(input.mfi_gating_ch)
    def handle_mfi_gating_ch():
        ui.remove_ui("#qc_report_div")

        mfi_channels = {
            "None": "None",
            "RL1-H": "<RL1-H> RL1-H", "YL1-H": "<YL1-H> YL1-H",
            "BL1-A": "<BL1-A> BL1-A", "BL2-A": "<BL2-A> BL2-A",
            "YL1-A": "<YL1-A> YL1-A", "YL2-A": "<YL2-A> YL2-A", "YL3-A": "<YL3-A> YL3-A",
            "RL1-A": "<RL1-A> RL1-A", "RL2-A": "<RL2-A> RL2-A", "RL3-A": "<RL3-A> RL3-A",
            "VL1-A": "<VL1-A> VL1-A", "VL2-A": "<VL2-A> VL2-A", "VL3-A": "<VL3-A> VL3-A",
            "VL4-A": "<VL4-A> VL4-A", "VL5-A": "<VL5-A> VL5-A", "VL6-A": "<VL6-A> VL6-A",
        }

        ui.insert_ui(
            ui.div(
                ui.p("Select MFI signal channel", class_="h5 text-primary fw-bold"),
                ui.div(id="main_contents_col2_div"),
                ui.p("Select the channel that should be used to compute MFI.",
                     class_="text-secondary"),
                ui.input_selectize("select_mfi_ch", "Select options below:",
                                   choices=mfi_channels),
                ui.output_text("mfi_ch_value"),
                ui.br(),
                ui.input_action_button("download_report", "Accept & Download QC Report",
                                       class_="btn-warning"),
                id="mfi_ch_box_div",
            ),
            selector="#main_contents_col2_topmark_div",
            where="afterEnd",
        )

        @output
        @render.text
        def mfi_ch_value():
            return input.select_mfi_ch() if hasattr(input, "select_mfi_ch") else ""

    # ── Step 6: Download / finalize ────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.download_report)
    def handle_download_report():
        ui.remove_ui("#qc_report_div")
        ui.remove_ui("#mfi_ch_box_div")

        d = _err_data.get()
        data_path = input.data_path().strip()
        pinfo_sheets = d["pinfo_sheets"]
        merge_info = d["merge_info"]

        approved_qc_report(data_path, pinfo_sheets, merge_info)

        selected_assay = input.selected_assay()
        mfi_ch = None
        try:
            mfi_ch = input.select_mfi_ch()
        except Exception:
            pass

        if selected_assay == "fcs":
            if mfi_ch and mfi_ch != "None":
                from pathlib import Path
                import pandas as pd
                from datetime import date as dt
                report_path = Path(data_path.rstrip("/")) / "qc_report"
                pd.DataFrame({"mfi_channel": [mfi_ch]}).to_csv(
                    report_path / f"{dt.today()}-mfi-channel.csv", index=False
                )
                insert_me = ui.div(
                    ui.p("Done", class_="h5 text-primary fw-bold"),
                    ui.div(id="main_contents_col2_div"),
                    ui.div(
                        ui.card(ui.p("An approved qc-report and a merged data file have been saved "
                                     "in your project folder. Please upload the approved project "
                                     "folder to the required location.", class_="text-secondary")),
                        ui.p("You may exit the app.", class_="text-secondary"),
                        id="done_message",
                    ),
                    id="msg_box_div",
                )
            else:
                insert_me = ui.div(
                    ui.p("MFI Channel Info. Missing", class_="h5 text-danger fw-bold"),
                    ui.div(id="main_contents_col2_div"),
                    ui.div(
                        ui.card(ui.p("Please go back and add the MFI channel.", class_="text-secondary")),
                        ui.input_action_button("qc_report", "Go Back", class_="btn-warning"),
                    ),
                    id="msg_box_div",
                )
        else:
            insert_me = ui.div(
                ui.p("Done", class_="h5 text-primary fw-bold"),
                ui.div(id="main_contents_col2_div"),
                ui.div(
                    ui.card(ui.p("An approved qc-report and a merged data file have been saved "
                                 "in your project folder. Please upload the approved project "
                                 "folder to the required location.", class_="text-secondary")),
                    ui.p("You may exit the app.", class_="text-secondary"),
                    id="done_message",
                ),
                id="msg_box_div",
            )

        ui.insert_ui(insert_me, selector="#main_contents_col2_topmark_div", where="afterEnd")

    # ── Re-read data clears results ────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.read_data)
    def clear_on_reread():
        ui.remove_ui("#main_contents1")


app = App(app_ui, server, static_assets=Path(__file__).parent / "www")
