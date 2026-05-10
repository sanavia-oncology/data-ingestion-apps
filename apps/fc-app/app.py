from pathlib import Path
from datetime import date
import numpy as np
import pandas as pd

from shiny import App, reactive, render, ui
from shiny.render import DataGrid

from code.pages.application_page import application_page
from code.helpers.footer_section import footer_section
from code.helpers.success_horiz_card import success_horiz_card

from code.data.readers import read_data
from code.gating.params import DEFAULT_CH
from code.gating.auto_gate import compute_optimal_vertices
from code.gating.apply_gate import gate2_viability
from code.summary.stats import summary_events, make_results_table
from code.summary.plots import (
    events_boxplot, plot_plate_events, plot_mfi,
    plot_optimal_vertices, sample_profile_plot,
)

APP_ROOT = Path(__file__).parent
TLG_DIR = APP_ROOT / "www" / "docs" / "tlgs"
TLG_DIR.mkdir(parents=True, exist_ok=True)

_NAV_CSS = """
.navbar { background-color: rgba(0,11,140,1) !important; }
.navbar .nav-link, .navbar-brand { font-size: 16px !important; color: white !important; }
body { height: 100vh; display: flex; flex-direction: column; }
body.bslib-page-navbar > .container-fluid {
    flex: 1; min-height: 0; display: flex; flex-direction: column;
    padding-left: 0 !important; padding-right: 0 !important;
}
.tab-content { flex: 1; min-height: 0; padding: 0 !important; margin: 0 !important; }
"""

app_ui = ui.page_navbar(
    application_page(),
    title=ui.img(src="img/sanavia_cream.png", height="40px"),
    padding=0,
    navbar_options=ui.navbar_options(underline=False),
    footer=footer_section(),
    header=ui.tags.style(ui.HTML(_NAV_CSS)),
    id="navbar",
)


def server(input, output, session):
    _data = reactive.Value(None)
    _gate_state = reactive.Value(None)
    _gated = reactive.Value(None)
    _annot = reactive.Value({})

    # ── Step 1: Read data ─────────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.read_data)
    def _handle_read_data():
        ui.remove_ui("#loaded_files_div")
        ui.remove_ui("#main_contents1")

        path = input.data_path().strip()
        try:
            res = read_data(path)
        except Exception as e:
            ui.insert_ui(
                ui.div(
                    ui.div(ui.p(f"Error: {e}", class_="text-danger"), id="loaded_files_div"),
                ),
                selector="#main_contents",
                where="afterEnd",
            )
            return

        _data.set(res)

        pinfos = res["pinfos"]
        n_plates = len(pinfos["Platename"].unique())
        n_fcs = len(res["fcs_dict"])
        mfi_ch = res["mfi_channel"] or "not found"

        plate_msg = f"{n_plates} plate{'s' if n_plates != 1 else ''}"
        fcs_msg = f"{n_fcs} FCS file{'s' if n_fcs != 1 else ''}"

        card = success_horiz_card(
            ui.p("Files detected!", class_="fw-bold text-success fs-5"),
            ui.p(f"{plate_msg} · {fcs_msg} · MFI channel: {mfi_ch}", class_="text-muted small"),
            img_src="img/data_loaded2.jpg",
            bn=ui.input_action_button("proceed", "Proceed", class_="btn-primary btn-sm"),
        )

        ui.insert_ui(
            ui.div(card, id="loaded_files_div"),
            selector="#main_contents",
            where="afterEnd",
        )

    # ── Step 2: Event summary ─────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.proceed)
    def _handle_proceed():
        ui.remove_ui("#main_contents1")

        res = _data.get()
        pinfos = res["pinfos"]
        today = date.today()

        events_fig_path = str(TLG_DIR / f"{today}_plate-events-fig.pdf")
        plot_plate_events(pinfos, is_gated=False, fig_path=events_fig_path)

        @output
        @render.plot(height=110)
        def plot_events_box():
            return events_boxplot(pinfos["Result"].values)

        @output
        @render.text
        def stat_summary_box():
            return summary_events(pinfos["Result"].values, "Total Events")

        col1 = ui.div(
            ui.p("Optimal Viability Gate Detection", class_="h5 text-primary fw-bold"),
            ui.p("Computationally determine optimal gates for all samples up to viability.",
                 class_="text-secondary"),
            ui.input_action_button("automatic_gate", "Automatically Gate up to Viability",
                                   class_="btn-warning"),
            id="ref_sample_gating_div",
        )

        col2 = ui.div(
            ui.p("Total Number of Events (Not Gated)", class_="h5 text-secondary fw-bold"),
            ui.div(
                ui.tags.iframe(
                    src=f"docs/tlgs/{today}_plate-events-fig.pdf",
                    width="100%", height="100%",
                ),
                style="height:340px;",
            ),
            ui.p("Each page corresponds to a single plate.", class_="text-secondary"),
            ui.div(ui.output_plot("plot_events_box")),
            ui.div(ui.output_text_verbatim("stat_summary_box"), style="width:100%;"),
            id="gating_summary_div",
        )

        ui.insert_ui(
            ui.div(
                ui.div(
                    ui.div(col1, class_="col"),
                    ui.div(col2, class_="col"),
                    class_="row",
                ),
                id="main_contents1",
            ),
            selector="#main_contents",
            where="afterEnd",
        )

    # ── Step 3: Compute optimal gates ─────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.automatic_gate)
    def _handle_auto_gate():
        res = _data.get()
        pinfos = res["pinfos"]
        fcs_dict = res["fcs_dict"]
        mfi_channel = res["mfi_channel"]

        ch = {k: dict(v) for k, v in DEFAULT_CH.items()}
        if mfi_channel:
            ch["ab+"]["x_ch"] = mfi_channel

        # Select up to 5 reference samples per plate
        ref_keys = []
        for plate in pinfos["Platename"].unique():
            plate_keys = [k for k in fcs_dict if k.startswith(plate + "|")]
            ref_keys.extend(plate_keys[:5])

        refs = {k: fcs_dict[k] for k in ref_keys if k in fcs_dict}

        opt_res = compute_optimal_vertices(refs, ch)
        _gate_state.set({**opt_res, "ch": ch})

        @output
        @render.plot(height=225)
        def plot_vertices():
            return plot_optimal_vertices(_gate_state.get())

        ui.insert_ui(
            ui.div(
                ui.p("Computation complete!", class_="text-success"),
                ui.p("Inspect automatic gates", class_="h5 text-primary"),
                ui.p(
                    "The sample below is a random sampling of events for qualitative "
                    "assessment of the automatic gates (not a real sample).",
                    class_="text-secondary",
                ),
                ui.output_plot("plot_vertices"),
                ui.br(),
                ui.p("Gate all samples", class_="h5 text-primary"),
                ui.p("Apply the gates above to all samples in the project.",
                     class_="text-secondary"),
                ui.input_action_button("gate_all", "Apply the Gates to All Samples",
                                       class_="btn-secondary"),
                id="auto_viability_gate_results",
            ),
            selector="#ref_sample_gating_div",
            where="afterEnd",
        )

    # ── Step 4: Gate all samples ──────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.gate_all)
    def _handle_gate_all():
        ui.remove_ui("#main_contents1")

        res = _data.get()
        pinfos = res["pinfos"]
        fcs_dict = res["fcs_dict"]
        gate_st = _gate_state.get()
        verts = gate_st["optim_verts_list"]
        ch = gate_st["ch"]

        gres_list = {}
        for sid, sample in fcs_dict.items():
            mat = sample.get_events(source="raw")
            mat_cols = list(sample.pnn_labels)
            try:
                gres_list[sid] = gate2_viability(mat, mat_cols, ch, verts)
            except Exception:
                gres_list[sid] = None

        results_table = make_results_table(gres_list, pinfos)
        _gated.set({"results_table": results_table, "gres_list": gres_list})

        today = date.today()
        viable_fig_path = str(TLG_DIR / f"{today}_plate-viable-events-fig.pdf")
        mfi_fig_path = str(TLG_DIR / f"{today}_plate-viable-mfi-fig.pdf")
        plot_plate_events(results_table, is_gated=True, fig_path=viable_fig_path)
        plot_mfi(results_table, fig_path=mfi_fig_path)

        @output
        @render.plot(height=110)
        def plot_viable_box():
            rt = _gated.get()["results_table"]
            return events_boxplot(rt["Result"].values, rt["/intact/singlet/viable"].values)

        @output
        @render.text
        def stat_viable_box():
            rt = _gated.get()["results_table"]
            total_str = summary_events(rt["Result"].values, "Total Events")
            viable_str = summary_events(rt["/intact/singlet/viable"].values, "Viable Events")
            return total_str + "\n\n" + viable_str

        col1 = ui.div(
            ui.p("Results: Log10 MFI of Viable Cells", class_="h5 text-primary fw-bold"),
            ui.div(
                ui.tags.iframe(src=f"docs/tlgs/{today}_plate-viable-mfi-fig.pdf",
                               width="100%", height="100%"),
                style="height:340px;",
            ),
            ui.p("Each page corresponds to a single plate.", class_="text-secondary"),
            ui.br(),
            ui.p("Review and Annotate Samples", class_="h5 text-secondary"),
            ui.p("Review results sample by sample and annotate for downstream analysis.",
                 class_="text-secondary"),
            ui.input_action_button("review_board", "Review and Annotate Samples",
                                   class_="btn-secondary"),
        )

        col2 = ui.div(
            ui.p("Total Number of Viable Cells", class_="h5 text-primary fw-bold"),
            ui.div(
                ui.tags.iframe(src=f"docs/tlgs/{today}_plate-viable-events-fig.pdf",
                               width="100%", height="100%"),
                style="height:340px;",
            ),
            ui.p("Each page = single plate (black=total, blue=viable).", class_="text-secondary"),
            ui.output_plot("plot_viable_box"),
            ui.div(ui.output_text_verbatim("stat_viable_box"), style="width:100%;"),
        )

        ui.insert_ui(
            ui.div(
                ui.div(
                    ui.div(col1, class_="col"),
                    ui.div(col2, class_="col"),
                    class_="row",
                ),
                id="main_contents1",
            ),
            selector="#main_contents",
            where="afterEnd",
        )

    # ── Step 5: Annotation page ───────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.review_board)
    def _handle_review_board():
        ui.remove_ui("#main_contents1")
        _annot.set({})

        rt = _gated.get()["results_table"].copy()
        display_cols = ["Platename", "Plate Position", "Primary Role",
                        "/intact/singlet/viable", "Cells Neg", "Log10 MFI"]
        disp = rt[display_cols].copy()
        disp["Log10 MFI"] = disp["Log10 MFI"].map(lambda x: f"{x:.3f}" if pd.notna(x) else "NA")
        disp["Cells Neg"] = disp["Cells Neg"].map(lambda x: f"{x*100:.2f}" if pd.notna(x) else "NA")
        disp = disp.rename(columns={
            "/intact/singlet/viable": "Viable",
            "Plate Position": "Well",
            "Primary Role": "Role",
        })
        disp["plate_row"] = disp["Well"].str[0]
        disp["plate_col"] = disp["Well"].str[1:]
        disp.index = rt.index

        plates = sorted(disp["Platename"].unique().tolist())
        rows = sorted(disp["plate_row"].unique().tolist())
        cols = sorted(disp["plate_col"].unique().tolist(), key=lambda x: int(x))

        @reactive.calc
        def _filtered_df():
            d = disp.copy()
            if input.filter_platename() != "All":
                d = d[d["Platename"] == input.filter_platename()]
            if input.filter_row() != "All":
                d = d[d["plate_row"] == input.filter_row()]
            if input.filter_col() != "All":
                d = d[d["plate_col"] == input.filter_col()]
            return d

        @output
        @render.data_frame
        def results_df():
            d = _filtered_df()
            show = [c for c in ["Platename", "Well", "Role", "Viable", "Cells Neg", "Log10 MFI"]
                    if c in d.columns]
            return DataGrid(d[show].reset_index(drop=True), selection_mode="row")

        @output
        @render.text
        def samples_noted():
            annot = _annot.get()
            all_sids = list(rt.index)
            flags = []
            for sid in all_sids:
                raw = annot.get(sid, "Keep|None")
                flags.append(raw.split("|")[0])
            from collections import Counter
            cnt = Counter(flags)
            lines = ["Samples noted:"]
            for flag in ("Keep", "Drop", "Warn"):
                lines.append(f"  {flag}: {cnt.get(flag, 0)}")
            return "\n".join(lines)

        col1 = ui.div(
            ui.p("Click on Row to Select Sample", class_="h5 text-primary fw-bold"),
            ui.div(
                ui.div(
                    ui.input_select("filter_platename", "Platename",
                                    choices=["All"] + plates, width="100%"),
                    class_="col-6",
                ),
                ui.div(
                    ui.input_select("filter_row", "Row",
                                    choices=["All"] + rows, width="100%"),
                    class_="col-3",
                ),
                ui.div(
                    ui.input_select("filter_col", "Col",
                                    choices=["All"] + cols, width="100%"),
                    class_="col-3",
                ),
                class_="row",
            ),
            ui.output_data_frame("results_df"),
            id="annotation_table_div1",
        )

        col2 = ui.div(
            ui.div(id="plot_sample_profile_div"),
            ui.p("Annotation Form", class_="h5 text-primary fw-bold"),
            ui.p("If necessary make changes and submit.", class_="text-secondary"),
            ui.div(
                ui.div(
                    ui.input_radio_buttons("drop_radio", "Flag",
                                           choices={"Keep": "Keep", "Drop": "Drop", "Warn": "Warn"}),
                    class_="col-3",
                ),
                ui.div(
                    ui.input_select("reason_select", "Reason",
                                    choices={"None": "None", "Low Ab Conc": "Low Ab Conc",
                                             "Tech Dup": "Tech Dup", "Other": "Other"}),
                    class_="col-4",
                ),
                ui.div(
                    ui.output_text_verbatim("samples_noted"),
                    class_="col-5",
                ),
                class_="row",
            ),
            ui.input_action_button("annotate_smpl", "Submit notes", class_="btn-secondary"),
            ui.br(), ui.br(),
            ui.p("Save results and exit", class_="h6 text-secondary"),
            ui.input_action_button("save_exit", "Download Results & Exit App",
                                   class_="btn-warning"),
            ui.div(id="exit_div"),
            id="annot_form_div",
        )

        ui.insert_ui(
            ui.div(
                ui.div(
                    ui.div(col1, class_="col", id="annotation_table_div"),
                    ui.div(col2, class_="col"),
                    class_="row",
                ),
                id="main_contents1",
            ),
            selector="#main_contents",
            where="afterEnd",
        )

    # ── Step 6: Sample profile on row selection ────────────────────────────────
    @reactive.effect
    @reactive.event(input.results_df_selected_rows)
    def _handle_row_select():
        ui.remove_ui("#plot_sample_profile_div_inner")

        sel_rows = input.results_df_selected_rows()
        if not sel_rows:
            return

        gated = _gated.get()
        rt = gated["results_table"]
        gres_list = gated["gres_list"]

        # Map filtered row index back to original SID
        disp_index = list(rt.index)
        sel_idx = sel_rows[0]
        if sel_idx >= len(disp_index):
            return
        k = disp_index[sel_idx]

        tsi = rt.at[k, "Target Spec ID"] if "Target Spec ID" in rt.columns else "?"
        pi = rt.at[k, "Probe ID"] if "Probe ID" in rt.columns else "?"

        @output
        @render.plot(height=180)
        def plot_sample_profile():
            return sample_profile_plot(k, gres_list)

        ui.insert_ui(
            ui.div(
                ui.card(
                    ui.p(f"Target Spec ID = {tsi} | Probe ID = {pi}", class_="small text-muted"),
                    ui.output_plot("plot_sample_profile"),
                ),
                id="plot_sample_profile_div_inner",
            ),
            selector="#plot_sample_profile_div",
            where="afterEnd",
        )

    # ── Step 7: Annotate sample ───────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.annotate_smpl)
    def _handle_annotate():
        sel_rows = input.results_df_selected_rows()
        if not sel_rows:
            return

        rt = _gated.get()["results_table"]
        disp_index = list(rt.index)
        sel_idx = sel_rows[0]
        if sel_idx >= len(disp_index):
            return
        k = disp_index[sel_idx]

        flag = input.drop_radio()
        reason = input.reason_select()
        annot = dict(_annot.get())

        if flag != "Keep":
            annot[k] = f"{flag}|{reason}"
        elif k in annot:
            del annot[k]

        _annot.set(annot)

    # ── Step 8: Save and exit ─────────────────────────────────────────────────
    @reactive.effect
    @reactive.event(input.save_exit)
    def _handle_save_exit():
        ui.remove_ui("#annotation_table_div1")
        ui.remove_ui("#annot_form_div")

        gated = _gated.get()
        res = _data.get()
        rt = gated["results_table"].copy()
        annot = _annot.get()

        to_drop = {sid: "Keep|None" for sid in rt.index}
        to_drop.update(annot)
        rt["to_drop"] = [to_drop.get(sid, "Keep|None") for sid in rt.index]

        gating_path = Path(res["data_path"]) / "gating_results"
        gating_path.mkdir(exist_ok=True)
        out_path = gating_path / f"{date.today()}-gated-results.csv"
        rt.to_csv(str(out_path), index=False)

        ui.insert_ui(
            ui.div(
                ui.card(
                    ui.p("Results saved!", class_="fw-bold text-success"),
                    ui.p(f"File: {out_path}", class_="small text-muted"),
                    ui.p("Close the browser tab to exit.", class_="text-secondary"),
                ),
            ),
            selector="#exit_div",
            where="afterEnd",
        )


app = App(app_ui, server, static_assets=APP_ROOT / "www")
