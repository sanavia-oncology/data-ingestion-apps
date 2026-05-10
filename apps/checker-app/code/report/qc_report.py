from datetime import date
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.backends.backend_pdf import PdfPages


def _capitalize(s: str) -> str:
    return s.capitalize() if s else s


def _plot_96well(ax, canvas, colors, text_colors, title=""):
    rows = list("ABCDEFGH")
    ax.set_xlim(0, 12)
    ax.set_ylim(0, 8)
    ax.set_aspect("equal")
    ax.set_title(title, fontsize=8, pad=10)
    ax.axis("off")

    for ri, row in enumerate(rows):
        for ci in range(12):
            y = 7 - ri
            x = ci
            color = colors[ri][ci] or "white"
            text = str(canvas[ri][ci]) if canvas[ri][ci] is not None else "NA"
            text_col = text_colors[ri][ci] or "gray"

            rect = patches.Rectangle((x, y), 1, 1, linewidth=0.5,
                                      edgecolor="white", facecolor=color)
            ax.add_patch(rect)
            ax.text(x + 0.5, y + 0.5, text, ha="center", va="center",
                    fontsize=5, color=text_col)

    for i in range(13):
        ax.axvline(i, color="white", lw=0.5)
    for i in range(9):
        ax.axhline(i, color="white", lw=0.5)

    for ri, r in enumerate(rows):
        ax.text(-0.3, 7 - ri + 0.5, r, ha="right", va="center", fontsize=6)
    for ci in range(12):
        ax.text(ci + 0.5, 8.2, str(ci + 1), ha="center", va="bottom", fontsize=6)


def _pinfo_to_plate_grid(pinfo_df, value_col, value_map, color_map):
    canvas = [[None] * 12 for _ in range(8)]
    colors = [["white"] * 12 for _ in range(8)]
    text_colors = [["gray"] * 12 for _ in range(8)]
    rows = list("ABCDEFGH")

    for _, row in pinfo_df.iterrows():
        pos = str(row["Plate Position"])
        if len(pos) < 2:
            continue
        ri = rows.index(pos[0]) if pos[0] in rows else None
        try:
            ci = int(pos[1:]) - 1
        except ValueError:
            ci = None
        if ri is None or ci is None or ci < 0 or ci > 11:
            continue
        val = row[value_col]
        mapped = value_map.get(val, "?")
        canvas[ri][ci] = mapped
        colors[ri][ci] = color_map.get(mapped, "lightgray")
        text_colors[ri][ci] = "black" if color_map.get(mapped) in ("skyblue", "white", None) else "white"

    return canvas, colors, text_colors


# ── Cover sheet ──────────────────────────────────────────────────────────────

def cover_sheet_plot(pdf, pinfo_sheets, approved=False):
    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    ax.axis("off")

    creator = list(pinfo_sheets.values())[0]["Creator"].iloc[0]
    parts = creator.split(".")
    name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator

    ax.text(0.5, 0.85, "Semantic QC Report", ha="center", va="center",
            fontsize=18, fontweight="bold", transform=ax.transAxes)
    ax.text(0.5, 0.55, name, ha="center", va="center",
            fontsize=12, transform=ax.transAxes)
    ax.text(0.5, 0.45, f"Report Date: {date.today()}", ha="center", va="center",
            fontsize=10, transform=ax.transAxes)

    status_text = "Reviewed and approved" if approved else "Please review carefully for any logical errors"
    status_color = "seagreen" if approved else "gray"
    ax.text(0.5, 0.15, status_text, ha="center", va="center",
            fontsize=10, color=status_color, transform=ax.transAxes)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Role plots ───────────────────────────────────────────────────────────────

_ROLE_COLORS = {
    "SMPL": "skyblue", "NEG": "red", "POS": "blue",
    "REF": "gray", "STD": "forestgreen", "OTHER": "orange",
}

def _role_key_for(pinfo_sheets):
    combo_set = set()
    for df in pinfo_sheets.values():
        r1 = df["Primary Role"].astype(str)
        r2 = df["Subrole"].fillna("").astype(str)
        combo_set.update(f"{a}|{b}" for a, b in zip(r1, r2))

    mapping = {
        "sample|sample": "SMPL", "sample|": "SMPL",
        "control|negative": "NEG", "control|positive": "POS",
        "control|reference": "REF", "standard|sample": "STD", "standard|": "STD",
    }
    key = {}
    for combo in sorted(combo_set):
        key[combo] = mapping.get(combo, "OTHER")
    return key


def role_plot_cover(pdf, pinfo_sheets, approved=False):
    key = _role_key_for(pinfo_sheets)
    fig, axes = plt.subplots(1, 2, figsize=(6.5, 4.25))

    ax = axes[0]
    ax.axis("off")
    ax.text(0.5, 0.9, "Primary & Subrole", ha="center", va="center",
            fontsize=14, fontweight="bold", transform=ax.transAxes)
    status = "Reviewed and approved" if approved else "Please review carefully for any logical errors"
    ax.text(0.5, 0.75, status, ha="center", va="center",
            fontsize=8, color="seagreen" if approved else "gray", transform=ax.transAxes)

    df0 = list(pinfo_sheets.values())[0]
    probe_type = _capitalize(str(df0["Probe Type"].iloc[0]))
    spec_type = _capitalize(str(df0["Target Spec Type"].iloc[0]))
    ax.text(0.5, 0.55, f"Probe Type: {probe_type}", ha="center", fontsize=10, transform=ax.transAxes)
    ax.text(0.5, 0.40, f"Target Spec Type: {spec_type}", ha="center", fontsize=10, transform=ax.transAxes)

    ax2 = axes[1]
    ax2.axis("off")
    rows_data = []
    for combo, abbr in key.items():
        role_label = {"SMPL": "Sample", "NEG": "Negative control", "POS": "Positive control",
                      "REF": "Reference", "STD": "Standard"}.get(abbr, "Other")
        rows_data.append([abbr, role_label])

    if rows_data:
        tbl = ax2.table(cellText=rows_data, colLabels=["Fig ID", "Role Type"],
                        loc="center", cellLoc="left")
        tbl.auto_set_font_size(False)
        tbl.set_fontsize(9)
        tbl.scale(1, 1.3)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


def role_plot(pdf, plate_name, pinfo_df, pinfo_sheets, approved=False):
    key = _role_key_for(pinfo_sheets)

    def get_combo(row):
        r1 = str(row["Primary Role"])
        r2 = str(row["Subrole"]) if pd.notna(row["Subrole"]) and row["Subrole"] != "" else ""
        c = f"{r1}|{r2}"
        return key.get(c, "OTHER")

    pinfo_df = pinfo_df.copy()
    pinfo_df["_abbr"] = pinfo_df.apply(get_combo, axis=1)

    canvas, colors, text_colors = _pinfo_to_plate_grid(
        pinfo_df, "_abbr", {v: v for v in _ROLE_COLORS}, _ROLE_COLORS
    )

    platename = pinfo_df["Platename"].iloc[0]
    creator = pinfo_df["Creator"].iloc[0]
    parts = str(creator).split(".")
    creator_name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator
    if approved:
        creator_name += " (reviewed and approved)"

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    fig.subplots_adjust(top=0.82, bottom=0.22)
    _plot_96well(ax, canvas, colors, text_colors,
                 title=f"{plate_name} | {platename}\nRole Type")
    fig.text(0.1, 0.92, f"{creator_name}\nAnalysis date: {date.today()}",
             fontsize=6, color="gray", ha="left", va="top")

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Probe ID plots ────────────────────────────────────────────────────────────

def probe_id_plot_cover(pdf, pinfo_sheets, approved=False):
    all_ids = sorted({str(v) for df in pinfo_sheets.values()
                      for v in df["Probe ID"].dropna()})
    id_map = {pid: f"{i+1:03d}" for i, pid in enumerate(all_ids)}

    fig, axes = plt.subplots(1, 2, figsize=(6.5, 4.25))
    ax = axes[0]
    ax.axis("off")
    ax.text(0.5, 0.9, "Probe ID", ha="center", fontsize=14, fontweight="bold", transform=ax.transAxes)
    status = "Reviewed and approved" if approved else "Please review carefully for any logical errors"
    ax.text(0.5, 0.75, status, ha="center", fontsize=8,
            color="seagreen" if approved else "gray", transform=ax.transAxes)
    probe_type = _capitalize(str(list(pinfo_sheets.values())[0]["Probe Type"].iloc[0]))
    ax.text(0.5, 0.55, f"Probe Type: {probe_type}", ha="center", fontsize=10, transform=ax.transAxes)

    ax2 = axes[1]
    ax2.axis("off")
    show = list(id_map.items())[:10]
    rows_data = [[v, k] for k, v in show]
    tbl = ax2.table(cellText=rows_data, colLabels=["Fig ID", "Probe ID"],
                    loc="center", cellLoc="left")
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(8)
    tbl.scale(1, 1.2)
    ax2.text(0.05, 0.92, f"{len(all_ids)} probes used in project.",
             transform=ax2.transAxes, fontsize=8)
    if len(all_ids) > 10:
        ax2.text(0.05, 0.05, "List too long. Showing first 10.",
                 transform=ax2.transAxes, fontsize=7)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


def probe_id_plot(pdf, plate_name, pinfo_df, pinfo_sheets, approved=False):
    all_ids = sorted({str(v) for df in pinfo_sheets.values()
                      for v in df["Probe ID"].dropna()})
    import matplotlib.cm as cm
    cmap = cm.get_cmap("Spectral", max(len(all_ids), 1))
    id_to_num = {pid: f"{i+1:03d}" for i, pid in enumerate(all_ids)}
    id_to_color = {pid: matplotlib.colors.to_hex(cmap(i / max(len(all_ids)-1, 1)))
                   for i, pid in enumerate(all_ids)}

    canvas, colors, text_colors = _pinfo_to_plate_grid(
        pinfo_df, "Probe ID",
        {pid: id_to_num[pid] for pid in all_ids},
        {id_to_num[pid]: id_to_color[pid] for pid in all_ids},
    )
    for r in range(8):
        for c in range(12):
            text_colors[r][c] = "black" if canvas[r][c] not in (None, "NA") else "gray"

    platename = pinfo_df["Platename"].iloc[0]
    probe_type = _capitalize(str(pinfo_df["Probe Type"].iloc[0]))
    creator = pinfo_df["Creator"].iloc[0]
    parts = str(creator).split(".")
    creator_name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    fig.subplots_adjust(top=0.82, bottom=0.22)
    _plot_96well(ax, canvas, colors, text_colors,
                 title=f"{plate_name} | {platename}\nProbe Type: {probe_type}")
    fig.text(0.1, 0.92, f"{creator_name}\nAnalysis date: {date.today()}",
             fontsize=6, color="gray", ha="left", va="top")

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Probe Quant plots ─────────────────────────────────────────────────────────

def probe_quant_plot_cover(pdf, pinfo_sheets, approved=False):
    all_vals = pd.concat([df["Probe Quant Value"] for df in pinfo_sheets.values()])
    numeric = pd.to_numeric(all_vals, errors="coerce").dropna()

    fig, axes = plt.subplots(1, 2, figsize=(6.5, 4.25))
    ax = axes[0]
    ax.axis("off")
    ax.text(0.5, 0.9, "Probe Amount", ha="center", fontsize=14, fontweight="bold", transform=ax.transAxes)
    status = "Reviewed and approved" if approved else "Please review carefully for any logical errors"
    ax.text(0.5, 0.75, status, ha="center", fontsize=8,
            color="seagreen" if approved else "gray", transform=ax.transAxes)
    df0 = list(pinfo_sheets.values())[0]
    probe_type = _capitalize(str(df0["Probe Type"].iloc[0]))
    pqt = _capitalize(str(df0["Probe Quant Type"].iloc[0]))
    ax.text(0.5, 0.55, f"Probe Type: {probe_type}", ha="center", fontsize=10, transform=ax.transAxes)
    ax.text(0.5, 0.40, f"Probe Quant Type: {pqt}", ha="center", fontsize=10, transform=ax.transAxes)

    ax2 = axes[1]
    ax2.axis("off")
    if len(numeric) > 0:
        stats = numeric.describe()
        rows_data = [[n, f"{v:.3f}"] for n, v in stats.items()]
        tbl = ax2.table(cellText=rows_data, colLabels=["Stat name", "Stat value"],
                        loc="center", cellLoc="left")
        tbl.auto_set_font_size(False)
        tbl.set_fontsize(9)
        tbl.scale(1, 1.3)
    ax2.text(0.05, 0.92, "Statistical summary of probe amount.", transform=ax2.transAxes, fontsize=8)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


def probe_quant_plot(pdf, plate_name, pinfo_df, pinfo_sheets, approved=False):
    all_vals = pd.concat([df["Probe Quant Value"] for df in pinfo_sheets.values()])
    all_numeric = pd.to_numeric(all_vals, errors="coerce").dropna().unique()
    all_numeric.sort()

    n = len(all_numeric)
    cmap = plt.cm.Blues
    val_to_color = {}
    for i, v in enumerate(all_numeric):
        val_to_color[v] = matplotlib.colors.to_hex(cmap(0.2 + 0.7 * i / max(n - 1, 1)))

    canvas = [[None] * 12 for _ in range(8)]
    colors = [["white"] * 12 for _ in range(8)]
    text_colors = [["gray"] * 12 for _ in range(8)]
    rows = list("ABCDEFGH")

    for _, row in pinfo_df.iterrows():
        pos = str(row["Plate Position"])
        if len(pos) < 2:
            continue
        ri = rows.index(pos[0]) if pos[0] in rows else None
        try:
            ci = int(pos[1:]) - 1
        except ValueError:
            ci = None
        if ri is None or ci is None or ci < 0 or ci > 11:
            continue
        try:
            v = float(row["Probe Quant Value"])
            closest = all_numeric[np.argmin(np.abs(all_numeric - v))]
            colors[ri][ci] = val_to_color[closest]
            canvas[ri][ci] = str(v)
            text_colors[ri][ci] = "black"
        except (ValueError, TypeError):
            pass

    for r in range(8):
        for c in range(12):
            if canvas[r][c] is None:
                canvas[r][c] = "NA"

    platename = pinfo_df["Platename"].iloc[0]
    creator = pinfo_df["Creator"].iloc[0]
    parts = str(creator).split(".")
    creator_name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    fig.subplots_adjust(top=0.82, bottom=0.22)
    _plot_96well(ax, canvas, colors, text_colors,
                 title=f"{plate_name} | {platename}")
    fig.text(0.1, 0.92, f"{creator_name}\nAnalysis Date: {date.today()}",
             fontsize=6, color="gray", ha="left", va="top")

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Specimen ID plots ─────────────────────────────────────────────────────────

def spec_plot_cover(pdf, pinfo_sheets, approved=False):
    all_ids = sorted({str(v) for df in pinfo_sheets.values()
                      for v in df["Target Spec ID"].dropna()})

    fig, axes = plt.subplots(1, 2, figsize=(6.5, 4.25))
    ax = axes[0]
    ax.axis("off")
    ax.text(0.5, 0.9, "Target Specimen", ha="center", fontsize=14, fontweight="bold", transform=ax.transAxes)
    status = "Reviewed and approved" if approved else "Please review carefully for any logical errors"
    ax.text(0.5, 0.75, status, ha="center", fontsize=8,
            color="seagreen" if approved else "gray", transform=ax.transAxes)
    spec_type = _capitalize(str(list(pinfo_sheets.values())[0]["Target Spec Type"].iloc[0]))
    ax.text(0.5, 0.55, f"Specimen Type: {spec_type}", ha="center", fontsize=10, transform=ax.transAxes)

    ax2 = axes[1]
    ax2.axis("off")
    show = [(f"CT-{i+1:02d}", sid) for i, sid in enumerate(all_ids[:10])]
    tbl = ax2.table(cellText=show, colLabels=["Fig ID", "Cell Type Name"],
                    loc="center", cellLoc="left")
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(8)
    tbl.scale(1, 1.2)
    ax2.text(0.05, 0.92, f"{len(all_ids)} target specimen types used in project.",
             transform=ax2.transAxes, fontsize=8)
    if len(all_ids) > 10:
        ax2.text(0.05, 0.05, "List too long. Showing first 10.",
                 transform=ax2.transAxes, fontsize=7)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


def spec_plot(pdf, plate_name, pinfo_df, pinfo_sheets, approved=False):
    all_ids = sorted({str(v) for df in pinfo_sheets.values()
                      for v in df["Target Spec ID"].dropna()})
    import matplotlib.cm as cm
    cmap = cm.get_cmap("Dark2", max(len(all_ids), 1))
    id_to_label = {sid: f"CT-{i+1:02d}" for i, sid in enumerate(all_ids)}
    id_to_color = {sid: matplotlib.colors.to_hex(cmap(i / max(len(all_ids)-1, 1)))
                   for i, sid in enumerate(all_ids)}

    canvas, colors, text_colors = _pinfo_to_plate_grid(
        pinfo_df, "Target Spec ID",
        {sid: id_to_label[sid] for sid in all_ids},
        {id_to_label[sid]: id_to_color[sid] for sid in all_ids},
    )
    for r in range(8):
        for c in range(12):
            text_colors[r][c] = "white" if canvas[r][c] not in (None, "NA") else "gray"

    platename = pinfo_df["Platename"].iloc[0]
    spec_type = _capitalize(str(pinfo_df["Target Spec Type"].iloc[0]))
    creator = pinfo_df["Creator"].iloc[0]
    parts = str(creator).split(".")
    creator_name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    fig.subplots_adjust(top=0.82, bottom=0.22)
    _plot_96well(ax, canvas, colors, text_colors,
                 title=f"{plate_name} | {platename}\nTarget Specimen: {spec_type} Type")
    fig.text(0.1, 0.92, f"{creator_name}\nAnalysis date: {date.today()}",
             fontsize=6, color="gray", ha="left", va="top")

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Assay data plots ──────────────────────────────────────────────────────────

def assay_data_plot_cover(pdf, merge_info, approved=False):
    first = list(merge_info.values())[0]["merged_data"]
    dt = first["Assay Data Type"].iloc[0] if "Assay Data Type" in first.columns else ""
    rt = first["Result Type"].iloc[0] if "Result Type" in first.columns else ""

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    ax.axis("off")
    ax.text(0.5, 0.85, "Assay Data", ha="center", fontsize=14, fontweight="bold", transform=ax.transAxes)
    status = "Reviewed & approved" if approved else "Please review carefully for any logical errors"
    ax.text(0.5, 0.70, status, ha="center", fontsize=9,
            color="seagreen" if approved else "gray", transform=ax.transAxes)
    ax.text(0.5, 0.50, f"Assay Data Type: {dt}", ha="center", fontsize=10, transform=ax.transAxes)
    ax.text(0.5, 0.35, f"Result Type: {rt}", ha="center", fontsize=10, transform=ax.transAxes)

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


def assay_data_plot(pdf, plate_name, merged_df, merge_info, approved=False):
    all_results = pd.concat(
        [mi["merged_data"]["Result"] for mi in merge_info.values() if mi["merged_data"] is not None],
        ignore_index=True,
    )
    numeric_all = pd.to_numeric(all_results, errors="coerce").dropna()
    min_v = float(numeric_all.min()) if len(numeric_all) else 0
    max_v = float(numeric_all.max()) if len(numeric_all) else 1

    cmap = plt.cm.RdYlBu_r
    canvas = [[None] * 12 for _ in range(8)]
    colors = [["white"] * 12 for _ in range(8)]
    text_colors = [["gray"] * 12 for _ in range(8)]
    rows = list("ABCDEFGH")

    for _, row in merged_df.iterrows():
        pos = str(row["Plate Position"])
        if len(pos) < 2:
            continue
        ri = rows.index(pos[0]) if pos[0] in rows else None
        try:
            ci = int(pos[1:]) - 1
        except ValueError:
            ci = None
        if ri is None or ci is None or ci < 0 or ci > 11:
            continue
        try:
            v = float(row["Result"])
            norm = (v - min_v) / (max_v - min_v + 1e-10)
            colors[ri][ci] = matplotlib.colors.to_hex(cmap(norm))
            canvas[ri][ci] = str(int(v)) if v == int(v) else f"{v:.1f}"
            text_colors[ri][ci] = "black"
        except (ValueError, TypeError):
            pass

    for r in range(8):
        for c in range(12):
            if canvas[r][c] is None:
                canvas[r][c] = "NA"

    platename = merged_df["Platename"].iloc[0]
    creator = merged_df["Creator"].iloc[0]
    parts = str(creator).split(".")
    creator_name = f"{parts[0].capitalize()} {parts[1].capitalize()}" if len(parts) >= 2 else creator
    if approved:
        creator_name += " (Reviewed and approved)"
    result_type = merged_df["Result Type"].iloc[0] if "Result Type" in merged_df.columns else ""
    analysis_type = merged_df["Analysis Type"].iloc[0] if "Analysis Type" in merged_df.columns else ""

    fig, ax = plt.subplots(figsize=(6.5, 4.25))
    fig.subplots_adjust(top=0.82, bottom=0.22)
    _plot_96well(ax, canvas, colors, text_colors,
                 title=f"{plate_name} | {platename}\n{analysis_type} ({result_type})")
    fig.text(0.1, 0.92, f"{creator_name}\nAnalysis Date: {date.today()}",
             fontsize=6, color="gray", ha="left", va="top")

    pdf.savefig(fig, bbox_inches="tight")
    plt.close(fig)


# ── Orchestration ─────────────────────────────────────────────────────────────

def make_qc_report(pinfo_sheets: dict, merge_info=None, fig_path: str = None, approved=False):
    if fig_path is None:
        raise ValueError("fig_path is required")

    with PdfPages(fig_path) as pdf:
        cover_sheet_plot(pdf, pinfo_sheets, approved=approved)

        role_plot_cover(pdf, pinfo_sheets, approved=approved)
        for plate_name, df in pinfo_sheets.items():
            role_plot(pdf, plate_name, df, pinfo_sheets, approved=approved)

        probe_id_plot_cover(pdf, pinfo_sheets, approved=approved)
        for plate_name, df in pinfo_sheets.items():
            probe_id_plot(pdf, plate_name, df, pinfo_sheets, approved=approved)

        probe_quant_plot_cover(pdf, pinfo_sheets, approved=approved)
        for plate_name, df in pinfo_sheets.items():
            probe_quant_plot(pdf, plate_name, df, pinfo_sheets, approved=approved)

        spec_plot_cover(pdf, pinfo_sheets, approved=approved)
        for plate_name, df in pinfo_sheets.items():
            spec_plot(pdf, plate_name, df, pinfo_sheets, approved=approved)

        if merge_info is not None:
            assay_data_plot_cover(pdf, merge_info, approved=approved)
            for plate_name, mi in merge_info.items():
                if mi["merged_data"] is not None:
                    assay_data_plot(pdf, plate_name, mi["merged_data"], merge_info, approved=approved)


def approved_qc_report(data_path: str, pinfo_sheets: dict, merge_info):
    report_path = Path(data_path.rstrip("/")) / "qc_report"
    report_path.mkdir(exist_ok=True)

    for old_file in report_path.iterdir():
        old_file.unlink()

    fig_path = str(report_path / f"{date.today()}-qc-report.pdf")
    make_qc_report(pinfo_sheets, merge_info, fig_path, approved=True)

    if merge_info is None:
        frames = list(pinfo_sheets.values())
        merged_data = pd.concat(frames, ignore_index=True)
        merged_data["PINFO_QC_DATE"] = date.today()
    else:
        frames = [mi["merged_data"] for mi in merge_info.values() if mi["merged_data"] is not None]
        merged_data = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

    table_path = report_path / f"{date.today()}-merged-data.csv"
    merged_data.to_csv(table_path, index=False)
