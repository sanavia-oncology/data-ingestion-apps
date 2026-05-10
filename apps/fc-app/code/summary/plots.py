from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.backends.backend_pdf import PdfPages

from ..gating.transforms import xform


# ── helpers ───────────────────────────────────────────────────────────────────

def _well_order(well: str) -> tuple:
    """Sort key: (col_num, row_letter) so A1 < A2 < ... < H12."""
    row = well[0]
    col = int(well[1:])
    return (col, row)


def _draw_polygon(ax, verts, **kw):
    from matplotlib.patches import Polygon as MPoly
    patch = MPoly(verts, closed=True, fill=False, **kw)
    ax.add_patch(patch)


# ── events boxplot ─────────────────────────────────────────────────────────────

def events_boxplot(total_events, viable_events=None) -> plt.Figure:
    fig, ax = plt.subplots(figsize=(5, 1.8))
    total = np.asarray(total_events, dtype=float)

    bp = ax.boxplot(total, vert=False, patch_artist=True,
                    boxprops=dict(facecolor="lightgray"),
                    medianprops=dict(color="black"),
                    flierprops=dict(marker="o", markersize=3, alpha=0.4))

    if viable_events is not None:
        viable = np.asarray(viable_events, dtype=float)
        ax.boxplot(viable, vert=False, patch_artist=True,
                   boxprops=dict(facecolor="steelblue", alpha=0.5),
                   medianprops=dict(color="navy"),
                   flierprops=dict(marker="o", markersize=3, alpha=0.4))

    for ref in (2500, 5000, 10000):
        ax.axvline(ref, color="red", linestyle="--", linewidth=0.8, alpha=0.6)

    ax.set_yticks([])
    ax.set_xlabel("Event count")
    ax.set_title("Event counts" + (" (gray=total, blue=viable)" if viable_events is not None else ""),
                 fontsize=9)
    fig.tight_layout()
    return fig


# ── plate events PDF ──────────────────────────────────────────────────────────

def plot_plate_events(df: pd.DataFrame, is_gated: bool, fig_path: str):
    Path(fig_path).parent.mkdir(parents=True, exist_ok=True)
    plates = df["Platename"].unique()

    with PdfPages(fig_path) as pdf:
        for plate in plates:
            sub = df[df["Platename"] == plate].copy()
            wells = sub["Plate Position"].tolist()
            wells_sorted = sorted(wells, key=_well_order)
            x_pos = np.arange(len(wells_sorted))
            well_to_x = {w: i for i, w in enumerate(wells_sorted)}

            fig, ax = plt.subplots(figsize=(10, 3.5))
            total = sub.set_index("Plate Position")["Result"].reindex(wells_sorted).values

            ax.scatter(x_pos, total, color="black", s=20, label="Total", zorder=3)

            if is_gated and "/intact/singlet/viable" in sub.columns:
                viable = sub.set_index("Plate Position")["/intact/singlet/viable"].reindex(wells_sorted).values
                ax.scatter(x_pos, viable, color="steelblue", s=20, label="Viable", zorder=4)

            for ref, color in [(2500, "red"), (5000, "orange"), (10000, "green")]:
                ax.axhline(ref, linestyle="--", linewidth=0.8, color=color, alpha=0.6)

            ax.set_xticks(x_pos)
            ax.set_xticklabels(wells_sorted, rotation=90, fontsize=6)
            ax.set_title(plate, fontsize=11)
            ax.set_ylabel("Events")
            if is_gated:
                ax.legend(fontsize=8)
            fig.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)


# ── MFI PDF ───────────────────────────────────────────────────────────────────

def plot_mfi(results_table: pd.DataFrame, fig_path: str):
    Path(fig_path).parent.mkdir(parents=True, exist_ok=True)
    plates = results_table["Platename"].unique()

    with PdfPages(fig_path) as pdf:
        for plate in plates:
            sub = results_table[results_table["Platename"] == plate].copy()
            wells = sub["Plate Position"].tolist()
            wells_sorted = sorted(wells, key=_well_order)
            x_pos = np.arange(len(wells_sorted))

            mfi = sub.set_index("Plate Position")["Log10 MFI"].reindex(wells_sorted).values
            neg_pct = sub.set_index("Plate Position")["Cells Neg"].reindex(wells_sorted).values * 100

            fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 5), sharex=True)

            ax1.scatter(x_pos, mfi, color="darkgreen", s=20, zorder=3)
            ax1.set_ylabel("Log10 MFI")
            ax1.set_title(plate, fontsize=11)

            ax2.scatter(x_pos, neg_pct, color="firebrick", s=20, zorder=3)
            ax2.axhline(20, linestyle="--", linewidth=0.8, color="gray", alpha=0.6)
            ax2.set_ylabel("Neg cells (%)")
            ax2.set_xticks(x_pos)
            ax2.set_xticklabels(wells_sorted, rotation=90, fontsize=6)

            fig.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)


# ── optimal vertices plot ─────────────────────────────────────────────────────

def plot_optimal_vertices(optimal_vertices_res: dict) -> plt.Figure:
    omat = optimal_vertices_res["omat"]
    mat_cols = optimal_vertices_res["mat_cols"]
    optim_verts = optimal_vertices_res["optim_verts_list"]
    verts_hold = optimal_vertices_res["verts_hold"]
    ch = optimal_vertices_res["default_ch"]

    def get_col(name):
        try:
            return mat_cols.index(name)
        except ValueError:
            return None

    pops = [
        ("intact",  ch["intact"]["x_ch"],  ch["intact"]["y_ch"],  "FSC-A", "SSC-A"),
        ("singlet", ch["singlet"]["x_ch"], ch["singlet"]["y_ch"], "FSC-A", "FSC-H"),
        ("viable",  ch["viable"]["x_ch"],  ch["viable"]["y_ch"],  "xform(VL1-A)", "SSC-A"),
    ]

    fig, axes = plt.subplots(1, 3, figsize=(12, 4))

    for ax, (pop, xch, ych, xlabel, ylabel) in zip(axes, pops):
        xi, yi = get_col(xch), get_col(ych)
        if xi is None or yi is None:
            ax.set_title(pop)
            continue

        x_vals = omat[:, xi]
        y_vals = omat[:, yi]
        if pop == "viable":
            x_vals = xform(x_vals)

        ax.scatter(x_vals, y_vals, s=1, alpha=0.2, color="gray", rasterized=True)

        for sid_verts in verts_hold.values():
            v = sid_verts[pop]
            v_closed = np.vstack([v, v[0]])
            ax.plot(v_closed[:, 0] if pop != "viable" else v_closed[:, 0],
                    v_closed[:, 1], color="red", linewidth=0.6, alpha=0.5)

        ov = optim_verts[pop]
        ov_closed = np.vstack([ov, ov[0]])
        ax.plot(ov_closed[:, 0], ov_closed[:, 1], color="black", linewidth=1.5)

        ax.set_xlabel(xlabel, fontsize=8)
        ax.set_ylabel(ylabel, fontsize=8)
        ax.set_title(pop.capitalize(), fontsize=10)

    fig.tight_layout()
    return fig


# ── sample profile plot ───────────────────────────────────────────────────────

def sample_profile_plot(k: str, gres_list: dict) -> plt.Figure:
    gres = gres_list.get(k)
    if gres is None:
        fig, ax = plt.subplots()
        ax.text(0.5, 0.5, "No data", ha="center", va="center")
        return fig

    mat = gres["mat"]
    mat_cols = gres["mat_cols"]
    imat = gres["imat"]
    ch = gres["ch"]
    verts = gres["verts"]

    def idx(name):
        try:
            return mat_cols.index(name)
        except ValueError:
            return None

    panels = [
        ("intact",  ch["intact"]["x_ch"],  ch["intact"]["y_ch"],  imat["root"],               "FSC-A", "SSC-A"),
        ("singlet", ch["singlet"]["x_ch"], ch["singlet"]["y_ch"], imat["/intact"],             "FSC-A", "FSC-H"),
        ("viable",  ch["viable"]["x_ch"],  ch["viable"]["y_ch"],  imat["/intact/singlet"],     "xform(VL1-A)", "SSC-A"),
        ("ab+",     ch["ab+"]["x_ch"],     ch["ab+"]["y_ch"],     imat["/intact/singlet/viable"], f"xform({ch['ab+']['x_ch']})", "SSC-A"),
    ]

    fig, axes = plt.subplots(1, 4, figsize=(14, 3.5))

    for ax, (pop, xch, ych, mask, xlabel, ylabel) in zip(axes, panels):
        if xch is None:
            ax.set_title(pop)
            continue
        xi, yi = idx(xch), idx(ych)
        if xi is None or yi is None:
            ax.set_title(pop)
            continue

        m = mask.values if hasattr(mask, "values") else mask
        x_vals = mat[m, xi]
        y_vals = mat[m, yi]

        if pop in ("viable", "ab+"):
            x_vals = xform(x_vals)

        n_plot = min(len(x_vals), 3000)
        if len(x_vals) > n_plot:
            sel = np.random.choice(len(x_vals), n_plot, replace=False)
            x_vals, y_vals = x_vals[sel], y_vals[sel]

        ax.scatter(x_vals, y_vals, s=1, alpha=0.3, color="steelblue", rasterized=True)

        if pop in verts:
            v = verts[pop]
            v_closed = np.vstack([v, v[0]])
            ax.plot(v_closed[:, 0], v_closed[:, 1], color="black", linewidth=1.2)

        ax.set_xlabel(xlabel, fontsize=7)
        ax.set_ylabel(ylabel, fontsize=7)
        ax.set_title(pop.capitalize(), fontsize=9)

    fig.tight_layout()
    return fig


# ── MFI boxplot ───────────────────────────────────────────────────────────────

def mfi_boxplot(log10_mfi_values) -> plt.Figure:
    vals = np.asarray(log10_mfi_values, dtype=float)
    vals = vals[~np.isnan(vals)]

    fig, ax = plt.subplots(figsize=(5, 2))
    x_jitter = np.random.uniform(-0.05, 0.05, len(vals))
    ax.scatter(vals, x_jitter, s=15, alpha=0.6, color="darkgreen")
    ax.axvline(np.median(vals), color="black", linestyle="--", linewidth=1)
    ax.set_xlabel("Log10 MFI")
    ax.set_yticks([])
    ax.set_title("MFI distribution", fontsize=9)
    fig.tight_layout()
    return fig
