import numpy as np
import pandas as pd

from .transforms import xform
from .stats import points_in_polygon
from .generate_verts import generate_intact_verts, generate_singlet_verts, generate_viable_verts


def _col_idx(mat_cols, ch_name):
    return list(mat_cols).index(ch_name)


def auto_gate2_viability(mat: np.ndarray, mat_cols: list, ch: dict) -> dict:
    n = len(mat)
    imat = pd.DataFrame({"root": np.ones(n, dtype=bool)})

    # 1. intact
    xi = _col_idx(mat_cols, ch["intact"]["x_ch"])
    yi = _col_idx(mat_cols, ch["intact"]["y_ch"])
    pts = mat[:, [xi, yi]]
    intact_verts = generate_intact_verts(pts)
    sel_intact = points_in_polygon(intact_verts, pts)
    imat["/intact"] = sel_intact

    # 2. singlet (on intact events only)
    xi = _col_idx(mat_cols, ch["singlet"]["x_ch"])
    yi = _col_idx(mat_cols, ch["singlet"]["y_ch"])
    pts_s = mat[sel_intact][:, [xi, yi]]
    singlet_verts = generate_singlet_verts(pts_s)
    sel_singlet_sub = points_in_polygon(singlet_verts, pts_s)
    sel_singlet = np.zeros(n, dtype=bool)
    sel_singlet[np.where(sel_intact)[0]] = sel_singlet_sub
    imat["/intact/singlet"] = sel_singlet

    # 3. viable (on singlet events, xform applied to VL1-A)
    xi = _col_idx(mat_cols, ch["viable"]["x_ch"])
    yi = _col_idx(mat_cols, ch["viable"]["y_ch"])
    raw_x = mat[sel_singlet][:, xi]
    x_xf = xform(raw_x)
    pts_v = np.column_stack([x_xf, mat[sel_singlet][:, yi]])
    viable_verts = generate_viable_verts(pts_v)
    sel_viable_sub = points_in_polygon(viable_verts, pts_v)
    sel_viable = np.zeros(n, dtype=bool)
    sel_viable[np.where(sel_singlet)[0]] = sel_viable_sub
    imat["/intact/singlet/viable"] = sel_viable

    verts = {"intact": intact_verts, "singlet": singlet_verts, "viable": viable_verts}
    return {"mat": mat, "mat_cols": mat_cols, "imat": imat, "verts": verts, "ch": ch}


def compute_optimal_vertices(fcs_dict_refs: dict, default_ch: dict) -> dict:
    verts_hold = {}
    mat_hold = []

    last_mat_cols = None
    for sid, sample in fcs_dict_refs.items():
        mat = sample.get_events(source="raw")
        mat_cols = list(sample.pnn_labels)
        last_mat_cols = mat_cols
        try:
            res = auto_gate2_viability(mat, mat_cols, default_ch)
            verts_hold[sid] = res["verts"]
            mat_hold.append(mat)
        except Exception:
            pass

    if not verts_hold:
        raise RuntimeError("No reference samples could be gated.")

    optim_verts_list = {}

    for pop in ("intact", "singlet", "viable"):
        xp = np.array([verts_hold[k][pop][:, 0] for k in verts_hold])  # shape (n_refs, 4)
        yp = np.array([verts_hold[k][pop][:, 1] for k in verts_hold])

        if pop == "intact":
            ax = np.quantile(xp[:, 0], 0.25)
            ay = np.quantile(yp[:, 0], 0.25)
            bx = np.quantile(xp[:, 1], 0.99)
            by = ay
            if ax > 2e5:
                ax = 2e5
            if ay > 1e5:
                ay = 1e5
            if bx < 8e5:
                bx = 8e5
            cx = bx
            cy = np.quantile(yp[:, 2], 0.99)
            if cy < 8e5:
                cy = 8e5
            dx, dy = ax, cy

        elif pop == "singlet":
            ax = np.quantile(xp[:, 0], 0.25)
            ay = np.quantile(yp[:, 0], 0.25)
            bx = np.quantile(xp[:, 1], 0.75)
            by = np.quantile(yp[:, 1], 0.50)
            cx = bx
            cy = np.quantile(yp[:, 2], 0.75)
            dx = ax
            dy = np.quantile(yp[:, 3], 0.75)

        else:  # viable
            ax = np.quantile(xp[:, 0], 0.75)
            ay = np.quantile(yp[:, 0], 0.75)
            bx = np.quantile(xp[:, 1], 0.25)
            by = ay
            if ax > 0:
                ax = 0
            if bx > 3.5:
                bx = 3.5
            cx = bx
            cy = np.quantile(yp[:, 2], 0.25)
            dx, dy = ax, cy

        optim_verts_list[pop] = np.array([[ax, ay], [bx, by], [cx, cy], [dx, dy]])

    omat = np.vstack(mat_hold)
    if len(omat) > 10000:
        idx = np.random.choice(len(omat), 10000, replace=False)
        omat = omat[idx]

    return {
        "optim_verts_list": optim_verts_list,
        "verts_hold": verts_hold,
        "default_ch": default_ch,
        "omat": omat,
        "mat_cols": last_mat_cols,
    }
