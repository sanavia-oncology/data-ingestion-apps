import numpy as np
import pandas as pd

from .transforms import xform
from .stats import points_in_polygon


def _col_idx(mat_cols, ch_name):
    return list(mat_cols).index(ch_name)


def gate2_viability(mat: np.ndarray, mat_cols: list, ch: dict, verts: dict) -> dict:
    n = len(mat)
    imat = pd.DataFrame({"root": np.ones(n, dtype=bool)})

    # 1. intact
    xi = _col_idx(mat_cols, ch["intact"]["x_ch"])
    yi = _col_idx(mat_cols, ch["intact"]["y_ch"])
    pts = mat[:, [xi, yi]]
    sel_intact = points_in_polygon(verts["intact"], pts)
    imat["/intact"] = sel_intact

    # 2. singlet (on intact events)
    xi = _col_idx(mat_cols, ch["singlet"]["x_ch"])
    yi = _col_idx(mat_cols, ch["singlet"]["y_ch"])
    pts_s = mat[sel_intact][:, [xi, yi]]
    sel_singlet_sub = points_in_polygon(verts["singlet"], pts_s)
    sel_singlet = np.zeros(n, dtype=bool)
    sel_singlet[np.where(sel_intact)[0]] = sel_singlet_sub
    imat["/intact/singlet"] = sel_singlet

    # 3. viable (on singlet events, xform on VL1-A)
    xi = _col_idx(mat_cols, ch["viable"]["x_ch"])
    yi = _col_idx(mat_cols, ch["viable"]["y_ch"])
    x_xf = xform(mat[sel_singlet][:, xi])
    pts_v = np.column_stack([x_xf, mat[sel_singlet][:, yi]])
    sel_viable_sub = points_in_polygon(verts["viable"], pts_v)
    sel_viable = np.zeros(n, dtype=bool)
    sel_viable[np.where(sel_singlet)[0]] = sel_viable_sub
    imat["/intact/singlet/viable"] = sel_viable

    return {"mat": mat, "mat_cols": mat_cols, "imat": imat, "verts": verts, "ch": ch}
