import numpy as np
from scipy.stats import norm, iqr
from statsmodels.nonparametric.smoothers_lowess import lowess

from .stats import fit_bivariate_normal, fit_bivariate_normal_robust


def generate_intact_verts(pts: np.ndarray) -> np.ndarray:
    try:
        fit = fit_bivariate_normal_robust(pts, trim=0.8)
        probs = [0.05, 0.95]
        qx = norm.ppf(probs, loc=fit["mu"][0], scale=fit["sd"][0])
        qy = norm.ppf(probs, loc=fit["mu"][1], scale=fit["sd"][1])
        qx = np.clip(qx, 0, None)
        qy = np.clip(qy, 0, None)
        verts = np.array([
            [qx[0], qy[0]],
            [qx[1], qy[0]],
            [qx[1], qy[1]],
            [qx[0], qy[1]],
        ])
    except Exception:
        verts = np.array([
            [107422.7,      0.0],
            [882256.2,      0.0],
            [882256.2, 627644.9],
            [107422.7, 627644.9],
        ])
    return verts


def generate_singlet_verts(pts: np.ndarray) -> np.ndarray:
    try:
        smoothed = lowess(pts[:, 0], pts[:, 1], frac=0.3)
        hold_x = smoothed[:, 1]
        hold_y = smoothed[:, 0]

        fit = fit_bivariate_normal(np.column_stack([hold_x, hold_y]))
        ex, ey = fit["mu"]
        sr = np.sqrt(fit["sigma"][1, 1]) / np.sqrt(fit["sigma"][0, 0])
        rho = fit["rho"]

        def mfit(x):
            return ey + rho * sr * (x - ex)

        a1 = pts[:, 0].min()
        a2 = mfit(a1)
        b1 = norm.ppf(0.999, loc=fit["mu"][0], scale=fit["sd"][0])
        b2 = mfit(b1)
        shift = iqr(pts[:, 1] - mfit(pts[:, 0])) * 1.5

        verts = np.array([
            [a1, a2 - shift],
            [b1, b2 - shift],
            [b1, b2 + shift],
            [a1, a2 + shift],
        ])
    except Exception:
        verts = np.array([
            [108079.0,  62823.83],
            [720052.9, 380813.59],
            [720052.9, 481123.20],
            [108079.0, 163133.43],
        ])
    return verts


def generate_viable_verts(pts: np.ndarray) -> np.ndarray:
    try:
        x = pts[:, 0]
        mx = x.mean()
        sdx = x.std(ddof=1)
        a1, b1 = mx - 2.25 * sdx, mx + 1.25 * sdx
        verts = np.array([
            [a1,   0.0],
            [b1,   0.0],
            [b1, 8e5],
            [a1, 8e5],
        ])
    except Exception:
        verts = np.array([
            [1.360837, 0.0],
            [3.511222, 0.0],
            [3.511222, 8e5],
            [1.360837, 8e5],
        ])
    return verts
