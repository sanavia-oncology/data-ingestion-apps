import numpy as np
from scipy.stats import spearmanr


def fit_bivariate_normal(pts: np.ndarray) -> dict:
    pts = np.asarray(pts, dtype=float)
    n = len(pts)
    mu = pts.mean(axis=0)
    sigma = np.cov(pts.T) * (n - 1) / n
    sd = np.sqrt(np.diag(sigma))
    rho = sigma[0, 1] / (sd[0] * sd[1]) if (sd[0] > 0 and sd[1] > 0) else 0.0
    return {"mu": mu, "sigma": sigma, "rho": rho, "sd": sd}


def fit_bivariate_normal_robust(pts: np.ndarray, trim: float = 0.90) -> dict:
    pts = np.asarray(pts, dtype=float)

    mu0 = np.median(pts, axis=0)
    sds0 = np.median(np.abs(pts - mu0), axis=0) / 0.6745
    rho0 = spearmanr(pts[:, 0], pts[:, 1]).statistic
    sigma0 = np.array([
        [sds0[0] ** 2,          rho0 * sds0[0] * sds0[1]],
        [rho0 * sds0[0] * sds0[1], sds0[1] ** 2],
    ])

    try:
        sigma0_inv = np.linalg.inv(sigma0)
        diff = pts - mu0
        d2 = np.einsum("ij,jk,ik->i", diff, sigma0_inv, diff)
    except np.linalg.LinAlgError:
        d2 = np.zeros(len(pts))

    cut = np.quantile(d2, trim)
    trimmed = pts[d2 <= cut]

    return fit_bivariate_normal(trimmed)


def points_in_polygon(poly: np.ndarray, pts: np.ndarray) -> np.ndarray:
    poly = np.asarray(poly, dtype=float)
    pts = np.asarray(pts, dtype=float)
    n_verts = len(poly)
    inside = np.zeros(len(pts), dtype=bool)
    j = n_verts - 1
    with np.errstate(divide="ignore", invalid="ignore"):
        for i in range(n_verts):
            xi, yi = poly[i, 0], poly[i, 1]
            xj, yj = poly[j, 0], poly[j, 1]
            crosses = ((yi > pts[:, 1]) != (yj > pts[:, 1])) & (
                pts[:, 0] < (xj - xi) * (pts[:, 1] - yi) / (yj - yi) + xi
            )
            inside ^= crosses
            j = i
    return inside
