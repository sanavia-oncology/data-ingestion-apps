from datetime import date
import numpy as np
import pandas as pd

from ..gating.transforms import xform


def summary_events(x: np.ndarray, name: str = "Total Events") -> str:
    x = np.asarray(x, dtype=float)
    bins = [
        ("< 2,500",       np.sum(x < 2500)),
        ("2,500 – 5,000", np.sum((x >= 2500) & (x < 5000))),
        ("5,000 – 10,000",np.sum((x >= 5000) & (x < 10000))),
        ("> 10,000",      np.sum(x >= 10000)),
    ]
    lines = [f"Summary: {name}", "-" * 30]
    lines += [f"  {label:<20} {count:>5}" for label, count in bins]
    lines += [
        "-" * 30,
        f"  {'Min':<20} {int(np.min(x)):>5}",
        f"  {'Median':<20} {int(np.median(x)):>5}",
        f"  {'Max':<20} {int(np.max(x)):>5}",
    ]
    return "\n".join(lines)


def gating_summary(gres: dict) -> dict:
    if gres is None:
        return {k: np.nan for k in [
            "Log10 MFI", "Std Dev", "Cells Neg",
            "root", "/intact", "/intact/singlet", "/intact/singlet/viable"
        ]}

    imat = gres["imat"]
    mat = gres["mat"]
    mat_cols = gres["mat_cols"]
    ch = gres["ch"]

    count_events = {col: int(imat[col].sum()) for col in imat.columns}

    ab_ch = ch["ab+"]["x_ch"]
    ab_idx = mat_cols.index(ab_ch)
    viable_mask = imat["/intact/singlet/viable"].values
    ab_vals = xform(mat[viable_mask, ab_idx])

    neg_mask = ab_vals < 0
    pos_vals = ab_vals[~neg_mask]

    log10_mfi = float(np.mean(pos_vals)) if len(pos_vals) > 0 else np.nan
    std_dev = float(np.std(pos_vals, ddof=1)) if len(pos_vals) > 1 else np.nan
    cells_neg = float(np.mean(neg_mask)) if len(ab_vals) > 0 else np.nan

    return {
        "Log10 MFI": log10_mfi,
        "Std Dev": std_dev,
        "Cells Neg": cells_neg,
        **count_events,
    }


def make_results_table(gres_list: dict, pinfos: pd.DataFrame) -> pd.DataFrame:
    records = []
    for sid, gres in gres_list.items():
        row = gating_summary(gres)
        parts = sid.split("|", 1)
        row["Platename"] = parts[0] if len(parts) > 0 else ""
        row["Plate Position"] = parts[1] if len(parts) > 1 else ""
        row["Gating Date"] = date.today()
        row["_sid"] = sid
        records.append(row)

    stat_df = pd.DataFrame(records).set_index("_sid")
    stat_df = stat_df.drop(columns=["Platename", "Plate Position"], errors="ignore")

    result = pinfos.join(stat_df, how="left")
    return result
