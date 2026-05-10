from pathlib import Path
import pandas as pd
import flowkit as fk


def read_data(data_path: str) -> dict:
    p = Path(data_path.strip())
    if not p.is_dir():
        raise ValueError(f"Not a directory: {data_path}")

    # ── FCS files ─────────────────────────────────────────────────────────────
    fcs_paths = sorted(p.rglob("*.fcs"))
    if not fcs_paths:
        raise ValueError("No FCS files found.")

    fcs_raw = {}
    for path in fcs_paths:
        try:
            s = fk.Sample(str(path))
            meta = s.get_metadata()
            platename = meta.get("platename", meta.get("$PLATENAME", "")).strip()
            wellid = meta.get("wellid", meta.get("$WELLID", "")).strip()
            if not platename or not wellid:
                continue
            sid = f"{platename}|{wellid}"
            fcs_raw[sid] = s
        except Exception:
            continue

    if not fcs_raw:
        raise ValueError("Could not parse FCS metadata from any file.")

    # ── merged-data.csv ───────────────────────────────────────────────────────
    merged_paths = sorted(p.rglob("*merged-data.csv"))
    if not merged_paths:
        raise ValueError("merged-data.csv not found.")
    merged_data = pd.read_csv(str(merged_paths[-1]))  # use most recent if multiple

    # Build composite index matching FCS keys
    if "Platename" in merged_data.columns and "Plate Position" in merged_data.columns:
        merged_data.index = merged_data["Platename"] + "|" + merged_data["Plate Position"]
    else:
        raise ValueError("merged-data.csv missing 'Platename' or 'Plate Position' columns.")

    # ── mfi-channel.csv ───────────────────────────────────────────────────────
    mfi_paths = sorted(p.rglob("*mfi-channel.csv"))
    mfi_channel = None
    if mfi_paths:
        mfi_df = pd.read_csv(str(mfi_paths[0]))
        raw_ch = str(mfi_df.iloc[0, 0])
        # Strip "> " prefix if present (checker-app format)
        mfi_channel = raw_ch.split("> ")[-1].strip() if "> " in raw_ch else raw_ch.strip()

    merged_data["mfi_channel"] = mfi_channel

    # ── filter FCS dict to keys in merged_data ─────────────────────────────
    valid_keys = set(merged_data.index)
    fcs_dict = {k: v for k, v in fcs_raw.items() if k in valid_keys}

    # ── common channels ───────────────────────────────────────────────────────
    if fcs_dict:
        common_channels = set(list(fcs_dict.values())[0].pnn_labels)
        for s in fcs_dict.values():
            common_channels &= set(s.pnn_labels)
        common_channels = sorted(common_channels)
    else:
        common_channels = []

    return {
        "pinfos": merged_data,
        "fcs_dict": fcs_dict,
        "common_channels": common_channels,
        "mfi_channel": mfi_channel,
        "data_path": str(p),
    }
