from pathlib import Path
import pandas as pd


def get_file_paths(data_path: str, selected_assay: str) -> dict:
    res = {"dir_test": "", "pinfo_csv_paths": [], "assay_file_paths": []}

    p = Path(data_path.strip())
    if not p.is_dir():
        res["dir_test"] = "fail"
        return res

    res["dir_test"] = "pass"

    pinfo_dir = p / "plate_information_sheets"
    assay_dir = p / "assay_data"

    pinfo_paths = sorted(pinfo_dir.rglob("*.csv")) if pinfo_dir.is_dir() else []
    res["pinfo_csv_paths"] = [str(f) for f in pinfo_paths]

    if selected_assay == "fcs" and assay_dir.is_dir():
        res["assay_file_paths"] = sorted(str(f) for f in assay_dir.rglob("*.fcs"))
    elif selected_assay == "derived-results" and assay_dir.is_dir():
        res["assay_file_paths"] = sorted(
            str(f) for f in assay_dir.rglob("*_derived.csv")
        )

    return res


def read_pinfo_csvs(paths: list[str]) -> dict[str, pd.DataFrame]:
    pinfos = {}
    for path in paths:
        fname = Path(path).name
        df = pd.read_csv(path, header=0)
        df["Filename"] = fname
        pinfos[fname] = df
    return pinfos


def read_fcs_head(paths: list[str]) -> pd.DataFrame:
    import flowio

    rows = []
    for path in paths:
        fname = Path(path).name
        try:
            fd = flowio.FlowData(path, only_text=True)
            meta = fd.text
        except Exception:
            meta = {}

        def get(key, _meta=meta):
            return _meta.get(key, "not_in_fcs")

        rows.append(
            {
                "PLATENAME": get("platename"),
                "WELLID": get("wellid"),
                "DATE": get("date"),
                "TOT": get("tot"),
                "Filename": fname,
            }
        )

    df = pd.DataFrame(rows)
    df["PLATENAME"] = df["PLATENAME"].replace("NA", "00_Missing_Platename")
    df["Platename"] = df["PLATENAME"]
    df["Plate Position"] = df["WELLID"]
    df["Analysis Date"] = df["DATE"]
    df["Analysis Type"] = "Antibody Binding"
    df["Assay Data Type"] = "Flow Cytometry"
    df["Result Type"] = "FCS Total Events"
    df["Result"] = df["TOT"]
    return df


def read_derived_results_csvs(paths: list[str]) -> pd.DataFrame:
    frames = []
    for path in paths:
        fname = Path(path).name
        df = pd.read_csv(path, header=0)
        df["Filename"] = fname
        frames.append(df)
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def read_assay_data(paths: list[str], selected_assay: str) -> pd.DataFrame:
    if selected_assay == "fcs":
        return read_fcs_head(paths)
    if selected_assay == "derived-results":
        return read_derived_results_csvs(paths)
    return pd.DataFrame()
