import pandas as pd

REQUIRED_COLUMNS = [
    "Platename", "Plate Position", "Creator",
    "Target Spec Type", "Target Spec ID",
    "Probe Type", "Probe ID",
    "Probe Quant Type", "Probe Quant Value",
    "Primary Role", "Subrole",
]

VALID_WELLS = [f"{r}{c}" for r in "ABCDEFGH" for c in range(1, 13)]

EXPECTED_CREATORS = {
    "silvana.digiandomenico", "nan.chen", "brendan.buehler",
    "maria.sjostrand", "remy.schneider", "glenn.gregorio",
    "bernardo.reis", "megan.mccloskey", "louis.mattera",
    "kwame.okrah", "karl.sebby", "server.ertem",
    "valentina.marchionni", "lab.user",
}

EXPECTED_TARGET_SPEC_TYPES = {"cell", "serum", "tissue", "protein", "peptide", "other"}
EXPECTED_PROBE_TYPES = {"antibody", "cell", "car-t", "adc", "other"}
EXPECTED_PROBE_QUANT_TYPES = {
    "concentration (ug/ml)", "e:t ratio", "dilution factor",
    "car-t dilution", "cell dilution", "others",
}
EXPECTED_PRIMARY_ROLES = {"sample", "control", "standard", "others"}
EXPECTED_SUBROLES = {
    "negative", "positive", "max-kill ctrl", "min-kill ctrl",
    "reference", "others", "",
}


def _fill_na(vals):
    return ["" if (v is None or (isinstance(v, float) and pd.isna(v))) else str(v) for v in vals]


def _mode(vals):
    from collections import Counter
    c = Counter(v for v in vals if v != "")
    return c.most_common(1)[0][0] if c else ""


def required_columns_check(col_names: list[str]) -> pd.DataFrame:
    is_avail = [c in col_names for c in REQUIRED_COLUMNS]
    df = pd.DataFrame({"Required Columns": REQUIRED_COLUMNS, "Is Available?": is_avail})
    df.attrs["pinfo_colnames"] = col_names
    return df


def platename_check(platename) -> pd.DataFrame:
    if platename is None:
        return pd.DataFrame({"Platename": ["Is Missing"], "Is Available?": [False], "Is Identical?": [False]})
    x = _fill_na(platename)
    is_avail = [v != "" for v in x]
    mode_val = _mode(x)
    is_identical = [v == mode_val for v in x]
    return pd.DataFrame({"Platename": platename, "Is Available?": is_avail, "Is Identical?": is_identical})


def plate_position_check(plate_position) -> pd.DataFrame:
    if plate_position is None:
        return pd.DataFrame({
            "Plate Position": ["Is Missing"],
            "Is Available?": [False], "Is Distinct?": [False], "Right Format?": [False],
        })
    x = _fill_na(plate_position)
    is_avail = [v != "" for v in x]
    seen = set()
    is_distinct = []
    for v in x:
        is_distinct.append(v not in seen)
        seen.add(v)
    right_fmt = [v in VALID_WELLS for v in x]
    return pd.DataFrame({
        "Plate Position": plate_position,
        "Is Available?": is_avail,
        "Is Distinct?": is_distinct,
        "Right Format?": right_fmt,
    })


def creator_check(creator) -> pd.DataFrame:
    if creator is None:
        return pd.DataFrame({"Creator": ["Is Missing"], "Is Available?": [False], "Is Identical?": [False], "Right Format?": [False]})
    x = _fill_na(creator)
    is_avail = [v != "" for v in x]
    mode_val = _mode(x)
    is_identical = [v == mode_val for v in x]
    right_fmt = [v in EXPECTED_CREATORS for v in x]
    return pd.DataFrame({"Creator": creator, "Is Available?": is_avail, "Is Identical?": is_identical, "Right Format?": right_fmt})


def target_specimen_type_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Target Spec Type": ["Is Missing"], "Is Available?": [False], "Is Identical?": [False], "Right Format?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    mode_val = _mode(x)
    is_identical = [v == mode_val for v in x]
    right_fmt = [v in EXPECTED_TARGET_SPEC_TYPES for v in x]
    return pd.DataFrame({"Target Spec Type": vals, "Is Available?": is_avail, "Is Identical?": is_identical, "Right Format?": right_fmt})


def target_specimen_id_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Target Spec ID": ["Is Missing"], "Is Available?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    return pd.DataFrame({"Target Spec ID": vals, "Is Available?": is_avail})


def probe_type_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Probe Type": ["Is Missing"], "Is Available?": [False], "Is Identical?": [False], "Right Format?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    mode_val = _mode(x)
    is_identical = [v == mode_val for v in x]
    right_fmt = [v in EXPECTED_PROBE_TYPES for v in x]
    return pd.DataFrame({"Probe Type": vals, "Is Available?": is_avail, "Is Identical?": is_identical, "Right Format?": right_fmt})


def probe_id_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Probe ID": ["Is Missing"], "Is Available?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    return pd.DataFrame({"Probe ID": vals, "Is Available?": is_avail})


def probe_quantification_type_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Probe Quant Type": ["Is Missing"], "Is Available?": [False], "Is Identical?": [False], "Right Format?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    mode_val = _mode(x)
    is_identical = [v == mode_val for v in x]
    right_fmt = [v in EXPECTED_PROBE_QUANT_TYPES for v in x]
    return pd.DataFrame({"Probe Quant Type": vals, "Is Available?": is_avail, "Is Identical?": is_identical, "Right Format?": right_fmt})


def probe_quantification_value_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Probe Quant Value": ["Is Missing"], "Is Available?": [False]})
    is_avail = [True] * len(vals)
    return pd.DataFrame({"Probe Quant Value": vals, "Is Available?": is_avail})


def primary_role_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Primary Role": ["Is Missing"], "Is Available?": [False], "Right Format?": [False]})
    x = _fill_na(vals)
    is_avail = [v != "" for v in x]
    right_fmt = [v in EXPECTED_PRIMARY_ROLES for v in x]
    return pd.DataFrame({"Primary Role": vals, "Is Available?": is_avail, "Right Format?": right_fmt})


def subrole_check(vals) -> pd.DataFrame:
    if vals is None:
        return pd.DataFrame({"Subrole": ["Is Missing"], "Right Format?": [False]})
    x = _fill_na(vals)
    right_fmt = [v in EXPECTED_SUBROLES for v in x]
    return pd.DataFrame({"Subrole": vals, "Right Format?": right_fmt})


def _all_checks_pass(df: pd.DataFrame) -> bool:
    check_cols = df.columns[1:]
    return bool(df[check_cols].all(axis=None))


def perform_sheet_checks(sheet: pd.DataFrame) -> dict:
    checks_detailed = {}

    checks_detailed["Required Columns"] = required_columns_check(sheet.columns.tolist())
    checks_detailed["Platename"] = platename_check(sheet.get("Platename", pd.Series()).tolist())
    checks_detailed["Plate Position"] = plate_position_check(sheet.get("Plate Position", pd.Series()).tolist())
    checks_detailed["Creator"] = creator_check(sheet.get("Creator", pd.Series()).tolist())
    checks_detailed["Target Spec Type"] = target_specimen_type_check(sheet.get("Target Spec Type", pd.Series()).tolist())
    checks_detailed["Target Spec ID"] = target_specimen_id_check(sheet.get("Target Spec ID", pd.Series()).tolist())
    checks_detailed["Probe Type"] = probe_type_check(sheet.get("Probe Type", pd.Series()).tolist())
    checks_detailed["Probe ID"] = probe_id_check(sheet.get("Probe ID", pd.Series()).tolist())
    checks_detailed["Probe Quant Type"] = probe_quantification_type_check(sheet.get("Probe Quant Type", pd.Series()).tolist())
    checks_detailed["Probe Quant Value"] = probe_quantification_value_check(sheet.get("Probe Quant Value", pd.Series()).tolist())
    checks_detailed["Primary Role"] = primary_role_check(sheet.get("Primary Role", pd.Series()).tolist())
    checks_detailed["Subrole"] = subrole_check(sheet.get("Subrole", pd.Series()).tolist())

    checks_summary = {name: "Pass" if _all_checks_pass(df) else "Fail"
                      for name, df in checks_detailed.items()}

    return {"checks_summary": checks_summary, "checks_detailed": checks_detailed}


def perform_plateinfo_checks(pinfo_sheets: dict) -> dict:
    per_plate = {pid: perform_sheet_checks(sheet) for pid, sheet in pinfo_sheets.items()}

    checks_summary = {
        pid: res["checks_summary"] for pid, res in per_plate.items()
    }
    checks_detailed = {
        pid: res["checks_detailed"] for pid, res in per_plate.items()
    }

    return {"checks_summary": checks_summary, "checks_detailed": checks_detailed}
