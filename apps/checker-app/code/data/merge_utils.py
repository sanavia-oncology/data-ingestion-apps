from datetime import date
import pandas as pd
from code.qc.checks import platename_check, plate_position_check


def assay_plate_position_check(data_list: dict) -> dict:
    position_check_list = {}
    for plate, df in data_list.items():
        position_check_list[plate] = plate_position_check(df["Plate Position"].tolist())

    def _all_pass(df):
        check_cols = df.columns[1:]
        return bool(df[check_cols].all(axis=None))

    position_check_summary = {
        plate: _all_pass(res) for plate, res in position_check_list.items()
    }

    return {
        "position_check_summary": position_check_summary,
        "position_check_detailed": position_check_list,
    }


def merge_assay_data(assay_data: pd.DataFrame, pinfo_sheets: dict) -> dict:
    data_list = {
        name: grp for name, grp in assay_data.groupby("Platename")
    }

    dres = assay_plate_position_check(data_list)
    position_check_summary = dres["position_check_summary"]
    position_check_detailed = dres["position_check_detailed"]

    hold = {}

    for pid, pinfo_dat in pinfo_sheets.items():
        check_list = {}

        pn_check = platename_check(pinfo_dat["Platename"].tolist())
        pp_check = plate_position_check(pinfo_dat["Plate Position"].tolist())

        check1 = bool(
            pn_check[pn_check.columns[1:]].all(axis=None)
            and pp_check[pp_check.columns[1:]].all(axis=None)
        )
        check_list["check1"] = check1

        if check1:
            pid_platename = pinfo_dat.iloc[0]["Platename"]
            check2 = pid_platename in data_list
        else:
            pid_platename = None
            check2 = False
        check_list["check2"] = check2

        if check2:
            check3 = position_check_summary.get(pid_platename, False)
        else:
            check3 = False
        check_list["check3"] = check3

        if check3:
            dat = data_list[pid_platename].copy()
            dat_rn = (dat["Platename"] + "|" + dat["Plate Position"]).tolist()
            SID = (pinfo_dat["Platename"] + "|" + pinfo_dat["Plate Position"]).tolist()
            check4 = all(s in dat_rn for s in SID)
        else:
            SID = None
            dat_rn = None
            check4 = False
        check_list["check4"] = check4

        if check4:
            dat = dat.copy()
            dat.index = dat_rn
            pinfo_copy = pinfo_dat.copy()
            pinfo_copy["SID"] = SID
            merged = pinfo_copy.copy()
            assay_cols = [
                c for c in dat.columns
                if c not in pinfo_copy.columns or c in ("Platename", "Plate Position")
            ]
            for col in assay_cols:
                if col not in merged.columns:
                    merged[col] = [dat.loc[s, col] if s in dat.index else None for s in SID]
            merged["PINFO_QC_DATE"] = date.today()
            merged = merged.reset_index(drop=True)
        else:
            merged = None

        hold[pid] = {
            "merge_checks": check_list,
            "merged_data": merged,
            "pinfo_sid": SID,
            "assay_sid": dat_rn,
            "key": pid_platename,
        }

    return {
        "merge_info": hold,
        "assay_position_check_summary": position_check_summary,
        "assay_position_check_detailed": position_check_detailed,
    }
