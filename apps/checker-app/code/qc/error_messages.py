from datetime import date
import io
import pandas as pd


def print_pinfo_error_msg(plate_nam: str, pinfo_col: str, checks_detailed: dict) -> str:
    buf = io.StringIO()

    def p(*args):
        print(*args, file=buf)

    x = checks_detailed[plate_nam][pinfo_col]
    p(f"Today's date: {date.today()}")
    p()

    if pinfo_col == "Required Columns":
        x_fmt = x.copy()
        x_fmt.columns = [c.replace(" ", "_") for c in x_fmt.columns]
        x_fmt.iloc[:, 0] = "'" + x_fmt.iloc[:, 0] + "'"
        p(x_fmt.to_string(index=False))
    else:
        col_vals = x.iloc[:, 0]
        p(f"Column: {pinfo_col}")
        p(f"Length of column: {len(col_vals)}")

        if pinfo_col == "Probe Quant Value":
            p()
            p("Column summary table:")
            p("---------------------")
            numeric = pd.to_numeric(col_vals, errors="coerce")
            p(numeric.describe().to_string())
        else:
            p()
            p("Column summary table:")
            p("---------------------")

            display_vals = col_vals.astype(str)
            if pinfo_col == "Plate Position":
                display_vals = display_vals.str[0]

            tab = display_vals.value_counts().sort_values(ascending=False)
            tab_df = pd.DataFrame({"Value": "'" + tab.index + "'", "Count": tab.values})
            tab_df.index = range(1, len(tab_df) + 1)
            p(tab_df.to_string())

    # Error rows: any check column is False
    check_cols = x.columns[1:]
    err_mask = (~x[check_cols]).any(axis=1)
    y = x[err_mask]

    if len(y) > 0:
        p()
        p("Status: FAIL!")

        if pinfo_col == "Required Columns":
            p()
            p("Plate information sheet column(s) with errors:")
            p("----------------------------------------------")
            pinfo_colnames = x.attrs.get("pinfo_colnames", [])
            recognized = set(x.iloc[:, 0])
            unrecognized = [f"'{c}'" for c in pinfo_colnames[:11] if c not in recognized]
            if unrecognized:
                udf = pd.DataFrame({"Unrecognized_Columns": unrecognized})
                p(udf.to_string(index=False))
        else:
            p()
            p("Plate information sheet row(s) with errors:")
            p("-------------------------------------------")
            # Drop columns where all error rows pass (keep only value col + failing checks)
            failing_check_cols = [c for c in check_cols if not y[c].all()]
            y_show = y[x.columns[:1].tolist()].copy()
            y_show.columns = [y_show.columns[0].replace(" ", "_")]
            y_show.iloc[:, 0] = "'" + y_show.iloc[:, 0].astype(str) + "'"
            y_show.insert(0, "Row", y.index + 1)
            y_show = y_show.reset_index(drop=True)
            p(y_show.to_string(index=False))
    else:
        p()
        p("Status: PASS!")

    return buf.getvalue()


def print_merge_error_msg(
    plate_nam: str,
    merge_info: dict,
    assay_position_check_summary: dict,
    assay_position_check_detailed: dict,
) -> str:
    buf = io.StringIO()

    def p(*args):
        print(*args, file=buf)

    x = merge_info[plate_nam]["merge_checks"]

    p(f"Today's date: {date.today()}")
    p()
    p("Data merge information:")
    p("-----------------------")
    p()

    if all(x.values()):
        pinfo_sid = merge_info[plate_nam]["pinfo_sid"] or []
        assay_sid = merge_info[plate_nam]["assay_sid"] or []
        p(f"{len(pinfo_sid)} samples in Plate_Information_Sheet|Platename matched")
        p(f"{len(assay_sid)} samples in Assay_Data|Platename.")
        p()
        p("Status: PASS!")
    else:
        failing = [k for k, v in x.items() if not v]
        check_ind = int(failing[0].replace("check", ""))
        apnam = merge_info[plate_nam]["key"]

        if check_ind == 1:
            p("Plate_Information_Sheet|Platename and/or\nPlate_Information_Sheet|Plate_Position failed.")

        elif check_ind == 2:
            assay_pnames = [f"'{n}'" for n in assay_position_check_summary]
            df = pd.DataFrame({"Assay_Data|Platename": assay_pnames})
            p(f"Plate_Information_Sheet|Platename = '{apnam}'")
            p("not found in Assay_Data.\n")
            p("The following Assay_Data|Platename were detected:\n")
            p(df.to_string(index=False))

        elif check_ind == 3:
            ac = assay_position_check_detailed.get(apnam, pd.DataFrame())
            if not ac.empty:
                ac_cols = [c.replace(" ", "_") for c in ac.columns]
                ac = ac.copy()
                ac.columns = ac_cols
                check_cols = ac.columns[1:]
                err_mask = (~ac[check_cols]).any(axis=1)
                ac_err = ac[err_mask].copy()
                ac_err.insert(0, "Row", ac_err.index + 1)
                ac_err = ac_err.reset_index(drop=True)
                p(f"Errors in Assay_Data|Plate_Position.\n\nSee table below for rows with errors:\n")
                p(f"Assay_Data|Platename = '{apnam}'\n")
                p(ac_err.to_string(index=False))

        elif check_ind == 4:
            pinfo_sid = merge_info[plate_nam]["pinfo_sid"] or []
            assay_sid = set(merge_info[plate_nam]["assay_sid"] or [])
            mia = [s for s in pinfo_sid if s not in assay_sid]
            df = pd.DataFrame({"Platename|Plate_Position": [f"'{s}'" for s in mia]})
            p("Samples in the Plate_Information_Sheet did not\nfind matches in the Assay_Data.\n")
            p("The non-matching sample(s) are:\n")
            p(df.to_string(index=False))

        p()
        p("Status: FAIL!")

    return buf.getvalue()


def print_error_msg(
    plate_nam: str,
    pinfo_col: str = None,
    checks_detailed: dict = None,
    merge_info: dict = None,
    assay_position_check_summary: dict = None,
    assay_position_check_detailed: dict = None,
) -> str:
    if pinfo_col == "DATA MERGE":
        return print_merge_error_msg(
            plate_nam, merge_info, assay_position_check_summary, assay_position_check_detailed
        )
    return print_pinfo_error_msg(plate_nam, pinfo_col, checks_detailed)
