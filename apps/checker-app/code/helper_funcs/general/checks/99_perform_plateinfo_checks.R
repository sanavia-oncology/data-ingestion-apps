
# author: Kwame Okrah
# date: 2025-11-14

perform_sheet_checks = function(sheet) {
    CHECKS_DETAILED = list()

    sheet_colnames = colnames(sheet)
    CHECKS_DETAILED[["Required Columns"]] = required_columns_check(sheet_colnames)

    platename = sheet$Platename
    CHECKS_DETAILED[["Platename"]] = platename_check(platename)

    plate_postion = sheet$"Plate Position"
    CHECKS_DETAILED[["Plate Position"]] = plate_position_check(plate_postion)

    creator = sheet$Creator
    CHECKS_DETAILED[["Creator"]] = creator_check(creator)

    target_specimen_type = sheet$"Target Spec Type"
    CHECKS_DETAILED[["Target Spec Type"]] = target_specimen_type_check(target_specimen_type)

    target_speciment_id = sheet$"Target Spec ID"
    CHECKS_DETAILED[["Target Spec ID"]] = target_speciment_id_check(target_speciment_id)

    probe_type = sheet$"Probe Type"
    CHECKS_DETAILED[["Probe Type"]] = probe_type_check(probe_type)

    probe_id = sheet$"Probe ID"
    CHECKS_DETAILED[["Probe ID"]] = probe_id_check(probe_id)

    probe_quantification_type = sheet$"Probe Quant Type"
    CHECKS_DETAILED[["Probe Quant Type"]] = probe_quantification_type_check(probe_quantification_type)

    probe_quantification_value = sheet$"Probe Quant Value"
    CHECKS_DETAILED[["Probe Quant Value"]] = probe_quantification_value_check(probe_quantification_value)

    primary_role = sheet$"Primary Role"
    CHECKS_DETAILED[["Primary Role"]] = primary_role_check(primary_role)

    subrole = sheet$Subrole
    CHECKS_DETAILED[["Subrole"]] = subrole_check(subrole)

    CHECKS_SUMMARY = list()
    for (i in names(CHECKS_DETAILED)) {
        x = CHECKS_DETAILED[[i]]
        y = x[, 2:ncol(x), drop=FALSE]
        CHECKS_SUMMARY[[i]] = all(apply(y, 2, all))
    }

    checks_summary = unlist(CHECKS_SUMMARY)

    res = list(checks_summary = checks_summary,
               checks_detailed = CHECKS_DETAILED)
  
    return(res)
}

perform_plateinfo_checks = function(pinfo_sheets) {
    hold = list()
  
    for (i in names(pinfo_sheets)) {
        sheet = pinfo_sheets[[i]]
        hold[[i]] = perform_sheet_checks(sheet)
    }

    checks_summary = sapply(hold, "[[", "checks_summary")
    checks_summary = ifelse(checks_summary == TRUE, "Pass", "Fail")
  
    checks_detailed = lapply(hold, "[[", "checks_detailed")

    res = list(checks_summary = checks_summary,
               checks_detailed = checks_detailed)

    return(res)
}