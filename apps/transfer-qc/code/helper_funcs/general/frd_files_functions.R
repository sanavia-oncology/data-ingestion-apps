# author: Kwame Okrah
# date: 2025-12-14

make_rinfo_sheet = function(meta_data) {
    md_pl = split(meta_data, meta_data$plate_name)
    hold_pinfo = list()
    
    for (ps in names(md_pl)) {
        md_ = md_pl[[ps]]
        md = md_[md_$StepType=="ASSOC",,drop=F]
        loading = md_[md_$StepType=="LOADING",,drop=F]
        
        RUN_ID = md$RunID
        FRD_FILE = md$frd_file
        FRD_FOLDER = md$frd_folder
        
        PLATENAME = md$plate_name
        PLATE_POSITION = md$well_id
        CREATOR = "lab.user"
        
        TARG_SPEC_TYPE = "peptide"
        TARG_SPEC_ID = md$loaded_analyte
        
        TARG_SPEC_QUANT_TYPE = "concentration (ug/ml)"
        TARG_SPEC_QUANT_VAL = unique(loading$Concentration)
        
        PROBE_TYPE = "antibody"
        PROBE_ID = md$SampleID
        PROBE_QUANT_TYPE = "concentration (ug/ml)"
        PROBE_QUANT_VAL = md$Concentration
        
        PRIM_ROLE_ = tolower(gsub("K", "", md$WellType))
        PRIM_ROLE = ifelse(PRIM_ROLE_=="reference", "control", PRIM_ROLE_)
        SUBROLE = ifelse(PRIM_ROLE_=="reference", PRIM_ROLE_, NA)
        
        PRIM_ROLE[TARG_SPEC_ID=="1X Kinetics Buffer"] = "control"
        SUBROLE[TARG_SPEC_ID=="1X Kinetics Buffer"] = "negative"
        
        pinfo = data.frame("Run ID"=RUN_ID,
                           "FRD File"=FRD_FILE,
                           "FRD Folder"=FRD_FOLDER,
                           "Platename"=PLATENAME, 
                           "Plate Position"=PLATE_POSITION,
                           "Creator"=CREATOR,
                           "Target Spec Type"=TARG_SPEC_TYPE,
                           "Target Spec ID"=TARG_SPEC_ID, 
                           "Probe Type"=PROBE_TYPE, 
                           "Probe ID"=PROBE_ID,
                           "Probe Quant Type"=PROBE_QUANT_TYPE,
                           "Probe Quant Value"=PROBE_QUANT_VAL,
                           "Primary Role"=PRIM_ROLE,
                           "Subrole"=SUBROLE,
                           "Target Spec Quant Type"=TARG_SPEC_QUANT_TYPE,
                           "Target Spec Quant Value"=TARG_SPEC_QUANT_VAL,
                           check.names = F)
        
        hold_pinfo[[ps]] = pinfo
    }
    
    rinfo = do.call(rbind, hold_pinfo)
    rownames(rinfo) = NULL
    
    return(rinfo)
}

make_project_rinfo_sheet = function(res) {
    runids = res[["runids"]]
    hold = list()
    for (runid in runids) {
        meta_data = res[["meta_data"]][[runid]]
        hold[[runid]] = make_rinfo_sheet(meta_data)
    }
    
    ans = do.call(rbind, hold)
    rownames(ans) = NULL
    
    return(ans)
}

make_fred_pinfo_assay_data = function(data_path) {

    all_fls = list.files(data_path, recursive=TRUE, full.names=TRUE)
    frd_files = all_fls[grep("\\.frd$", all_fls)]
    res = load_frd_files(frd_files)

    pinfo_path = paste0(data_path, "/plate_information_sheets")
    assay_data_path = paste0(data_path, "/assay_data")

    if (dir.exists(pinfo_path)) {
        unlink(pinfo_path, recursive=TRUE)
        dir.create(pinfo_path)
    }else{
        dir.create(pinfo_path)
    }

    if (dir.exists(assay_data_path)) {
        unlink(assay_data_path, recursive=TRUE)
        dir.create(assay_data_path)
    }else{
        dir.create(assay_data_path)
    }

    cat("making pinfo and assay data files...\n")
  
    pj = make_project_rinfo_sheet(res)
    pj_file_nam = paste0(Sys.Date(), "_", pj[1, "Run ID"], "_proj-pinfo.csv")
    pj_file_path = paste0(pinfo_path, "/", pj_file_nam)
  
    write.csv(pj, file=pj_file_path, row.names=FALSE)

    for (runid in res$runids) {
        run_list = res[["assay_data"]][[runid]]

        run_path = paste0(assay_data_path, "/", runid)
        dir.create(run_path)

        for (frd in names(run_list)) {
            frd_fl_nam = paste0(run_path, "/", gsub("[\\||\\.]", "_", frd), ".csv")
            write.csv(run_list[[frd]], file=frd_fl_nam, row.names=FALSE)
        }
    }
  
}