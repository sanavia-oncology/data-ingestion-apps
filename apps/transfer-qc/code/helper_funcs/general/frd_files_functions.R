# author: Kwame Okrah
# date: 2025-12-14

load_frd_file = function(file_path) {
    xml_data = xml2::as_list(xml2::read_xml(file_path))
    
    ExperimentInfo = unlist(xml_data[["ExperimentResults"]][["ExperimentInfo"]])
    KineticsData = xml_data[["ExperimentResults"]][["KineticsData"]]
    
    OtherData_sel = c("FlowRate", "StepType", "StepName", 
                      "StepStatus","ActualTime", "CycleTime")
    
    CommonData_sel = c("SampleLocation", "SampleID", "SampleRow", 
                       "SamplePlate",
                       "WellType", "Concentration", "ConcentrationUnits", 
                       "MolarConcentration", "MolarConcUnits", 
                       "MolecularWeight",
                       "Temperature", "StartTime", "AssayTime")
    
    meta_data_hold = list()
    assay_data_hold = list()
    
    for (k in 1:length(KineticsData)) {
        
        CommonData = unlist(KineticsData[[k]][["CommonData"]][CommonData_sel])
        OtherData = unlist(KineticsData[[k]][OtherData_sel])
        
        step_name = OtherData["StepName"]
        well_id = paste0(CommonData["SampleRow"], CommonData["SampleLocation"])
        
        meta_data_hold[[step_name]] = c(ExperimentInfo, 
                                        CommonData, 
                                        OtherData,
                                        well_id=well_id)
        
        AssayXData = KineticsData[[k]][["AssayXData"]][[1]]
        AssayYData = KineticsData[[k]][["AssayYData"]][[1]]
        
        x_raw = base64enc::base64decode(gsub("\\s+", "", AssayXData))
        y_raw = base64enc::base64decode(gsub("\\s+", "", AssayYData))
        
        x = readBin(x_raw, what="double", size=8, endian="little", n=length(x_raw)/8)
        y = readBin(y_raw, what="double", size=8, endian="little", n=length(y_raw)/8)

        n = length(x)
        if (!length(y) == n) {
            stop("\nlength(y) and length(x) are different (K.okrah).")
        }
        
        at_ =  as.numeric(CommonData["AssayTime"])
        pps = n / at_ 
        
        assay_data = data.frame(run_id = rep(ExperimentInfo["RunID"], n),
                                step_name = rep(step_name, n),
                                assay_time = rep(at_, n),
                                points_per_sec = rep(pps, n),
                                "assay_y_data"=y,
                                "assay_x_data"=x)
        
        assay_data_hold[[k]] = assay_data
    }
    
    meta_data = do.call(rbind, meta_data_hold)
    meta_data = as.data.frame(meta_data)
    
    sel = meta_data$StepName == "Loading"
    meta_data$loaded_analyte = meta_data$SampleID[sel]
    meta_data$loaded_analyte_loc = meta_data$well_id[sel]
    
    assay_data = do.call(rbind, assay_data_hold)
    
    res = list(meta_data=meta_data, assay_data=assay_data)
    
    return(res)
}

load_frd_files = function(frd_files) {
    cat("loading frd files...\n")
    
    well_lvls = paste0(rep(LETTERS[1:8], each=12), 1:12)
    step_lvls = c("Baseline", 
                  "Loading", 
                  "Baseline2", 
                  "Association", 
                  "Dissociation")
    
    meta_data_hold = list()
    assay_data_hold = list()
    
    for (fl in frd_files) {
        fl_ = sapply(strsplit(fl, "/"), function(x) x[length(x)])
        fldr = sapply(strsplit(fl, "/"), function(x) x[length(x)-1])
        
        res = load_frd_file(fl)
        meta_data = res[["meta_data"]]
        meta_data$frd_file = fl_
        meta_data$frd_folder = fldr
        
        meta_data$well_id = factor(meta_data$well_id, levels=well_lvls)
        
        meta_data$loaded_analyte_loc = factor(meta_data$loaded_analyte_loc, 
                                              levels=well_lvls)
        
        meta_data$StepName = factor(meta_data$StepName, levels=step_lvls)
        
        meta_data$plate_name = paste0(meta_data$RunID, 
                                      "_", 
                                      meta_data$loaded_analyte_loc)
        
        id = paste0(unique(meta_data$RunID), "|", fl_)
        
        ad = res[["assay_data"]]
        
        ad$frd_file = fl_
        ad$frd_folder = fldr
        ad$step_name = factor(ad$step_name, levels=step_lvls)
        ad$assoc_sid = meta_data["Association", "SampleID"]
        ad$assoc_well_id = meta_data["Association", "well_id"]
        ad$loading_sid = meta_data["Loading", "SampleID"]
        ad$loading_well_id = meta_data["Loading", "well_id"]
        ad$SensorName = unique(meta_data$SensorName)
        ad$plate_name = paste0(ad$run_id, "_", ad$loading_well_id)
        
        assay_data_hold[[id]] = ad
        meta_data_hold[[id]] = meta_data
    }
    
    anam = strsplit(names(assay_data_hold), "\\|")
    runid = sapply(anam, "[[", 1)
    
    assay_data_runid = split(assay_data_hold, runid)
    meta_data_runid = split(meta_data_hold, runid)

    for (r in names(meta_data_runid)) {
        rl = meta_data_runid[[r]]
        rl_dat = do.call(rbind, rl)
        rownames(rl_dat) = NULL
        meta_data_runid[[r]] = rl_dat
    }
    
    res = list(meta_data = meta_data_runid, 
               assay_data = assay_data_runid,
               runids = names(split(runid, runid)))
    
    return(res)
}

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