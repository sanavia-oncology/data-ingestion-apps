# author: Kwame Okrah
# date: 2025-11-14

capitalize = function(x) {
    x_ = strsplit(x, "")[[1]]
    x_[1] = toupper(x_[1])
    paste0(x_, collapse = "")
}

plot_96well_plate = function(mat, 
                             mat_col,
                             mat_tex_col,
                             main = "main",
                             border_col = "white",
                             border_lwd = 1.5,
                             text_cex = 0.6) {
     
    op = par(mar=c(4, 3, 6, 2), cex.main=0.9)
    
    plot(0, 0, xlim=c(0, 12), ylim=c(0, 8), pch="", 
         xaxs="i", yaxs="i", xaxt="n", yaxt="n",
         xlab="", ylab="", main=main)
    
    box(col=border_col, lwd=border_lwd)
    
    ROW = 7:0; names(ROW) = LETTERS[1:8]
    COL = 0:11; names(COL) = as.character(1:12)
    
    for (rind in rownames(mat)) {
        for (cind in colnames(mat)) {

            cell_text = mat[rind, cind]
            cell_col = mat_col[rind, cind]
            text_col = mat_tex_col[rind, cind]
            
            polygon(c(0, 1, 1, 0) + COL[cind], 
                    c(0, 0, 1, 1) + ROW[rind], 
                    col=cell_col)
            
            text(COL[cind]+0.5, ROW[rind]+0.5, 
                 cell_text, cex=text_cex, col=text_col)
          
        }
    }

    abline(v=seq(0, 12, 1), col=border_col, lwd=border_lwd)
    abline(h=seq(0, 8, 1), col=border_col, lwd=border_lwd)
  
    axis(2, seq(1, 8) - 0.5, 
         LETTERS[8:1], 
         las=2, 
         mgp=c(0.5, 0.25, 0),
         tick=FALSE,
         font = 1,
         cex.axis=0.8,
         col=border_col)
    
    axis(3, seq(1, 12) - 0.5, 
         1:12,
         tick=FALSE,
         mgp=c(0.5, 0.15, 0),
         font = 1,
         cex.axis=0.8,
         col=border_col)
    
    par(op)
}

cover_sheet_plot = function(pinfo_sheets, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))
    
    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)
    text(0, 0.8, "Semantic QC Report", cex=2, font = 2)

    cr = pinfo_sheets[[1]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])
    text(0, 0.2, paste0(fn, " ", ln))

    text(0, 0.0, paste0("Report Date: ", Sys.Date()))
     
    if (approved) {
        text(0, -0.8, 
            "Reviewed and apporoved",
            col="seagreen")       
    }else{
        text(0, -0.8, 
            "Please review carefully for any logical errors",
            col="gray")
    }

    par(op)
}

role_plot_cover = function(pinfo_sheets, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))

    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)
    text(0, 0.8, "Primary & Subrole", cex=2, font = 2)

    if (approved) {
        text(0, 0.5, "Reviewed and approved",
            col="seagreen")
    }else{
        text(0, 0.5, "Please review carefully for any logical errors",
            col="gray")
    }

    
    pt_ = pinfo_sheets[[1]][["Probe Type"]][1]
    pt = capitalize(pt_)
    
    st_ = pinfo_sheets[[1]][["Target Spec Type"]][1]
    st = capitalize(st_)

    text(0, -0.20, paste0("Probe Type: ", pt), cex=1.35)
    text(0, -0.45, paste0("Target Spec Type: ", st), cex=1.35)
    
    par(op)
  
     # value range
    vr = list()
    for (s_ in names(pinfo_sheets)) {
        s = pinfo_sheets[[s_]]
        d1 = s[["Primary Role"]]
        d2 = s[["Subrole"]]
        d2[is.na(d2)|d2==""] = "sample"
        vr[[s_]] = paste0(d1, "|", d2)
    }

    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = ifelse(key_names %in% "sample|sample", "SMPL",
                 ifelse(key_names %in% "control|negative", "NEG",
                        ifelse(key_names %in% "control|positive", "POS",
                              ifelse(key_names %in% "control|reference", "REF",
                                    ifelse(key_names %in% "standard|sample", "STD", "OTHER")))))
    
    names(key) = key_names
  
    lk = length(key)
  
    rt = ifelse(key %in% "SMPL", "Sample",
                 ifelse(key %in% "NEG", "Negative control",
                        ifelse(key %in% "POS", "Positive control",
                              ifelse(key %in% "REF", "Reference",
                                    ifelse(key %in% "STD", "Standard", "Other")))))
        
    df = data.frame("Fig ID"=key,
                    "Role Type"=rt,
                    check.names = F)
    rownames(df) = NULL
    
    grid::grid.newpage()
    
    tt = gridExtra::ttheme_default(
        base_size = 10)
    
    gridExtra::grid.table(df, theme=tt)
    
    grid::grid.text(paste0(lk, " role type(s) used in project."),
                    x=0.05, y=0.9, just="left")
  
}

role_plot = function(plate_name, pinfo_sheets, approved = FALSE) {
    
    assay_data1 = pinfo_sheets[[plate_name]][["Primary Role"]]
    assay_data2 = pinfo_sheets[[plate_name]][["Subrole"]]
    assay_data2[is.na(assay_data2)|assay_data2==""] = "sample"
    assay_data = paste0(assay_data1, "|", assay_data2)

    plate_position = pinfo_sheets[[plate_name]][["Plate Position"]]
    
    # value range
    vr = list()
    for (s_ in names(pinfo_sheets)) {
        s = pinfo_sheets[[s_]]
        d1 = s[["Primary Role"]]
        d2 = s[["Subrole"]]
        d2[is.na(d2)|d2==""] = "sample"
        vr[[s_]] = paste0(d1, "|", d2)
    }

    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = ifelse(key_names %in% "sample|sample", "SMPL",
                 ifelse(key_names %in% "control|negative", "NEG",
                        ifelse(key_names %in% "control|positive", "POS",
                              ifelse(key_names %in% "control|reference", "REF",
                                    ifelse(key_names %in% "standard|sample", "STD", "OTHER")))))
    
    names(key) = key_names
    
    x = key[assay_data]
    lk = length(key)
    
    color = c("SMPL"="skyblue", 
              "NEG"="red3",
              "POS"="blue3",
              "REF"="gray30",
              "STD"="forestgreen")
    
    canvas = matrix(NA, nrow=8, ncol=12)
    rownames(canvas) = LETTERS[1:nrow(canvas)]
    colnames(canvas) = as.character(1:ncol(canvas))
    canvas_col = canvas
    canvas_text_col = canvas
    
    for (i in 1:length(x)) {
        ind = plate_position[i]
        row_ind = substr(ind, 1, 1)
        col_ind = gsub("[A-Z]", "", ind)
        canvas_col[row_ind, col_ind] = color[x[i]]
        
        canvas[row_ind, col_ind] = x[i]
        
        if (x[i] == "SMPL") {
            tcol = "black"
        }else{
            tcol = "white"
        }
        canvas_text_col[row_ind, col_ind] = tcol
    }
    
    canvas[is.na(canvas)] = "NA"
    canvas_text_col[is.na(canvas_text_col)] = "gray"
    
    pl = pinfo_sheets[[plate_name]][["Platename"]][1]
    
    cr = pinfo_sheets[[plate_name]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])
    
    if (approved) {
        ln = paste0(ln, " (reviewed and approved)")
    }

    main = paste0(plate_name, " | ", pl, "\n",
                  "Role Type")
    
    plot_96well_plate(canvas, 
                      canvas_col, 
                      canvas_text_col, 
                      main = main)
    
    margin_text = paste0(fn, " ", ln, "\n", 
                         "Analysis date: ", Sys.Date())
    
    mtext(margin_text, side = 3, 
          line = 2, adj = 0, col="gray",
          cex = 0.8)

}

probe_id_plot_cover = function(pinfo_sheets, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))
    
    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)
    
    text(0, 0.8, "Probe ID", cex=2, font = 2)

    if (approved) {
        text(0, 0.5, "Reviewed and approved",
            col="seagreen")
    }else{
        text(0, 0.5, "Please review carefully for any logical errors",
            col="gray")
    }
    
    pt = pinfo_sheets[[1]][["Probe Type"]][1]
    pt = capitalize(pt)
    text(0, -0.2, paste0("Probe Type: ", pt), cex=1.35)
    
    par(op)
    
    # value range
    vr = lapply(pinfo_sheets, function(x) unique(x[["Probe ID"]]))
    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = paste0("", sprintf(1:length(key_names), fmt="%03d"))
    names(key) = key_names
    
    lk = length(key)
  
    if (lk > 10) {
        key_sub = key[1:10]
      
        df = data.frame("Fig ID"=key_sub,
                        "Probe ID"=names(key_sub),
                        check.names = F)
        rownames(df) = NULL
        
        grid::grid.newpage()
        
        tt = gridExtra::ttheme_default(
            base_size = 10)
        
        gridExtra::grid.table(df, theme=tt)
        
        grid::grid.text(paste0(lk, " probes used in project"),
                        x=0.05, y=0.9, just="left")
      
        grid::grid.text("List of probes is too long.\nShowing only 10 probes. See Plate Information sheet.",
                        x=0.05, y=0.1, just="left") 
    }else{
        key_sub = key
      
        df = data.frame("Fig ID"=key_sub,
                        "Probe ID"=names(key_sub),
                        check.names = F)
        rownames(df) = NULL
        
        grid::grid.newpage()
        
        tt = gridExtra::ttheme_default(
            base_size = 10)
        
        gridExtra::grid.table(df, theme=tt)
        
        grid::grid.text(paste0(lk, " probes used in project."),
                        x=0.05, y=0.9, just="left")
    }
    
}

probe_id_plot = function(plate_name, pinfo_sheets, approved = FALSE) {
    num_column = "Probe ID"
    
    assay_data = pinfo_sheets[[plate_name]][[num_column]]
    plate_position = pinfo_sheets[[plate_name]][["Plate Position"]]
    
    # value range
    vr = lapply(pinfo_sheets, function(x) unique(x[[num_column]]))
    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = as.character(1:length(key_names))
    names(key) = key_names
    
    x = key[assay_data]
    lk = length(key)
    
    MS = RColorBrewer::brewer.pal(n=10, name="Spectral")
    
    if (lk <= length(MS)) {
        color = MS[1:lk]
    }else{
        color = colorRampPalette(MS)(lk)
    }
    names(color) = key
  
    canvas = matrix(NA, nrow=8, ncol=12)
    rownames(canvas) = LETTERS[1:nrow(canvas)]
    colnames(canvas) = as.character(1:ncol(canvas))
    canvas_col = canvas
    canvas_text_col = canvas
    
    for (i in 1:length(x)) {
        ind = plate_position[i]
        row_ind = substr(ind, 1, 1)
        col_ind = gsub("[A-Z]", "", ind)
        canvas_col[row_ind, col_ind] = color[x[i]]
        canvas[row_ind, col_ind] = paste0("", sprintf(as.numeric(x[i]), fmt="%03d"))
        canvas_text_col[row_ind, col_ind] = "black"
    }
    
    canvas[is.na(canvas)] = "NA"
    canvas_text_col[is.na(canvas_text_col)] = "gray"
    
    pl = pinfo_sheets[[plate_name]][["Platename"]][1]
    
    pt_ = pinfo_sheets[[plate_name]][["Probe Type"]][1]
    pt = capitalize(pt_)
    
    cr = pinfo_sheets[[plate_name]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])
    
    if (approved) {
        ln = paste0(ln, " (reviewed and approved)")

    }
    main = paste0(plate_name, " | ", pl, "\n",
                  "Probe Type: ", pt)
    
    plot_96well_plate(canvas, 
                    canvas_col, 
                    canvas_text_col,
                    main = main)
    
    margin_text = paste0(fn, " ", ln, "\n", 
                         "Analysis date: ", Sys.Date())
    
    mtext(margin_text, side = 3, 
          line = 2, adj = 0, col="gray",
          cex = 0.8)
    
    canvas_ = unique(as.character(canvas))
    canvas_ = sort(canvas_[canvas_ != "NA"])
    key_rev = names(key)
    names(key_rev) = paste0("", sprintf(as.numeric(key), fmt="%03d"))
    canvas_ = key_rev[canvas_]
    
    mtext("Probe Key", side=1, adj=0, line=1.0, cex=0.6, font=2)
    
    line = 1.6
    if (length(canvas_) <= 6) {
        for (nam in names(canvas_)) {
            mtext(paste0(nam, ": ", canvas_[nam]), 
                  side=1, adj=0, line=line, cex=0.6)
            line = line + 0.5
        } 
    }else{
        for (nam in names(canvas_)[1:5]) {
            mtext(paste0(nam, ": ", canvas_[nam]), 
                  side=1, adj=0, line=line, cex=0.6)
            line = line + 0.5
        }
        mtext("See cover sheet for full key", side=1, adj=0, 
              line=4.1, cex=0.6)
    }
}

probe_quant_plot_cover = function(pinfo_sheets, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))

    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)
    text(0, 0.8, "Probe Amount", cex=2, font = 2)

    if (approved) {
        text(0, 0.5, "Reviewed and approved",
            col="gray")
    }else{
        text(0, 0.5, "Please review carefully for any logical errors",
            col="gray")
    }

    
    pt_ = pinfo_sheets[[1]][["Probe Type"]][1]
    pt = capitalize(pt_)
    pqt_ = pinfo_sheets[[1]][["Probe Quant Type"]][1]
    pqt = capitalize(pqt_)

    text(0, -0.20, paste0("Probe Type: ", pt), cex=1.35)
    text(0, -0.45, paste0("Probe Quant Type: ", pqt), cex=1.35)

    par(op)
  
    num_column = "Probe Quant Value"

    # value range
    vr = lapply(pinfo_sheets, function(x) unique(as.numeric(x[[num_column]])))
    vr = unlist(vr)
    uvr = sort(unique(vr[!is.na(vr)]))
  
    ss = summary(uvr)
  
    df = data.frame("Stat name" = names(ss),
                    "Stat value" = sprintf(as.numeric(ss), fmt="%0.3f"),
                        check.names = F)
    rownames(df) = NULL
        
    grid::grid.newpage()
        
    tt = gridExtra::ttheme_default(
        base_size = 10)
    
    gridExtra::grid.table(df, theme=tt)
  
    grid::grid.text(paste0("Statistical summary of probe amount.\n",
                           "Review min. and max. to make sure they are as expected."),
                    x=0.05, y=0.9, just="left")
  
}

probe_quant_plot = function(plate_name, pinfo_sheets, approved = FALSE) {
    num_column = "Probe Quant Value"
    
    assay_data = pinfo_sheets[[plate_name]][[num_column]]
    plate_position = pinfo_sheets[[plate_name]][["Plate Position"]]

    x = as.numeric(assay_data)
    
    # value range
    vr = lapply(pinfo_sheets, function(x) unique(as.numeric(x[[num_column]])))
    vr = unlist(vr)
    uvr = sort(unique(vr[!is.na(vr)]))
    
    n = length(uvr)
    if (n==1) {
        col_key = blues9[3]
    }else{
        col_key = colorRampPalette(blues9[-9])(n)
    }
    
    names(col_key) = as.character(uvr)

    x_col = col_key[as.character(x)]

    canvas = matrix(NA, nrow=8, ncol=12)
    rownames(canvas) = LETTERS[1:nrow(canvas)]
    colnames(canvas) = as.character(1:ncol(canvas))
    canvas_col = canvas
    canvas_text_col = canvas

    for (i in 1:length(x)) {
        ind = plate_position[i]
        row_ind = substr(ind, 1, 1)
        col_ind = gsub("[A-Z]", "", ind)
        canvas[row_ind, col_ind] = as.character(x[i])
        canvas_col[row_ind, col_ind] = x_col[i]
        canvas_text_col[row_ind, col_ind] = "black" 
    }

    canvas[is.na(canvas)] = "NA"
    canvas_text_col[is.na(canvas_text_col)] = "gray"
  
    pl = pinfo_sheets[[plate_name]][["Platename"]][1]

    cr = pinfo_sheets[[plate_name]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])

    if (approved) {
        ln = paste0(ln, " (reviewed and approved)")
    }

    pt_ = pinfo_sheets[[plate_name]][["Probe Type"]][1]
    pt = capitalize(pt_)
    pqt_ = pinfo_sheets[[plate_name]][["Probe Quant Type"]][1]
    pqt = capitalize(pqt_)

    main = paste0(plate_name, " | ", pl, "\n",
                  pt, " ", pqt)
  
    plot_96well_plate(canvas, 
                      canvas_col, 
                      canvas_text_col, 
                      main = main)

    margin_text = paste0(fn, " ", ln, "\n", 
                         "Analysis Date: ", Sys.Date())
    
    mtext(margin_text, side = 3, 
          line = 2, adj = 0, col="gray",
          cex = 0.8)
    
}

spec_plot_cover = function(pinfo_sheets, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))
    
    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)
    
    text(0, 0.8, "Target Specimen", cex=2, font = 2)

    if (approved) {
        text(0, 0.5, "Reviewd and approved",
            col="seagreen")
    }else{
        text(0, 0.5, "Please review carefully for any logical errors",
            col="gray")
    }

    
    st = pinfo_sheets[[1]][["Target Spec Type"]][1]
    st = capitalize(st)
    text(0, -0.2, paste0("Specimen Type: ", st), cex=1.35)
    
    par(op)
    
    # value range
    vr = lapply(pinfo_sheets, function(x) unique(x[["Target Spec ID"]]))
    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = paste0("CT-", sprintf(1:length(key_names), fmt="%02d"))
    names(key) = key_names
    
    lk = length(key)
  
    if (lk <= 10) {
        
        df = data.frame("Fig ID"=key,
                        "Cell Type Name"=names(key),
                        check.names = F)
        rownames(df) = NULL
        
        grid::grid.newpage()
        
        tt = gridExtra::ttheme_default(
            base_size = 10)
        
        gridExtra::grid.table(df, theme=tt)
        
        grid::grid.text(paste0(lk, " target specimen types used in project."),
                        x=0.05, y=0.9, just="left")  
    }else{
      
        key_sub = key[1:10]
      
        df = data.frame("Fig ID"=key_sub,
                        "Cell Type Name"=names(key_sub),
                        check.names = F)
        rownames(df) = NULL
        
        grid::grid.newpage()
        
        tt = gridExtra::ttheme_default(
            base_size = 10)
        
        gridExtra::grid.table(df, theme=tt)
        
        grid::grid.text(paste0(lk, " target specimen types used in project."),
                        x=0.05, y=0.9, just="left") 
      
        grid::grid.text("List of target specimen types is too long.\nShowing only 10 types. See Plate Information Sheet.",
                        x=0.05, y=0.1, just="left")     

    }
    
}

spec_plot = function(plate_name, pinfo_sheets, approved = FALSE) {
    num_column = "Target Spec ID"
    
    assay_data = pinfo_sheets[[plate_name]][[num_column]]
    plate_position = pinfo_sheets[[plate_name]][["Plate Position"]]
    
    # value range
    vr = lapply(pinfo_sheets, function(x) unique(x[[num_column]]))
    vr = unlist(vr)
    key_names = sort(unique(vr[!is.na(vr)]))
    
    key = as.character(1:length(key_names))
    names(key) = key_names
    
    x = key[assay_data]
    lk = length(key)
    
    MS = RColorBrewer::brewer.pal(n=8, name="Dark2")
    
    if (lk <= length(MS)) {
        color = MS[1:lk]
    }else{
        color = colorRampPalette(MS)(lk)
    }
    names(color) = key
    
    canvas = matrix(NA, nrow=8, ncol=12)
    rownames(canvas) = LETTERS[1:nrow(canvas)]
    colnames(canvas) = as.character(1:ncol(canvas))
    canvas_col = canvas
    canvas_text_col = canvas
    
    for (i in 1:length(x)) {
        ind = plate_position[i]
        row_ind = substr(ind, 1, 1)
        col_ind = gsub("[A-Z]", "", ind)
        canvas_col[row_ind, col_ind] = color[x[i]]
        canvas[row_ind, col_ind] = paste0("CT-", sprintf(as.numeric(x[i]), fmt="%02d"))
        canvas_text_col[row_ind, col_ind] = "white"
    }
    
    canvas[is.na(canvas)] = "NA"
    canvas_text_col[is.na(canvas_text_col)] = "gray"
    
    pl = pinfo_sheets[[plate_name]][["Platename"]][1]
    
    tst = pinfo_sheets[[plate_name]][["Target Spec Type"]][1]
    tst = capitalize(tst)
    
    cr = pinfo_sheets[[plate_name]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])
    
    if (approved) {
        ln = paste0(ln, " (reviewed and approved)")
    }
    main = paste0(plate_name, " | ", pl, "\n",
                  "Target Specimen: ", tst, " Type")
    
    plot_96well_plate(canvas, 
                    canvas_col, 
                    canvas_text_col,
                    main = main)
    
    margin_text = paste0(fn, " ", ln, "\n", 
                         "Analysis date: ", Sys.Date())
    
    mtext(margin_text, side = 3, 
          line = 2, adj = 0, col="gray",
          cex = 0.8)
    
    canvas_ = unique(as.character(canvas))
    canvas_ = sort(canvas_[canvas_ != "NA"])
    key_rev = names(key)
    names(key_rev) = paste0("CT-", sprintf(as.numeric(key), fmt="%02d"))
    canvas_ = key_rev[canvas_]
    
    mtext("CT Key", side=1, adj=0, line=1.0, cex=0.6, font=2)
    
    line = 1.6
    if (length(canvas_) <= 6) {
        for (nam in names(canvas_)) {
            mtext(paste0(nam, ": ", canvas_[nam]), 
                  side=1, adj=0, line=line, cex=0.6)
            line = line + 0.5
        } 
    }else{
        for (nam in names(canvas_)[1:5]) {
            mtext(paste0(nam, ": ", canvas_[nam]), 
                  side=1, adj=0, line=line, cex=0.6)
            line = line + 0.5
        }
        mtext("See cover sheet for full key", side=1, adj=0, 
              line=4.1, cex=0.6)
    }
    
}

assay_data_plot_cover = function(merge_info, approved = FALSE) {
    op = par(mar=c(4, 2, 4, 2))

    plot(0, 0, pch="", xlab = "", ylab = "", axes=FALSE)

    text(0, 0.8, "Assay Data", cex=2, font = 2)

    if (approved) {
        text(0, 0.5, "Reviewd & approved",
            col="seagreen")
    }else{
        text(0, 0.5, "Please review carefully for any logical errors",
            col="gray")

    }
    
    dt = merge_info[[1]][["merged_data"]][["Assay Data Type"]][1]
    rt = merge_info[[1]][["merged_data"]][["Result Type"]][1]

    text(0, -0.20, paste0("Assay Data Type: ", dt), cex=1.35)
    text(0, -0.45, paste0("Result Type: ", rt), cex=1.35)

    par(op)
}

assay_data_plot = function(plate_name, merge_info, approved = FALSE) {
    num_column = "Result"
    
    assay_data = merge_info[[plate_name]][["merged_data"]][[num_column]]
    plate_position = merge_info[[plate_name]][["merged_data"]][["Plate Position"]]
    x = as.numeric(assay_data)
    
    # value range
    vr = lapply(merge_info, function(x) unique(as.numeric(x[["merged_data"]][[num_column]])))
    vr = unlist(vr)
    vr = unique(vr[!is.na(vr)])
    min_ = min(vr)
    max_ = max(vr) 

    lvr = length(vr)

    if (lvr > 50) {
        n = 51
    }else{
        if (lvr %% 2 == 0) {
            n = lvr + 1
        }else{
            n = lvr
        }
    }

    col_key = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(n)
    y = seq(min_, max_, length.out=n)
    names(col_key) = as.character(y)
    
    canvas = matrix(NA, nrow=8, ncol=12)
    rownames(canvas) = LETTERS[1:nrow(canvas)]
    colnames(canvas) = as.character(1:ncol(canvas))
    canvas_col = canvas
    canvas_text_col = canvas

    result_type = merge_info[[plate_name]][["merged_data"]][["Result Type"]][1]

    for (i in 1:length(x)) {
        ind = plate_position[i]
        row_ind = substr(ind, 1, 1)
        col_ind = gsub("[A-Z]", "", ind)
        
        if (!is.na(x[i])) {
            z = abs(y - x[i])
            canvas_col[row_ind, col_ind] = col_key[as.character(y[z == min(z)])]
            x_i = x[i]
            if (result_type %in% c("normalized AUC")) {
                x_i = sprintf(x_i, fmt="%0.2f")
            }
            canvas[row_ind, col_ind] = as.character(x_i)
            canvas_text_col[row_ind, col_ind] = "black" 
        }else{
            canvas_col[row_ind, col_ind] = NA
            canvas[row_ind, col_ind] = NA
            canvas_text_col[row_ind, col_ind] = NA
        }

    }

    canvas[is.na(canvas)] = "NA"
    canvas_text_col[is.na(canvas_text_col)] = "gray"
    
    pl = merge_info[[plate_name]][["merged_data"]][["Platename"]][1]

    cr = merge_info[[plate_name]][["merged_data"]][["Creator"]][1]
    crl = strsplit(cr, "\\.")
    fn = capitalize(crl[[1]][1])
    ln = capitalize(crl[[1]][2])

    if (approved) {
        ln = paste0(ln, " (Reviewed and approved)")
    }

    analysis_type = merge_info[[plate_name]][["merged_data"]][["Analysis Type"]][1]
    
    dt = merge_info[[plate_name]][["merged_data"]][["Assay Data Type"]][1]
    
    main = paste0(plate_name, " | ", pl, "\n", analysis_type,
                  " (", result_type, ")")
    
    plot_96well_plate(canvas, 
                      canvas_col, 
                      canvas_text_col, 
                      main = main)

    margin_text = paste0(fn, " ", ln, "\n", 
                         "Analysis Date: ", Sys.Date(), " | ",
                         "Assay Data Type: ", dt)
    
    mtext(margin_text, side = 3, 
          line = 2, adj = 0, col="gray",
          cex = 0.8)
    
    op = par(xpd = TRUE)

    qsel = floor(quantile(1:n, probs = c(0, .25, .5, .75, 1)))
    
    xloc = 5
    yloc = -1.5
    points(xloc+0.00, yloc, pch=15, cex=1.75, col = col_key[qsel[1]])
    points(xloc+0.25, yloc, pch=15, cex=1.75, col = col_key[qsel[2]])
    points(xloc+0.50, yloc, pch=15, cex=1.75, col = col_key[qsel[3]])
    points(xloc+0.75, yloc, pch=15, cex=1.75, col = col_key[qsel[4]])
    points(xloc+1.00, yloc, pch=15, cex=1.75, col = col_key[qsel[5]])
  
    if (result_type %in% c("normalized AUC")) {
        min_ = sprintf(min_, fmt="%0.2f")
        max_ = sprintf(max_, fmt="%0.2f")
    }
  
    text(xloc+0.00, yloc, min_, pos=2, cex=0.75)
    text(xloc+1.00, yloc, max_, pos=4, cex=0.75)
    text(xloc+0.50, yloc, "Color Key", pos=3, cex=0.75)
  
    par(op)

}

make_qc_report = function(pinfo_sheets, 
                          merge_info = NULL,
                          fig_path = NULL,
                          approved = FALSE) {

    if (is.null(fig_path)) {
        stop("Please provide a fig_path (K.Okrah)")
    }
    
    pdf(fig_path, height = 4.0, width = 6.25)

    cover_sheet_plot(pinfo_sheets, approved=approved)

    role_plot_cover(pinfo_sheets, approved=approved)
    for (plate_name in names(pinfo_sheets)) {
        role_plot(plate_name, pinfo_sheets, approved=approved)
    }  

    probe_id_plot_cover(pinfo_sheets, approved=approved)
    for (plate_name in names(pinfo_sheets)) {
        probe_id_plot(plate_name, pinfo_sheets, approved=approved)
    }
  
    probe_quant_plot_cover(pinfo_sheets, approved=approved)
    for (plate_name in names(pinfo_sheets)) {
        probe_quant_plot(plate_name, pinfo_sheets, approved=approved)
    }

    spec_plot_cover(pinfo_sheets, approved=approved)
    for (plate_name in names(pinfo_sheets)) {
        spec_plot(plate_name, pinfo_sheets, approved=approved)
    }
  
    if (!is.null(merge_info)) {
        assay_data_plot_cover(merge_info, approved=approved)
        for (plate_name in names(merge_info)) {
            assay_data_plot(plate_name, merge_info, approved=approved)
        }
    }

    dev.off()
}

approved_qc_report = function(data_path, 
                              pinfo_sheets, 
                              merge_info, 
                              author_qc = "unknown",
                              experiment_type = "none",
                              dose_type = "none",
                              notes="none") {
    
    fig_path = paste0(data_path, "/", Sys.Date(), "-qc-report.pdf")
    make_qc_report(pinfo_sheets, merge_info, fig_path, approved=TRUE)

    if (is.null(merge_info)) {
        merged_data = do.call(rbind, pinfo_sheets)
        rownames(merged_data) = NULL
    }else{
        merged_data = do.call(rbind, lapply(merge_info, function(x) x[["merged_data"]]))
        rownames(merged_data) = NULL
    }

    merged_data$author_exp = merged_data$Creator
    merged_data$author_qc = author_qc
    merged_data$date_qc = Sys.Date()
    
    merged_data$experiment_type = experiment_type
    merged_data$dose_type = dose_type
    merged_data$notes = notes
    
    table_path = paste0(data_path, "/", Sys.Date(), "-merged-data.csv")
    write.csv(merged_data, file=table_path, row.names=FALSE)
}
