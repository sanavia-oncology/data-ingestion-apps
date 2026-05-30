
P = paste0("/Users/kwameokrah/sanavia_apps/checkR/apps/",
           "order-recvd/code/helper_funcs/src/gs_funcs/")
for (fl in list.files(P, recursive = T, full.names = T)) source(fl)

project_fldr = "/Users/kwameokrah/data_depo/genscript-recvd"
order_files_paths = get_paths_by_order(project_fldr)

for (order_folder in names(order_files_paths)) {
    fls = order_files_paths[[order_folder]]
    
    if (!length(grep("merged_sheets.csv$", fls)) > 0) {
        order_sheets = read_order_sheets(fls)
        merged_sheets = merge_order_sheets(order_sheets)
        merged_sheets[["Merge Date"]] = Sys.Date()
        
        # add no_seqs
        cn = colnames(merged_sheets)
        seq_cols = merged_sheets[,grep("^sequence", cn),drop=F]
        no_seqs = rowSums(!is.na(seq_cols))
        merged_sheets[["no_of_seqs"]] = no_seqs
        
        # drop duplicate columns
        merged_sheets = merged_sheets[,-grep("\\.[1-9]$", cn), drop=F]
        
        # add keys (if available)
        fls_keys = fls[grep("\\/keys\\/", fls)]
        if (length(fls_keys) > 0) {
            KEY = read.csv(fls_keys, header=T, check.names=F) 
            rownames(KEY) = KEY$assembly_id
            print(all(rownames(KEY) %in% merged_sheets$Name))
            print(all(merged_sheets$Name %in% rownames(KEY)))
            
            KEY_Match = KEY[merged_sheets$Name,,drop=F]
            merged_sheets = cbind(merged_sheets, KEY_Match)
        }
        
        save_dir = sapply(strsplit(fls[1], order_folder), "[[", 1)
        save_dir = paste0(save_dir, order_folder, "/00_merged_sheets")
        
        if (!dir.exists(save_dir)) {
            dir.create(save_dir)
        }
        
        file = paste0(save_dir, "/", order_folder, "-merged_sheets.csv")
        write.csv(merged_sheets, file=file, row.names=FALSE)    
    }
}

order_files_paths = get_paths_by_order(project_fldr)
merged_sheets_list = list()
for (order_folder in names(order_files_paths)) {
    fls = order_files_paths[[order_folder]]
    
    dat = read.csv(fls[grep("merged_sheets.csv$", fls)], 
                   header = TRUE,
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    
    dat_oid = sapply(strsplit(dat[["Order ID"]][1], "-"), "[[", 1)
    folder_oid = sapply(strsplit(order_folder, "_Order_"), "[[", 2)
    
    if (dat_oid!=folder_oid) {
        stop("dat_oid is not equal to folder_oid (K.Okrah)")
    }
    
    merged_sheets_list[[order_folder]] = dat
}

names(merged_sheets_list)

front_page_table(merged_sheets_list)

ms = merged_sheets_list[[5]]
plot_merged_sheet(ms)
head(sub_table_merged_sheet(ms))
