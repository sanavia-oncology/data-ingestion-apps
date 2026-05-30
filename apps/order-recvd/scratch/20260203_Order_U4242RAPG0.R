
P = paste0("/Users/kwameokrah/sanavia_apps/checkR/apps/",
           "order-recvd/code/helper_funcs/src/gs_funcs/")
for (fl in list.files(P, recursive = T, full.names = T)) source(fl)

project_fldr = "/Users/kwameokrah/data_depo/genscript-recvd/"
order_files_paths = get_paths_by_order(project_fldr)

fls = order_files_paths[["20260203_Order_U4242RAPG0"]]

order_sheets = read_order_sheets(fls)
names(order_sheets)

order_sheets[["number_of_sheets"]]
clone_strategy = order_sheets[["clone_strategy"]]
order_summary = order_sheets[["order_summary"]]
order_sheets[["location_map"]]

dim(clone_strategy)
dim(order_summary)

fls_locmap = fls[grep("location_map", fls)]

locmap1 = preproc_location_map(fls_locmap[1])
locmap2 = preproc_location_map(fls_locmap[2])
locmap3 = preproc_location_map(fls_locmap[3])
locmap4 = preproc_location_map(fls_locmap[4])
locmap5 = preproc_location_map(fls_locmap[5])

locmap = rbind(locmap1, locmap2, locmap3, locmap4, locmap5)

locmap_list = split(locmap, locmap$`Order ID`)

HOLD = list()
for (i in names(locmap_list)) {
    x = locmap_list[[i]]
    hold = c()
    for (k in c("Box Name", "Position", "Volume(ml)", "Box Type")) {
        vec = paste0(x[[k]], collapse = " | ")
        names(vec) = k
        hold = c(vec, hold)
    }
    hold = c("Order ID"=unique(x[["Order ID"]]), "Name"=unique(x[["Name"]]), hold)
    HOLD[[i]] = hold
}

HOLD = do.call(rbind, HOLD)
rownames(HOLD) = NULL
location_map = as.data.frame(HOLD)
o = order(as.numeric(sapply(strsplit(location_map[["Order ID"]], "-"), "[[", 2)))
location_map = location_map[o,]
order_sheets[["location_map"]] = location_map

clone_strategy[["Order ID"]]
order_summary[["Order ID"]]
location_map[["Order ID"]]

merged_sheets = merge_order_sheets(order_sheets)
merged_sheets[["Merge Date"]] = Sys.Date()

# add no_seqs
cn = colnames(merged_sheets)
seq_cols = merged_sheets[,grep("^sequence", cn),drop=F]
no_seqs = rowSums(!is.na(seq_cols))
merged_sheets[["no_of_seqs"]] = no_seqs

# drop duplicate columns
merged_sheets = merged_sheets[,-grep("\\.[1-9]$", cn), drop=F]

nam = merged_sheets$Name
head(nam)

target1 = gsub("MSV.", "MSV_HU", nam)
target1_alias = gsub("HU0", "HU", substr(target1, 10, 26))

df = data.frame("assembly_id"=nam,
                "assembly_type"="AT00",
                "assembly_type_alias"="monospec-bivalent",
                "target1"=target1,
                "target1_alias"=target1_alias,
                "target2"=NA,
                "target2_alias"=NA,
                "target1_antigen"="MUC1",
                "target2_antigen"=NA,
                "no_expected_seqs"=2)

write.csv(df,
          file="/Users/kwameokrah/data_depo/genscript-recvd/2026/20260203_Order_U4242RAPG0/keys/U4242RAPG0-key-order-00.csv",
          row.names = F)

merged_sheets = cbind(merged_sheets, df)

write.csv(merged_sheets,
          file="/Users/kwameokrah/data_depo/genscript-recvd/2026/20260203_Order_U4242RAPG0/00_merged_sheets/20260203_Order_U4242RAPG0-merged_sheets.csv",
          row.names = F)

head(merged_sheets)
