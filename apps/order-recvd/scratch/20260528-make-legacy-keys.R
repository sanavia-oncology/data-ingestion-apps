
P = paste0("/Users/kwameokrah/sanavia_apps/checkR/apps/",
           "order-recvd/code/helper_funcs/src/gs_funcs/")
for (fl in list.files(P, recursive = T, full.names = T)) source(fl)

project_fldr = "/Users/kwameokrah/data_depo/genscript-recvd"
order_files_paths = get_paths_by_order(project_fldr)


SEL = c("20241029_Order_U5221NXKG0",
        "20250618_Order_U494TDTHG0",
        "20260110_Order_U5791NXPG0",
        "20260112_Order_U3801956G0",
        "20260222_Order_U6457EDLG0",
        "20260222_Order_U9111LPRG0")

order_files_paths = order_files_paths[SEL]



for (i in 1:length(SEL)) {
    print(i)
    k = SEL[i]
    os_path = order_files_paths[[k]][3]
    tmp = readxl::read_excel(os_path, skip=1)
    print(k)
    # print(head(tmp))
    print(head(tmp$Name))
    print("")
    print("")
    
    nam = tmp$Name
    target1 = gsub("MSV.", "MSV_HU", nam)
    target1 = gsub("_0", ".0", target1)
    target1 = gsub("HUHU", "HU", target1)
    
    print(head(target1))
    
    if (k=="20241029_Order_U5221NXKG0") {
        target1_alias = target1  
    }else{
        target1_alias = substr(target1, 10, nchar(target1))
        target1_alias = gsub("HU0", "HU", target1_alias)
    }
    
    print(head(target1_alias))


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

    fl = paste0(sapply(strsplit(os_path, "/order_summary"), "[[", 1), 
                "/keys/", k, "-key-order-00.csv")

    write.csv(df, file=fl, row.names = F)
    
}
