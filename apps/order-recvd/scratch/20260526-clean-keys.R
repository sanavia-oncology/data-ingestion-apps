

FL1 = "/Users/kwameokrah/claude_code/shiny/data/genscript-recvd/2026/20260319_Order_U905XMYNG0/keys/.hidden"
hold1 = list()
for (k in list.files(FL1, recursive = T, full.names = T)) {
    hold1[[k]] = read.csv(k, header = T, stringsAsFactors = F, check.names = F)
}
names(hold1)

FL2 = "/Users/kwameokrah/claude_code/shiny/data/genscript-recvd/2026/20260518_Order_U5487PLRG0/keys/.hidden"
hold2 = list()
for (k in list.files(FL2, recursive = T, full.names = T)) {
    hold2[[k]] = read.csv(k, header = T, stringsAsFactors = F, check.names = F)
}
names(hold2)

FL3 = "/Users/kwameokrah/claude_code/shiny/data/genscript-recvd/2026/20260520_Order_U9725654G0/keys/.hidden"
hold3 = list()
for (k in list.files(FL3, recursive = T, full.names = T)) {
    hold3[[k]] = read.csv(k, header = T, stringsAsFactors = F, check.names = F)
}
names(hold3)

no_expected_seqs = c("AT00"=2,
                     "AT01"=4,
                     "AT02"=2,
                     "AT03"=2,
                     "AT04"=3,
                     "AT05"=2,
                     "AT06"=2,
                     "AT07"=2,
                     "AT08"=2,
                     "AT09"=2)

#------------------------------- Part 1
x = hold1[[1]]
head(x)

at_code = sapply(strsplit(x$assembly_id, "_"), "[[", 2)
target1 = x[,"Target 1"]
target2_ = x[,"Target 2"]

head(target1)
target1 = sapply(strsplit(target1, "_sc|sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = c("Cetuximab WT"="SAEX0100",
            "Cetuxi Affinity reduced" = "SAEX0101",
            "Datroway WT"="SAEX0200",
            "Pani WT"="SAEX0300",
            "Pani Affinity reduced"="SAEX0301",
            "Trodelvy WT"="SAEX0400",
            "Trodelvy Affinity reduced"="SAEX0401")[target2_]

target2_alias = c("Cetuximab WT"="cetuximab",
                  "Cetuxi Affinity reduced" = "cetuximab affinity reduced",
                  "Datroway WT"="datopotamab",
                  "Pani WT"="panitumumab",
                  "Pani Affinity reduced"="panitumumab affinity reduced",
                  "Trodelvy WT"="sacituzumab",
                  "Trodelvy Affinity reduced"="sacituzumab affinity reduced")[target2_]

table(target2, target2_alias)

sort(unique(target2))

target1_antigen = sapply(strsplit(sapply(strsplit(x[,"assembly_id"], "_"), "[[", 1), "x"), "[[", 1)
target2_antigen = sapply(strsplit(sapply(strsplit(x[,"assembly_id"], "_"), "[[", 1), "x"), "[[", 2)


df = data.frame("assembly_id"=x[,"assembly_id"],
                "assembly_type"=at_code,
                "assembly_type_alias"=x[,"Assembly type"],
                "target1"=target1,
                "target1_alias"=target1_alias,
                "target2"=target2,
                "target2_alias"=target2_alias,
                "target1_antigen"=target1_antigen,
                "target2_antigen"=target2_antigen,
                "no_expected_seqs"=no_expected_seqs[at_code])

xxx = c("cetuximab"="SAEX0100",
        "cetuximab_H_Y102A_W52V" = "SAEX0101",
        "datopotamab"="SAEX0200",
        "Pani WT"="SAEX0300",
        "panitumumab_L_D50V_H_T103A"="SAEX0301",
        "sacituzumab"="SAEX0400",
        "sacituzumab_L_D28R_S30K"="SAEX0401")

names(xxx)

tmp = data.frame("assembly_id"=names(xxx),
                 "assembly_type"="AT00",
                 "assembly_type_alias"="monospec-bivalent",
                 "target1"=xxx,
                 "target1_alias"=c("Cetuximab WT"="cetuximab",
                                   "Cetuxi Affinity reduced" = "cetuximab affinity reduced",
                                   "Datroway WT"="datopotamab",
                                   "Pani WT"="panitumumab",
                                   "Pani Affinity reduced"="panitumumab affinity reduced",
                                   "Trodelvy WT"="sacituzumab",
                                   "Trodelvy Affinity reduced"="sacituzumab affinity reduced"),
                 "target2"=NA,
                 "target2_alias"=NA,
                 "target1_antigen"=c("EGFR", 
                                     "EGFR", 
                                     "TROP2", 
                                     "EGFR", 
                                     "EGFR", 
                                     "TROP2", 
                                     "TROP2"),
                 "target2_antigen"=NA,
                 "no_expected_seqs"=2)

df = rbind(df, tmp)

head(df)


write.csv(df, file = paste0(gsub("\\/.hidden$", "", FL1), 
                            "/20260526-bispecs-key-order-01.csv"), row.names = F)


#------------------------------------------ Part 2
sapply(hold2, head)

x1 = hold2[[1]]
head(x1)
df1 = data.frame("assembly_id"=x1[,"abid"],
                 "assembly_type"="AT00",
                 "assembly_type_alias"="monospec-bivalent",
                 "target1"=x1[,"abid"],
                 "target1_alias"=tolower(x1[,"alias"]),
                 "target2"=NA,
                 "target2_alias"=NA,
                 "target1_antigen"="EGFR",
                 "target2_antigen"=NA,
                 "no_expected_seqs"=2)
head(df1)

x2 = hold2[[2]]
head(x2)
df2 = data.frame("assembly_id"=x2[,"abid"],
                 "assembly_type"="AT00",
                 "assembly_type_alias"="monospec-bivalent",
                 "target1"=x2[,"abid"],
                 "target1_alias"=tolower(x2[,"alias"]),
                 "target2"=NA,
                 "target2_alias"=NA,
                 "target1_antigen"="EGFR",
                 "target2_antigen"=NA,
                 "no_expected_seqs"=2)
head(df2)

x3 = hold2[[3]]
head(x3)
df3 = data.frame("assembly_id"=x3[,"abid"],
                 "assembly_type"="AT00",
                 "assembly_type_alias"="monospec-bivalent",
                 "target1"=x3[,"abid"],
                 "target1_alias"=tolower(x3[,"alias"]),
                 "target2"=NA,
                 "target2_alias"=NA,
                 "target1_antigen"="TROP2",
                 "target2_antigen"=NA,
                 "no_expected_seqs"=2)
head(df3)

x4 = hold2[[4]]
head(x4)
assembly_id = x4[,"assembly_id"]
assembly_type = paste0("AT", substr(assembly_id, 3, 4))
no_expected_seqs = no_expected_seqs[assembly_type]
assembly_type_alias = x4[,"assembly_type"]
target1 = sapply(strsplit(x4[,"target1"], "sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = x4[,"target2"]
target2_alias = tolower(x4[,"target2_alias"])

target2_antigen = x4[,"target2_antigen"]

df4 = data.frame("assembly_id"=assembly_id,
                 "assembly_type"=assembly_type,
                 "assembly_type_alias"=assembly_type_alias,
                 "target1"=target1,
                 "target1_alias"=target1_alias,
                 "target2"=target2,
                 "target2_alias"=target2_alias,
                 "target1_antigen"="MUC1",
                 "target2_antigen"=target2_antigen,
                 "no_expected_seqs"=no_expected_seqs)

df_main2 = rbind(df1, df2, df3, df4)
head(df_main2)

write.csv(df_main2, file = paste0(gsub("\\/.hidden$", "", FL2), 
                                  "/20260526-bispecs-key-order-02.csv"), row.names = F)


# ---------------------------------------- Part 3
sapply(hold3, head)

y1 = hold3[[1]]
head(y1)

assembly_id = y1[,"assembly_id"]
assembly_type = paste0("AT", substr(assembly_id, 3, 4))
no_expected_seqs = 2
assembly_type_alias = y1[,"assembly_type"]
target1 = sapply(strsplit(y1[,"target1"], "sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = y1[,"target2"]
target2_alias = gsub(" $", "", tolower(y1[,"target2_alias"]))

target2_antigen = substr(y1[,"assembly_id"], 2, 2)
target2_antigen = ifelse(target2_antigen=="E", "EGFR", "TROP2")

all(substr(y1$assembly_id, 19, 30)==y1$decoy_construct)

df_y1 = data.frame("assembly_id"=assembly_id,
                 "assembly_type"=assembly_type,
                 "assembly_type_alias"=assembly_type_alias,
                 "target1"=target1,
                 "target1_alias"=target1_alias,
                 "target2"=target2,
                 "target2_alias"=target2_alias,
                 "target1_antigen"="MUC1",
                 "target2_antigen"=target2_antigen,
                 "no_expected_seqs"=no_expected_seqs)
head(df_y1)


y2 = hold3[[2]]
head(y2)

assembly_id = y2[,"assembly_id"]
assembly_type = paste0("AT", substr(assembly_id, 3, 4))
no_expected_seqs = 2
assembly_type_alias = y2[,"assembly_type"]
target1 = sapply(strsplit(y2[,"target1"], "sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = y2[,"target2"]
target2_alias = gsub(" $", "", tolower(y2[,"target2_alias"]))

target2_antigen = substr(y2[,"assembly_id"], 2, 2)
target2_antigen = ifelse(target2_antigen=="E", "EGFR", "TROP2")

all(substr(y2$assembly_id, 19, 30)==y2$decoy_construct)

df_y2 = data.frame("assembly_id"=assembly_id,
                   "assembly_type"=assembly_type,
                   "assembly_type_alias"=assembly_type_alias,
                   "target1"=target1,
                   "target1_alias"=target1_alias,
                   "target2"=target2,
                   "target2_alias"=target2_alias,
                   "target1_antigen"="MUC1",
                   "target2_antigen"=target2_antigen,
                   "no_expected_seqs"=no_expected_seqs)
head(df_y2)


y3 = hold3[[3]]
head(y3)

assembly_id = y3[,"assembly_id"]
assembly_type = paste0("AT", substr(assembly_id, 3, 4))
no_expected_seqs = 2
assembly_type_alias = y3[,"assembly_type"]
target1 = sapply(strsplit(y3[,"target1"], "sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = y3[,"target2"]
target2_alias = gsub(" $", "", tolower(y3[,"target2_alias"]))

target2_antigen = substr(y3[,"assembly_id"], 2, 2)
target2_antigen = ifelse(target2_antigen=="E", "EGFR", "TROP2")

all(substr(y3$assembly_id, 19, 30)==y3$decoy_construct)

df_y3 = data.frame("assembly_id"=assembly_id,
                   "assembly_type"=assembly_type,
                   "assembly_type_alias"=assembly_type_alias,
                   "target1"=target1,
                   "target1_alias"=target1_alias,
                   "target2"=target2,
                   "target2_alias"=target2_alias,
                   "target1_antigen"="MUC1",
                   "target2_antigen"=target2_antigen,
                   "no_expected_seqs"=no_expected_seqs)
head(df_y3)


y4 = hold3[[4]]
head(y4)

assembly_id = y4[,"assembly_id"]
assembly_type = paste0("AT", substr(assembly_id, 3, 4))
no_expected_seqs = 2
assembly_type_alias = y4[,"assembly_type"]
target1 = sapply(strsplit(y4[,"target1"], "sc"), "[[", 1)
target1_alias = substr(target1, 10, 26)
target1_alias = gsub("HU0", "HU", target1_alias)
target1_alias

target2 = y4[,"target2"]
target2_alias = gsub(" $", "", tolower(y4[,"target2_alias"]))

target2_antigen = substr(y4[,"assembly_id"], 2, 2)
target2_antigen = ifelse(target2_antigen=="E", "EGFR", "TROP2")

all(substr(y4$assembly_id, 19, 30)==y4$decoy_construct)

df_y4 = data.frame("assembly_id"=assembly_id,
                   "assembly_type"=assembly_type,
                   "assembly_type_alias"=assembly_type_alias,
                   "target1"=target1,
                   "target1_alias"=target1_alias,
                   "target2"=target2,
                   "target2_alias"=target2_alias,
                   "target1_antigen"="MUC1",
                   "target2_antigen"=target2_antigen,
                   "no_expected_seqs"=no_expected_seqs)
head(df_y4)


y5 = hold3[[5]]
head(y5)


df_y5 = data.frame("assembly_id"=y5[,"abid"],
                 "assembly_type"="AT00",
                 "assembly_type_alias"="monospec-bivalent",
                 "target1"=y5[,"abid"],
                 "target1_alias"=tolower(y5[,"alias"]),
                 "target2"=NA,
                 "target2_alias"=NA,
                 "target1_antigen"="EGFR",
                 "target2_antigen"=NA,
                 "no_expected_seqs"=2)
head(df_y5)

df_main3 = rbind(df_y1, df_y2, df_y3, df_y4, df_y5)
head(df_main3)

write.csv(df_main3, file = paste0(gsub("\\/.hidden$", "", FL3), 
                                  "/20260526-bispecs-key-order-03.csv"), row.names = F)
