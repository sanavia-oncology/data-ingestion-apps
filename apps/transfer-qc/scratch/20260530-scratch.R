# author: Kwame Okrah
# date: 2026-03-04

library(shiny)
library(bslib)
library(DT)
library(shinyFiles)

# set app directory
app_dir = "/Users/kwameokrah/claude_code/shiny/R/data-ingestion-apps/apps/transfer-qc/"

# load helper function
helper_funcs_dir = paste0(app_dir, "code")
helper_r_scripts = list.files(helper_funcs_dir, full.names=TRUE, recursive=TRUE)
for (fl in helper_r_scripts) source(fl)

project_fldr = "/Users/kwameokrah/data_depo/flow-cytometry"
selected_assay = "fcs"

project_paths = get_paths_by_project(project_fldr)
front_page_table(project_paths, assay_type="fcs")

selected_project = "2026-03-31_KO-Demo"

file_paths = project_paths[[selected_project]]
pinfo_csv_paths = file_paths[["pinfos"]]
assay_file_paths = file_paths[["assay_data"]]

pinfo_sheets = read_pinfo_csvs(pinfo_csv_paths)
assay_data = read_assay_data(assay_file_paths, selected_assay)

res = perform_plateinfo_checks(pinfo_sheets)
checks_summary = res[["checks_summary"]]
checks_detailed = res[["checks_detailed"]]
rm("res")


PAR_STRING_L = strsplit(assay_data$PAR_STRING, ";")

uchannels = unique(unlist(PAR_STRING_L))
uchannels = uchannels[grep("^B|^R|^Y|^V", uchannels)]
o = order(uchannels)

o = order(sapply(strsplit(uchannels, "-"), "[[", 2)!="H",
          sapply(strsplit(uchannels, "-"), "[[", 1))

uchannels = uchannels[o]

ch_check_mat = t(sapply(PAR_STRING_L, function(x) uchannels %in% x))
dim(ch_check_mat)
ch_check_mat = ch_check_mat + 0

colnames(ch_check_mat) = uchannels



