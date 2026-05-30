# author: Kwame Okrah
# date: 2026-03-04

library(shiny)
library(bslib)
library(DT)
library(shinyFiles)

# set app directory
app_dir = "/Users/kwameokrah/shiny/local-apps/transfer-qc/"

# load helper function
helper_funcs_dir = paste0(app_dir, "code")
helper_r_scripts = list.files(helper_funcs_dir, full.names=TRUE, recursive=TRUE)
for (fl in helper_r_scripts) source(fl)

project_fldr = "/Users/kwameokrah/data_depo/flow-cytometry"

proj_paths = get_paths_by_project(project_fldr)
front_page_table(proj_paths, assay_type="fcs")
