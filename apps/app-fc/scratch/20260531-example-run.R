# author: Kwame Okrah
# date: 2026-03-04

# set app directory
app_dir = "/Users/kwameokrah/claude_code/shiny/R/data-ingestion-apps/apps/app-fc/"

# load helper function
helper_funcs_dir = paste0(app_dir, "code/helper_funcs")
helper_r_scripts = list.files(helper_funcs_dir, full.names=TRUE, recursive=TRUE)
for (fl in helper_r_scripts) source(fl)

# set data dir
project_fldr = "/Users/kwameokrah/data_depo/flow-cytometry"
selected_assay = "fcs"

# project_paths
project_paths = get_paths_by_project2(project_fldr)

# front_page_table2
df = front_page_table2(project_paths, assay_type="fcs")
df

# select a project
selected_project = "2026-03-31_KO-Demo"

# read in fcs files
path_list = project_paths[[selected_project]]
paths = c(path_list[["assay_data"]], path_list[["qc_report"]])

res = read_data(paths)

pinfos = res[["pinfos"]]
fcs_files = res[["fcs_files"]]

op = par(mfrow=c(2, 3))
plot_plate_events(pinfos, is_gated = FALSE, fig_path = NULL)
events_boxplot(pinfos$Result)
par(op)

pinfos = input_react_vals$pinfos
fcs_files = input_react_vals$fcs_files

ref = rownames(pinfos)
refl = split(ref, pinfos$Platename)

nsel = 1
fref = lapply(refl, function(x) {
    if (length(x) <= nsel) {
        return(x)
    }else{
        return(x[1:nsel])
    }
})

sel_refs = unlist(fref)
fcs_refs = fcs_files[sel_refs]

optimal_vertices_res = compute_optimal_vertices(fcs_refs, default_ch)

# gate ref samples
verts = optimal_vertices_res[["optim_verts_list"]]
default_ch = optimal_vertices_res[["default_ch"]]

# MFI channel comes from the project metadata
default_ch["ab+", "x_ch"] = pinfos[["mfi_channel"]][1]

gres_list_refs = list()
n_total = length(fcs_refs)

for (i in seq_along(names(fcs_refs))) {
    k = names(fcs_refs)[i]
    fcs = fcs_refs[[k]]
    mat = flowCore::exprs(fcs)
    result = tryCatch({
        gate2_viability(mat, default_ch, verts)
    }, error = function(e) {
        log_error(paste0("gate2_viability [", k, "]"), e)
        NA
    })
    gres_list_refs[[k]] = result
}


sample_profile_plot(1, gres_list_refs, FALSE)


#-------#
# gate all samples
gres_list = list()
n_total = length(fcs_files)

for (i in seq_along(names(fcs_files))) {
    k = names(fcs_files)[i]
    fcs = fcs_files[[k]]
    mat = flowCore::exprs(fcs)
    result = tryCatch({
        gate2_viability(mat, default_ch, verts)
    }, error = function(e) {
        log_error(paste0("gate2_viability [", k, "]"), e)
        NA
    })
    gres_list[[k]] = result
}

results_table = make_results_table(gres_list, pinfos, author_gating="KO")

tab = summary_viable_events(results_table)
tab

# plot total viable events
plot_plate_events(results_table, is_gated = TRUE)

# plot mfi
plot_mfi(results_table)

# events boxplot
events_boxplot(results_table$Result, results_table[, "/intact/singlet/viable"])


