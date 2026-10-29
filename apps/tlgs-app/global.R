# TLGs portal, local copy: Kwame's portal.html with the drop list written to DATA_DIR.
# Run: R -e "shiny::runApp('apps/tlgs-app', port = 3012)"
# Reads DATA_DIR and optional TLGS_DIR from ~/.env_data_ingestion_apps, or from the environment when DATA_DIR is already set.

library(shiny)
library(bslib)

for (fl in list.files("code", pattern = "\\.R$", full.names = TRUE)) source(fl)

if (!nzchar(Sys.getenv("DATA_DIR"))) dotenv::load_dot_env("~/.env_data_ingestion_apps")
DATA_DIR = Sys.getenv("DATA_DIR")
TLGS_DIR = Sys.getenv("TLGS_DIR", file.path(dirname(DATA_DIR), "99-flow-adam-database"))
DROP_DIR = file.path(DATA_DIR, "00-drop-folder")
DROP_FILE = file.path(DROP_DIR, "to-drop-wells_cummulative.csv")
dir.create(TLGS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DROP_DIR, recursive = TRUE, showWarnings = FALSE)

PORTAL_DIR = patch_portal("www/portal.html", "www/local.js", tempfile("portal"))
addResourcePath("portal", PORTAL_DIR)
addResourcePath("data", TLGS_DIR)
addResourcePath("drop", DROP_DIR)

building = FALSE
rebuild = function() {
    if (building) return(NULL)
    building <<- TRUE
    on.exit(building <<- FALSE)
    build_sana(DATA_DIR, DATA_DIR, TLGS_DIR)
}
stale = function() {
    m = read_manifest(TLGS_DIR)
    is.null(m) || !identical(m$fingerprint, fingerprint(DATA_DIR, DATA_DIR))
}
if (stale()) try(rebuild())
