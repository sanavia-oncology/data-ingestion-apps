# tlgs-app

Kwame's flow cytometry portal, local copy: shows the studies and writes the drop list.

Needs R with `shiny`, `bslib`, `jsonlite`, `dotenv`.

## Run

```
R -e "shiny::runApp('apps/tlgs-app', port = 3012)"
```

Reads `DATA_DIR` (the FC data folder) and optional `TLGS_DIR` from `~/.env_data_ingestion_apps`, or from the
environment when `DATA_DIR` is already set.

Writes:

- `99-flow-adam-database/` next to `DATA_DIR` (or `TLGS_DIR`): the study CSVs, summary table and `manifest.json`
- `DATA_DIR/00-drop-folder/to-drop-wells_cummulative.csv`: the drop list

## Updating Kwame's files

- Portal: replace `www/portal.html`.
- R: replace `code/preprocess-scripts-logic.R`.
