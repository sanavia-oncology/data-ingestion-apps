# The folder the uploader syncs: DATA_DIR in ~/.env_data_ingestion_apps, the
# line every app here reads (UPLOAD_DIR overrides it). scripts/fc_sync.sh does the same. ~/.upload-app/folders.txt is the list the
# retired folder-picker launcher wrote; still honoured so those Macs keep
# working.

read_folders = function(cfg) {
    if (nzchar(cfg$upload_dir)) return(path.expand(cfg$upload_dir))
    if (!file.exists(cfg$folders_file)) return(character(0))
    lines = trimws(readLines(cfg$folders_file, warn = FALSE))
    unique(path.expand(lines[nzchar(lines) & !startsWith(lines, "#")]))
}
