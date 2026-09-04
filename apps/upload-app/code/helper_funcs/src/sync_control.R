# The folder list fc_sync.sh uploads, written by
# apps/upload-app/scripts/fc_sync_start.command. The app only reads it.

read_folders = function(cfg) {
    if (!file.exists(cfg$folders_file)) return(character(0))
    lines = trimws(readLines(cfg$folders_file, warn = FALSE))
    unique(path.expand(lines[nzchar(lines) & !startsWith(lines, "#")]))
}
