# DATA_DIR from the env file; ~/.upload-app/folders.txt is the old picker's list.

read_folders = function(cfg) {
    if (nzchar(cfg$upload_dir)) return(path.expand(cfg$upload_dir))
    if (!file.exists(cfg$folders_file)) return(character(0))
    lines = trimws(readLines(cfg$folders_file, warn = FALSE))
    unique(path.expand(lines[nzchar(lines) & !startsWith(lines, "#")]))
}
