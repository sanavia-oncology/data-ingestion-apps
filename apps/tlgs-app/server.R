server = function(input, output, session) {
    notify = function(...) session$sendCustomMessage("tlgs", list(...))
    refresh = function() {
        m = tryCatch(rebuild(), error = function(e) {
            notify(type = "error", message = conditionMessage(e))
            NULL
        })
        if (!is.null(m)) notify(type = "reload", version = m$version)
    }

    observeEvent(input$drop_write, {
        id = input$drop_write$id
        ok = tryCatch({
            tmp = paste0(DROP_FILE, ".tmp")
            writeBin(charToRaw(enc2utf8(input$drop_write$text)), tmp)
            if (!file.rename(tmp, DROP_FILE)) stop("cannot replace ", DROP_FILE)
            notify(type = "written", id = id)
            TRUE
        }, error = function(e) {
            unlink(paste0(DROP_FILE, ".tmp"))
            notify(type = "error", id = id, message = conditionMessage(e))
            FALSE
        })
        if (ok) refresh()
    })

    observe({
        invalidateLater(60000)
        if (stale()) refresh()
    })
}
