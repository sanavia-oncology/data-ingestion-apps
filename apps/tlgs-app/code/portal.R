# Patches Kwame's portal.html for the local app: patch_portal(html, js, out) writes
# <out>/index.html and <out>/local.js and returns out. Stops if an anchor is missing.

patch_portal = function(html, js, out) {
    x = readChar(html, file.size(html), useBytes = TRUE)
    swap = function(from, to, n = 1) {
        hits = sum(gregexpr(from, x, fixed = TRUE, useBytes = TRUE)[[1]] > 0)
        if (hits != n) stop(html, ": expected ", n, " of ", from, ", found ", hits)
        x <<- gsub(from, to, x, fixed = TRUE, useBytes = TRUE)
    }
    cart = 'id="cartMsg" role="status"></span></div>'
    swap('id="toSelect">', 'id="toSelect" hidden>')
    swap('\nshowSelect();\n', '\nshowCover();\n')
    swap('(function restore(){', '(function restore(){ return;')
    swap(cart, paste0(cart, '<div class="cart drop" aria-label="Drop selected wells"><span class="drop-title">Drop selected wells</span>',
                      '<button type="button" class="btn" id="dropMode" aria-pressed="false">Select to drop</button>',
                      '<span class="cart-info" id="dropInfo">To drop: 0 wells</span>',
                      '<button type="button" class="btn" id="dropHide" aria-pressed="false" disabled>Hide</button>',
                      '<button type="button" class="btn" id="dropClr" disabled>Clear</button>',
                      '<button type="button" class="btn" id="dropDl" disabled>Update drop list</button>',
                      '<span class="cart-msg" id="dropMsg" role="status"></span></div>'))
    swap('function renderDrop(){} /* drop-well tools removed */', 'function renderDrop(){ _renderDropUnused(); }')
    swap('function markDrops(){}', 'function markDrops(){ _markDropsUnused(); }')
    swap('["wid","Plate information"],["wells","Well-level data"]];', '["wid","Plate information"],["wells","Well-level data"],["drop","Drop list"]];')
    swap('${wellsPane()}', '${wellsPane()}${dropPane()}', n = 2)
    swap('</body>', '<script src="local.js"></script></body>')
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    writeChar(x, file.path(out, "index.html"), eos = NULL, useBytes = TRUE)
    if (!file.copy(js, file.path(out, "local.js"), overwrite = TRUE)) stop("cannot copy ", js)
    out
}
