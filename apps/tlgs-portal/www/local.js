// Local TLGs build, loaded after the portal's own scripts: study data from ../data/, drop file through the Shiny page.
(() => {
    const waiting = new Map();
    let seq = 0;
    const send = msg => new Promise((ok, no) => {
        const id = ++seq;
        waiting.set(id, {ok, no});
        parent.postMessage({...msg, id}, location.origin);
    });

    const shim = {
        queryPermission: async () => "granted",
        requestPermission: async () => "granted",
        getDirectoryHandle: async () => ({
            getFileHandle: async (name, opts) => ({
                getFile: async () => {
                    const r = await fetch("../drop/" + name, {cache: "no-store"});
                    if (r.status === 404 && opts && opts.create) return new File([""], name);
                    if (r.status === 404) throw Object.assign(new Error(name + " not found"), {name: "NotFoundError"});
                    if (!r.ok) throw new Error(name + ": " + r.status);
                    return new File([await r.blob()], name, {lastModified: Date.parse(r.headers.get("Last-Modified")) || Date.now()});
                },
                createWritable: async () => {
                    let text = "";
                    return {write: async t => { text = t; }, close: () => send({type: "drop-write", text})};
                }
            })
        })
    };

    window.dropPane = () => `<div data-pane="drop" ${st.view === "drop" ? "" : "hidden"}><div class="panel-head"><div><h2>Drop list</h2><p id="dlInfo"></p></div><div class="cart"><button type="button" class="btn" data-dlref>Refresh</button><button type="button" class="btn" id="dlRemove" data-dlrm disabled>Remove selected rows</button><span class="cart-msg" id="dlMsg" role="status"></span></div></div><div class="wfilters" id="dlFilters"></div><div class="tscroll" id="dropListBox"></div><div class="pager" id="dlPager" style="margin-top:10px"></div></div>`;

    const orig = PORTAL.load;
    PORTAL.load = function(raws, meta) {
        const open = $("#studyView").hidden ? null : PROJ;
        orig.call(this, raws, meta);
        window.FOLDER = {dir: shim, root: "the data folder"};
        const errs = [...document.querySelectorAll("#errs li")].map(li => li.textContent);
        if (errs.length) $("#coverMeta").textContent = errs.join(" · ");
        if (open && RAWS[open]) openStudy(open);
    };

    async function load() {
        $("#coverMeta").textContent = "Loading study data…";
        try {
            const r = await fetch("../data/manifest.json", {cache: "no-store"});
            if (!r.ok) throw new Error("manifest.json: " + r.status);
            const studies = Object.values((await r.json()).studies || {});
            const files = await Promise.all(studies.map(async s => {
                const f = await fetch("../data/" + s.file, {cache: "no-cache"});
                if (!f.ok) throw new Error(s.file + ": " + f.status);
                return new File([await f.blob()], s.file.split("/").pop(), {type: "text/csv"});
            }));
            if (!files.length) { $("#coverMeta").textContent = "No study data yet"; return; }
            const dt = new DataTransfer();
            files.forEach(f => dt.items.add(f));
            $("#dirInput").files = dt.files;
            $("#dirInput").dispatchEvent(new Event("change"));
        } catch (e) {
            $("#coverMeta").textContent = "Could not load study data: " + e.message;
        }
    }

    addEventListener("message", e => {
        if (e.origin !== location.origin || !e.data) return;
        const m = e.data, w = waiting.get(m.id);
        if (m.type === "reload") load();
        else if (w) {
            waiting.delete(m.id);
            if (m.type === "written") w.ok();
            // the portal reports e.name, not e.message
            else w.no(Object.assign(new Error(m.message), {name: m.message}));
        } else if (m.type === "error") $("#dropMsg").textContent = $("#coverMeta").textContent = m.message;
    });

    load();
})();
