// Relays drop-file writes from the portal iframe to Shiny, and Shiny's "tlgs" messages back to the iframe.
window.addEventListener("message", e => {
    if (e.origin === location.origin && e.data && e.data.type === "drop-write")
        Shiny.setInputValue("drop_write", {id: e.data.id, text: e.data.text}, {priority: "event"});
});
Shiny.addCustomMessageHandler("tlgs", m => document.getElementById("portal").contentWindow.postMessage(m, location.origin));
