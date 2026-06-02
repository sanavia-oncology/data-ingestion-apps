# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {

    observe({
        insert_me1 = tags$div(
            tags$p("Coming Soon!",
                   class="h3 text-secondary fw-bold text-center")
        )
        
        insertUI(
            selector = "#main_contents",
            where    = "afterEnd",
            ui = tags$div(
                id = "main_contents1",
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col",
                        id    = "main_contents_col1",
                        insert_me1
                    ),
                    tags$div(
                        class = "col",
                        id    = "main_contents_col2",
                        tags$div(id = "main_contents_col2_1")
                    )
                )
            )
        )
    })
    
}
