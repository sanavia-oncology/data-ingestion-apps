# author: Kwame Okrah
# date: 2026-03-04

server = function(input, output, session) {

    # step 0
    observe({

        print("Clicked me")

        insertUI(
            selector = "#click_me1",
            where    = "afterEnd",
            ui = tags$div(
                id = "after_click_me",
                tags$p(
                    class = "text-muted",
                    "Thank you!"
                ),
                card(
                    card_image("www/img/data_loaded1.jpg"),
                    card_body(
                        tags$p(
                            class = "fs-6",
                            "Lorem ipsum dolor",
                            tags$br(),
                            "Hello World!"
                        )
                    ),
                    actionButton(
                        inputId = "proceed_button",
                        label   = "Proceed",
                        class   = "btn-primary"
                    )
                )
            )
        )

    }) |> bindEvent(input$click_me1)


    # step 1
    observe({

        # column1 defn.
        insert_me1 = tags$div(
            id = "insert_me1_div1",

            tags$div(
                id = "insert_me1_div2",
                tags$p("Column One", class = "h5 text-primary fw-bold"),
                tags$p("This is column 1.", class = "text-secondary")
            ),

            tags$div(
                tags$p(
                    "The reconstruction of electron density from complex
                     structure factors and its inversion, the computation
                     of complex structure factors from electron density,
                     are amongst the most fundamental and frequent tasks
                     in the course of crystallographic structure
                     determination.

                     Rupp, Bernhard. Biomolecular Crystallography: Principles,
                     Practice, and Application to Structural Biology (p. 439).
                     CRC Press",
                    class = "text-primary"
                )
            ),

            tags$div(
                card(
                    card_header("My Card"),
                    card_body(
                        tags$p(
                            "The reconstruction of electron density from complex
                             structure factors and its inversion, the computation
                             of complex structure factors from electron density,
                             are amongst the most fundamental and frequent tasks
                             in the course of crystallographic structure
                             determination.
                             In practice, the experimental structure factor
                             amplitudes and separately supplied phases from a phasing
                             experiment are needed—a consequence of phase
                             information being lost in the physical detection
                             of the diffracted photons, fittingly termed the phase problem
                             in crystallography. We will lay out in this short but important
                             chapter the mathematical principles of Fourier transforms as
                             far as they are needed to derive the equations used in
                             practical crystallography.
                             Rupp, Bernhard. Biomolecular Crystallography: Principles,
                             Practice, and Application to Structural Biology (p. 439).
                             CRC Press",
                            class = "text-primary"
                        ),
                        tags$b("Bold text here."),
                        br(),
                        "You can also write strings directly."
                    )
                )
            ),

            tags$div(
                actionButton(
                    "column1_button1",
                    "Column1 button",
                    class = "btn-warning"
                )
            )
        )


        # column2 defn.
        insert_me2 = tags$div(
            id = "insert_me2_div1",

            tags$div(
                id    = "insert_me2_div2a",
                class = "row",

                tags$p("Column Two (A)", class = "h5 text-primary fw-bold"),
                tags$p("This is column 2.", class = "text-secondary"),

                tags$div(
                    style = "height: 340px",
                    tags$iframe(
                        src    = paste0("docs/tlgs/", Sys.Date(), "_plate-events-fig.pdf"),
                        width  = "100%",
                        height = "100%"
                    )
                ),

                tags$p(
                    "Each page corresponds to a single plate.",
                    class = "text-secondary"
                )
            ),

            tags$div(
                id    = "insert_me2_div2b",
                class = "row",

                tags$div(
                    class = "row",

                    tags$div(
                        tags$p("Column Two (B)", class = "h5 text-primary fw-bold"),
                        tags$p("Some stuff here")
                    ),

                    tags$div(
                        class = "row",
                        tags$div(
                            tags$p("Column Two (C)", class = "h5 text-secondary fw-bold"),
                            tags$p("Some stuff here")
                        )
                    ),

                    tags$div(
                        class = "row",
                        bslib::layout_column_wrap(
                            actionButton(
                                "back2_click_me",
                                "Back to Click Me",
                                class = "btn-secondary"
                            ),
                            actionButton(
                                "download_report",
                                "Accept & Download Report",
                                class = "btn-warning"
                            )
                        )
                    )
                )
            )
        )


        insert_me = tags$div(
            id    = "main_contents_sub1",
            class = "row",
            tags$div(class = "column", insert_me1),
            tags$div(class = "column", insert_me2)
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
                        tags$div(id = "main_contents_col2_1"),
                        insert_me2
                    )
                )
            )
        )

    }) |> bindEvent(input$proceed_button)

}