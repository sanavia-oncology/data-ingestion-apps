# author: Kwame Okrah
# date: 2025-11-14



success_horiz_card = function(main_tp, tp, img_src, bn=NULL) {
  if (is.null(bn)) {
    bn = tags$div()
  }

  res = tags$div(
      class = "card",
      style = "width:420px; height:110px; overflow:hidden; margin-bottom: 20px;",

      tags$div(
        class = "row g-0",
        style = "height:100%;",

        # ---- Image Section (25%) ----
        tags$div(
          class = "col-3",
          style = "padding:5px;",
          tags$img(
            src = img_src,     # <-- replace
            style = "height:100%; width:100%; object-fit:cover;",
            class = "img-fluid rounded-start"
          )
        ),

        # ---- Text Section (75%) ----
        tags$div(
          class = "col-9",
          style = "padding:5px;",
          tags$div(
            class = "card-body",
            style = "padding:0;",  # card-body padding removed to avoid double-padding
            main_tp,
            tp,
            bn
          )
        )
      )
    )

  return(res)
}
