box::use(
  bsicons[bs_icon],
  bslib[card, card_header, card_body],
  shiny[tags],
)

#' Create a consistently styled card
#' @export
content_card <- function(
    title = NULL,
    ...,
    header_content = NULL,
    class = "content-card", 
    header_class = "card-header",
    body_class = "card-body") {
  card(
    class = class,
    if (!is.null(title) || !is.null(header_content)) {
      card_header(
        class = header_class,
        title,
        header_content
      )
    },
    card_body(
      class = body_class,
      ...
    )
  )
}

#' Create a value box with consistent styling
#' @export
metric_box <- function(
    title, 
    value, 
    subtitle = NULL, 
    icon_name = NULL, 
    theme = "primary") {
  bg_classes <- list(
    primary = "bg-primary",
    success = "bg-success", 
    info = "bg-info",
    danger = "bg-danger",
    warning = "bg-warning"
  )
  
  bg_class <- bg_classes[[theme]] %||% "bg-primary"
  
  tags$div(
    class = paste("card border-0 shadow-sm value-box-theme", bg_class),
    style = "height: 100%;",
    tags$div(
      class = "card-body p-3 d-flex flex-column justify-content-between",
      tags$div(
        tags$div(
          class = "d-flex justify-content-between align-items-start mb-2",
          tags$h6(
            class = "mb-0 text-white-50", 
            style = "font-weight: 500; opacity: 0.9;",
            title
          ),
          if (!is.null(icon_name)) tags$div(
            style = "opacity: 0.8;",
            bs_icon(icon_name, class = "fa-lg")
          )
        ),
        tags$h3(
          class = "mb-1", 
          style = "font-weight: 600;",
          value
        )
      ),
      if (!is.null(subtitle)) tags$small(
        style = "opacity: 0.9; font-weight: 400;",
        subtitle
      )
    )
  )
}

#' Create a two-column settings layout
#' @export
settings_layout <- function(...) {
  layout_columns(
    col_widths = c(6, 6),
    fill = FALSE,
    class = "bslib-gap-spacing",
    ...
  )
}

#' Create a properly spaced tab layout
#' @export
tab_layout <- function(...) {
  layout_column_wrap(
    width = 1,
    fill = FALSE,
    class = "bslib-gap-spacing",
    ...
  )
}
