box::use(
  bslib[bs_theme],
)

#' Create consistent theme for the app
#' @export
create_app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#4F46E5", # indigo for cognition
    secondary = "#64748B", # slate gray for neutrality
    success = "#14B8A6", # teal for system integrity
    info = "#0EA5E9", # sky blue for information
    warning = "#F59E0B", # amber for warnings
    danger = "#FF6B6B", # soft coral for empathy/alerts
    light = "#F8FAFC", # very light gray
    dark = "#1E293B", # dark slate
    fg = "#334155", # slate 700 for text
    bg = "#FFFFFF", # white background
    # typography
    "font-family-base" = paste(
      "'Roboto'",
      "-apple-system",
      "BlinkMacSystemFont",
      "'Segoe UI'",
      "'Helvetica Neue'",
      "Arial",
      "sans-serif",
      sep = ", "
    ),
    "font-size-base" = "1rem",
    "line-height-base" = "1.5",
    "headings-font-weight" = "600",
    # spacing
    "spacer" = "0.75rem",
    "card-spacer-y" = "1rem",
    "card-spacer-x" = "1rem",
    # card styling
    "card-border-radius" = "0.5rem",
    "card-border-width" = "1px",
    "card-border-color" = "rgba(0, 0, 0, 0.05)",
    "card-cap-bg" = "#F8FAFC",
    # component specific
    "input-border-radius" = "0.375rem",
    "btn-border-radius" = "0.375rem",
    "accordion-border-radius" = "0.5rem"
  )
}
