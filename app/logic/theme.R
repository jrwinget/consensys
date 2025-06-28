box::use(
  bslib[bs_theme],
)

#' Create consistent theme for the app
#' @export
create_app_theme <- function() {
  bs_theme(
    version = 5,
    # Custom color palette
    primary = "#4F46E5",    # Indigo for cognition
    secondary = "#64748B",  # Slate gray for neutrality
    success = "#14B8A6",    # Teal for system integrity
    info = "#0EA5E9",       # Sky blue for information
    warning = "#F59E0B",    # Amber for warnings
    danger = "#FF6B6B",     # Soft coral for empathy/alerts
    light = "#F8FAFC",      # Very light gray
    dark = "#1E293B",       # Dark slate
    # Base colors
    fg = "#334155",         # Slate 700 for text
    bg = "#FFFFFF",         # White background
    # Typography
    "font-family-base" = "'Roboto', -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
    "font-size-base" = "1rem",
    "line-height-base" = "1.5",
    "headings-font-weight" = "600",
    # Spacing - REDUCED for tighter layouts
    "spacer" = "0.75rem",  # Reduced from 1.5rem
    "card-spacer-y" = "1rem",  # Reduced from 1.5rem
    "card-spacer-x" = "1rem",  # Reduced from 1.5rem
    # Card styling
    "card-border-radius" = "0.5rem",
    "card-border-width" = "1px",
    "card-border-color" = "rgba(0, 0, 0, 0.05)",
    "card-cap-bg" = "#F8FAFC",
    # Component specific
    "input-border-radius" = "0.375rem",
    "btn-border-radius" = "0.375rem",
    "accordion-border-radius" = "0.5rem"
  )
}
