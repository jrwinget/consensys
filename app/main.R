box::use(
  bsicons[bs_icon],
  bslib[nav_item, nav_menu, nav_panel, nav_spacer, page_navbar, tooltip],
  shiny[NS, moduleServer, tags],
)

box::use(
  app/logic/theme[create_app_theme],
  app/view/intro_tab,
  app/view/sds_tab,
  app/view/sim_tab,
  app/view/sjs_tab,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    title = tags$span(
      "Group Decision Making Models",
      tooltip(
        bs_icon("info-circle"),
        "Interactive tools for understanding group decision processes",
        placement = "bottom"
      )
    ),
    theme = create_app_theme(),
    # progressive disclosure: start with intro
    nav_panel(
      title = "Introduction",
      value = "intro",
      icon = bs_icon("house-door"),
      intro_tab$ui(ns("intro"))
    ),
    nav_panel(
      title = "Social Decision Scheme",
      value = "sds",
      icon = bs_icon("diagram-3"),
      sds_tab$ui(ns("sds"))
    ),
    nav_panel(
      title = "Social Judgment Scheme",
      value = "sjs",
      icon = bs_icon("sliders"),
      sjs_tab$ui(ns("sjs"))
    ),
    nav_panel(
      title = "Simulation",
      value = "simulation",
      icon = bs_icon("play-circle"),
      sim_tab$ui(ns("sim"))
    ),
    nav_spacer(),
    nav_menu(
      title = bs_icon("three-dots"),
      align = "right",
      nav_item(
        tags$a(
          href = "https://github.com/jrwinget/consensys",
          tags$span(bs_icon("github"), "About")
        )
      ),
      nav_item(
        tags$a(
          href = "https://github.com/jrwinget/consensys/issues",
          tags$span(bs_icon("bug"), "Report Issue")
        )
      )
    ),
    # visual Hierarchy: subtle but clear branding
    window_title = "Consensys | Group Decision Models"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    intro_tab$server("intro")
    sds_tab$server("sds")
    sjs_tab$server("sjs")
    sim_tab$server("sim")
  })
}
