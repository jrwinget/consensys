box::use(
  bsicons[bs_icon],
  bslib[bs_theme, nav_item, nav_menu, nav_panel, nav_spacer, page_navbar],
  shiny[NS, moduleServer],
)

box::use(
  app/view/intro_tab,
  app/view/sds_tab,
  app/view/simulation_tab,
  app/view/sjs_tab,
)

# TODO: add documentation tab

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    title = "Group Decision Making Models",
    theme = bs_theme(version = 5, bootswatch = "morph"),
    nav_panel(
      "Introduction",
      intro_tab$ui(ns("intro"))
    ),
    nav_panel(
      "Social Decision Scheme",
      sds_tab$ui(ns("sds"))
    ),
    nav_panel(
      "Social Judgment Scheme",
      sjs_tab$ui(ns("sjs"))
    ),
    nav_panel(
      "Simulation",
      simulation_tab$ui(ns("simulation"))
    ),
    nav_spacer(),
    nav_menu(
      "More",
      title = "Additional Resources",
      align = "right",
      nav_item(
        "GitHub",
        icon = bs_icon("github"),
        href = "https://github.com/jrwinget/schematic"
      ),
      nav_item(
        "Documentation",
        icons = bs_icon("book"),
        href = "jrwinget.github.io/schematic/"
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    sds_tab$server("sds")
    sjs_tab$server("sjs")
    simulation_tab$server("simulation")
  })
}
