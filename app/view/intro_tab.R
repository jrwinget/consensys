box::use(
  bslib[
    card,
    card_body,
    card_header,
    layout_column_wrap
  ],
  shiny[tags],
)

#' @export
ui <- function(id) {
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header("Understanding Group Decision Making"),
      card_body(
        tags$h4("About this Application"),
        tags$p(
          "This application provides interactive tools for understanding and
          applying Social Decision Scheme (SDS) and Social Judgment Scheme (SJS)
          theories - frameworks for understanding how groups integrate
          individual judgments to reach collective decisions."
        ),
        tags$h4("Key Theories"),
        tags$ul(
          tags$li(
            tags$strong("Social Decision Scheme Theory:"),
            "Focuses on the rules and mathematical models by which individual
            preferences are aggregated into group decisions."
          ),
          tags$li(
            tags$strong("Social Judgment Scheme Theory:"),
            "Examines how individual judgments are influenced by group
            interactions, with emphasis on how discussion and persuasion shift
            individual attitudes."
          )
        ),
        tags$h4("How to Use This App"),
        tags$p("This application allows you to:"),
        tags$ul(
          tags$li("Explore SDS models with different decision rules"),
          tags$li("Simulate SJS processes for continuous judgments"),
          tags$li("Run simulations with different group compositions"),
          tags$li(
            "Visualize the impact of different decision schemes on outcomes"
          )
        )
      )
    )
  )
}
