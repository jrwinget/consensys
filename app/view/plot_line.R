box::use(
  dplyr[starts_with],
  echarts4r[
    e_charts,
    e_line,
    e_title,
    e_tooltip,
    echarts4rOutput,
    renderEcharts4r
  ],
  shiny[NS, moduleServer, req],
  tidyr[pivot_longer],
)

#' @export
ui <- function(id, plot_height = "350px") {
  ns <- NS(id)
  echarts4rOutput(ns("line"), height = plot_height)
}

#' @export
server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    output$line <- renderEcharts4r({
      req(res())

      results <- res()
      n_individuals <- ncol(results)
      n_rounds <- nrow(results) - 1

      results_df <- as.data.frame(results)
      colnames(results_df) <- paste("Individual", seq_len(n_individuals))
      results_df$Round <- 0:n_rounds

      results_df |>
        pivot_longer(
          cols = starts_with("Individual"),
          names_to = "Individual",
          values_to = "Position"
        ) |>
        e_charts(Round) |>
        e_line(Individual, serie = Position, legend = FALSE) |>
        e_title("Convergence of Judgments Over Time") |>
        e_tooltip(trigger = "axis")
    })
  })
}
