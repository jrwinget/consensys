box::use(
  echarts4r[
    e_charts,
    e_heatmap,
    e_title,
    e_tooltip,
    e_visual_map,
    echarts4rOutput,
    renderEcharts4r
  ],
  htmlwidgets[JS],
  shiny[NS, moduleServer, req],
)

box::use(
  app/logic/sjs_calculations[calculate_sjs_weights],
)

#' @export
ui <- function(id, plot_height = "250px") {
  ns <- NS(id)
  echarts4rOutput(ns("heatmap"), height = plot_height)
}

#' @export
server <- function(id, res, is_displayed = TRUE) {
  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderEcharts4r({
      req(res(), is_displayed)

      initial_positions <- res()[1, ]
      weights <- calculate_sjs_weights(initial_positions)
      n <- nrow(weights)

      heatmap_data <- expand.grid(
        From = factor(paste("Person", 1:n), levels = paste("Person", n:1)),
        To = factor(paste("Person", 1:n), levels = paste("Person", 1:n))
      )
      heatmap_data$Value <- as.vector(t(weights))

      heatmap_data |>
        e_charts(To) |>
        e_heatmap(From, Value) |>
        e_visual_map(
          min = 0,
          max = max(weights),
          calculable = TRUE,
          orient = "horizontal",
          left = "center",
          bottom = "0%",
          inRange = list(
            color = c("#F8FAFC", "#0EA5E9", "#4F46E5")
          )
        ) |>
        e_title("Influence Weight Matrix", left = "center") |>
        e_tooltip(
          formatter = JS(
            "function(params) {
              return params.name + ' influences ' + params.seriesName +
                '<br/>Weight: ' + params.value[2].toFixed(3);
            }"
          )
        )
    })
  })
}
