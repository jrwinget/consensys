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

      result <- res()

      if (is.matrix(result) && !is.null(attr(result, "scheme"))) {
        # SDS decision matrix
        decision_matrix <- result
        n_alts <- attr(decision_matrix, "n_alternatives")
        n_dists <- nrow(decision_matrix)

        # heatmap data for decision matrix
        heatmap_data <- expand.grid(
          Distribution = factor(
            paste("Dist", 1:n_dists),
            levels = paste("Dist", n_dists:1)
          ),
          Alternative = factor(
            paste("Alt", 1:n_alts),
            levels = paste("Alt", 1:n_alts)
          )
        )
        heatmap_data$Value <- as.vector(t(decision_matrix))

        # decision matrix heatmap
        heatmap_data |>
          e_charts(Alternative) |>
          e_heatmap(Distribution, Value) |>
          e_visual_map(
            min = 0,
            max = 1,
            calculable = TRUE,
            orient = "horizontal",
            left = "center",
            bottom = "0%",
            inRange = list(
              color = c("#F8FAFC", "#F59E0B", "#DC2626")
            )
          ) |>
          e_title("Decision Scheme Matrix (D)", left = "center") |>
          e_tooltip(
            formatter = JS(
              "function(params) {
                return params.seriesName + ' → ' + params.name +
                  '<br/>Probability: ' + params.value[2].toFixed(3);
              }"
            )
          )
      } else if (is.matrix(result) && ncol(result) >= 2) {
        # SJS results, influence weight matrix
        initial_positions <- result[1, ]
        weights <- calculate_sjs_weights(initial_positions)
        n <- nrow(weights)

        # proper labeling and data structure for SJS weights
        heatmap_data <- expand.grid(
          From = factor(paste("Person", 1:n), levels = paste("Person", n:1)),
          To = factor(paste("Person", 1:n), levels = paste("Person", 1:n))
        )
        heatmap_data$Value <- as.vector(t(weights))

        # influence weight heatmap
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
      } else {
        # fallback, empty plot with message
        data.frame(x = 1, y = 1, z = 0) |>
          e_charts(x) |>
          e_heatmap(y, z) |>
          e_title("No matrix data available", left = "center")
      }
    })
  })
}
