box::use(
  dplyr[starts_with],
  echarts4r,
  htmlwidgets[JS],
  shiny[NS, moduleServer, req],
  tidyr[pivot_longer],
)

#' @export
ui <- function(id, plot_height = "350px") {
  ns <- NS(id)
  echarts4r$echarts4rOutput(ns("line"), height = plot_height)
}

#' @export
server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    output$line <- echarts4r$renderEcharts4r({
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
        echarts4r$e_charts(Round, timeline = FALSE) |>
        echarts4r$e_line(
          Position,
          smooth = TRUE,
          lineStyle = list(width = 3),
          itemStyle = list(
            color = JS(
              "function(params) {
                var colors = [
                  '#4F46E5',
                  '#14B8A6',
                  '#0EA5E9',
                  '#F59E0B',
                  '#FF6B6B',
                  '#64748B'
                ];
                return colors[params.seriesIndex % colors.length];
              }"
            )
          )
        ) |>
        echarts4r$e_title("Convergence of Judgments Over Time") |>
        echarts4r$e_tooltip(
          trigger = "axis",
          formatter = JS(
            "function(params) {
              var result = 'Round ' + params[0].name + '<br/>';
              params.forEach(function(item) {
                result += '<span style=\"
                  display:inline-block;
                  margin-right:4px;
                  border-radius:10px;
                  width:10px;
                  height:10px;
                  background-color:' + item.color + ';
                \"></span>';
                result += item.seriesName + ': ' + 
                  item.data.value.toFixed(1) + '<br/>';
              });
              return result;
            }"
          )
        ) |>
        echarts4r$e_legend(show = TRUE, top = "bottom") |>
        echarts4r$e_x_axis(
          name = "Round",
          nameLocation = "center",
          nameGap = 25
        ) |>
        echarts4r$e_y_axis(
          name = "Position",
          nameLocation = "center",
          nameGap = 40,
          min = 0,
          max = 100
        )
    })
  })
}
