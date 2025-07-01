box::use(
  echarts4r[
    e_axis,
    e_bar,
    e_charts,
    e_title,
    e_tooltip,
    echarts4rOutput,
    renderEcharts4r
  ],
  htmlwidgets[JS],
  shiny[NS, moduleServer, req],
)

box::use(
  app/logic/sds_calculations[apply_decision_scheme],
)

#' @export
ui <- function(id, plot_height = "350px") {
  ns <- NS(id)
  echarts4rOutput(ns("heatmap"), height = plot_height)
}

#' @export
server <- function(id, res, y_var, title, decision_mat = NULL) {
  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderEcharts4r({
      req(res())
      res <- res()

      if (!is.null(decision_mat)) {
        req(decision_mat())
        res <- apply_decision_scheme(res, decision_mat())
      }

      data.frame(
        "Alternative" = paste("Alt", seq_along(res)),
        y_var = res
      ) |>
        e_charts(Alternative) |>
        e_bar(
          y_var,
          legend = FALSE,
          itemStyle = list(
            color = JS(
              "function(params) {
                var colors = [
                  '#4F46E5', '#14B8A6', '#0EA5E9', '#F59E0B', '#FF6B6B'
                ];
                return colors[params.dataIndex % colors.length];
              }"
            )
          )
        ) |>
        e_title(title) |>
        e_axis(axis = "y", max = 1, name = y_var) |>
        e_tooltip(
          formatter = JS(
            paste0(
              "function(params) {
                return params.name + '<br/>", y_var, ": ' +
                  (params.value * 100).toFixed(1) + '%';
                }"
            )
          )
        )
    })
  })
}
