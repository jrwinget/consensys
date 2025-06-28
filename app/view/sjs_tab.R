box::use(
  bslib[
    card,
    card_body,
    card_header,
    input_switch,
    input_task_button,
    layout_column_wrap,
    layout_columns
  ],
  dplyr[starts_with],
  echarts4r,
  purrr[map, map_dbl],
  shiny,
  tidyr[pivot_longer],
)

box::use(
  app/logic/sjs_calculations[
    calculate_sjs_weights,
    simulate_sjs_process
  ],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    # description --------------------------------------------------------------
    card(
      card_header("Social Judgment Scheme (SJS) Model"),
      card_body(
        shiny$tags$p(
          "The SJS model describes how group members influence each other's
          judgments based on their relative positions on a continuous scale."
        )
      )
    ),
    # settings -----------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Model Parameters"),
        card_body(
          shiny$numericInput(
            ns("n_individuals"),
            "Number of Individuals:",
            value = 5,
            min = 2,
            max = 10
          ),
          shiny$numericInput(
            ns("n_rounds"),
            "Number of Rounds:",
            value = 5,
            min = 1,
            max = 20
          ),
          input_switch(
            ns("show_weights"),
            "Show Influence Weights",
            value = FALSE
          ),
          input_task_button(
            ns("simulate"),
            "Run Simulation"
          )
        )
      ),
      card(
        card_header("Initial Positions"),
        card_body(
          shiny$uiOutput(ns("position_inputs")),
          shiny$tags$p("Set initial positions (0-100) for each individual")
        )
      )
    ),
    # results ------------------------------------------------------------------
    card(
      card_header("Simulation Results"),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          echarts4r$echarts4rOutput(ns("convergence_plot")),
          echarts4r$echarts4rOutput(ns("weight_matrix"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # dynamic inputs -----------------------------------------------------------
    output$position_inputs <- shiny$renderUI({
      shiny$req(input$n_individuals)

      map(
        seq_len(input$n_individuals),
        ~ shiny$sliderInput(
          inputId = session$ns(paste0("pos_", .x)),
          label = paste("Individual", .x),
          min = 0,
          max = 100,
          value = sample(20:80, 1)
        )
      )
    })

    # reactive values ----------------------------------------------------------
    sim_results <- shiny$reactive({
      shiny$req(input$simulate, input$n_individuals)

      initial_positions <- map_dbl(
        seq_len(input$n_individuals),
        ~ input[[paste0("pos_", .x)]]
      )

      simulate_sjs_process(initial_positions, input$n_rounds)
    })

    # plots --------------------------------------------------------------------
    output$convergence_plot <- echarts4r$renderEcharts4r({
      shiny$req(sim_results())

      results_df <- as.data.frame(sim_results())
      colnames(results_df) <- paste("Individual", seq_len(input$n_individuals))
      results_df$Round <- 0:input$n_rounds

      plot_data <- results_df |>
        pivot_longer(
          cols = starts_with("Individual"),
          names_to = "Individual",
          values_to = "Position"
        )

      plot_data |>
        echarts4r$e_charts(Round) |>
        echarts4r$e_line(Position, group = Individual) |>
        echarts4r$e_tooltip(trigger = "axis") |>
        echarts4r$e_legend(show = TRUE) |>
        echarts4r$e_title("Convergence of Judgments Over Time") |>
        echarts4r$e_x_axis(name = "Round") |>
        echarts4r$e_y_axis(name = "Position", min = 0, max = 100)
    })

    output$weight_matrix <- echarts4r$renderEcharts4r({
      shiny$req(sim_results(), input$show_weights)

      initial_positions <- sim_results()[1, ]
      weights <- calculate_sjs_weights(initial_positions)

      weight_df <- as.data.frame(weights)
      colnames(weight_df) <- paste("To", seq_len(weights))
      weight_df$From <- paste("From", seq_len(weights))

      weight_df |>
        echarts4r$e_charts(From) |>
        echarts4r$e_heatmap(To, value = weights) |>
        echarts4r$e_visual_map(min = 0, max = 1) |>
        echarts4r$e_title("Influence Weight Matrix") |>
        echarts4r$e_tooltip()
    })
  })
}
