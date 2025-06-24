box::use(
  bslib[
    card,
    card_body,
    card_header,
    layout_column_wrap,
    layout_columns
  ],
  dplyr[starts_with],
  echarts4r[
    e_bar,
    e_charts,
    e_line,
    e_title,
    e_tooltip,
    echarts4rOutput,
    renderEcharts4r
  ],
  purrr[map, map_dbl],
  shiny,
  stats[sd],
  tidyr[pivot_longer],
)

box::use(
  app/logic/sds_calculations[apply_decision_scheme, generate_decision_matrix],
  app/logic/sjs_calculations[simulate_sjs_process],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    # description --------------------------------------------------------------
    card(
      card_header("Model Comparison Simulation"),
      card_body(
        shiny$tags$p(
          "Compare how SDS and SJS models handle the same initial group
          configuration."
        )
      )
    ),
    # settings -----------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("SDS Settings"),
        card_body(
          shiny$selectInput(
            ns("sds_scheme"),
            "Decision Scheme:",
            choices = c(
              "Majority Rule" = "majority",
              "Proportional" = "proportional",
              "Two-Thirds Majority" = "twothirds",
              "Unanimity" = "unanimity",
              "Truth-Wins" = "truth"
            )
          ),
          shiny$numericInput(
            ns("n_alternatives"),
            "Number of Alternatives:",
            value = 3,
            min = 2,
            max = 5
          ),
          shiny$uiOutput(ns("alternative_weights"))
        )
      ),
      card(
        card_header("SJS Settings"),
        card_body(
          shiny$numericInput(
            ns("n_rounds"),
            "Number of Rounds:",
            value = 5,
            min = 1,
            max = 20
          ),
          shiny$numericInput(
            ns("n_individuals"),
            "Number of Individuals:",
            value = 5,
            min = 2,
            max = 10
          ),
          shiny$uiOutput(ns("initial_positions"))
        )
      )
    ),
    # results ------------------------------------------------------------------
    card(
      card_header("Comparison Results"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("SDS Model Outcome"),
            card_body(echarts4rOutput(ns("sds_plot")))
          ),
          card(
            card_header("SJS Model Outcome"),
            card_body(echarts4rOutput(ns("sjs_plot")))
          )
        ),
        card(
          card_header("Model Comparison Metrics"),
          card_body(echarts4rOutput(ns("comparison_metrics")))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # dynamic inputs -----------------------------------------------------------
    output$alternative_weights <- shiny$renderUI({
      shiny$req(input$n_alternatives)

      map(
        seq_len(input$n_alternatives),
        ~ shiny$sliderInput(
          inputId = session$ns(paste0("alt_weight_", .x)),
          label = paste("Alternative", .x, "Weight"),
          min = 0,
          max = 1,
          value = 1 / input$n_alternatives
        )
      )
    })

    output$initial_positions <- shiny$renderUI({
      shiny$req(input$n_individuals)

      map(
        seq_len(input$n_individuals),
        ~ shiny$sliderInput(
          inputId = session$ns(paste0("init_pos_", .x)),
          label = paste("Individual", .x, "Position"),
          min = 0,
          max = 100,
          value = sample(20:80, 1)
        )
      )
    })

    # reactive values ----------------------------------------------------------
    sds_results <- shiny$reactive({
      shiny$req(input$n_alternatives)

      weights <- map_dbl(
        seq_len(input$n_alternatives),
        ~ input[[paste0("alt_weight_", .x)]] %||% (1 / input$n_alternatives)
      )

      weights <- weights / sum(weights)

      res <- generate_decision_matrix(input$sds_scheme, input$n_alternatives)
      apply_decision_scheme(weights, res)
    })

    sjs_results <- shiny$reactive({
      shiny$req(input$n_individuals)

      initial_positions <- map_dbl(
        seq_len(input$n_individuals),
        ~ input[[paste0("init_pos_", .x)]] %||% 50
      )

      simulate_sjs_process(initial_positions, input$n_rounds)
    })

    # plots --------------------------------------------------------------------
    output$sds_plot <- renderEcharts4r({
      shiny$req(sds_results())

      data.frame(
        Alternative = paste("Alt", seq_along(sds_results())),
        Probability = sds_results()
      ) |>
        e_charts(Alternative) |>
        e_bar(Probability) |>
        e_title("Final Decision Probabilities") |>
        e_tooltip()
    })

    output$sjs_plot <- renderEcharts4r({
      shiny$req(sjs_results())

      results_df <- as.data.frame(sjs_results())
      colnames(results_df) <- paste("Individual", seq_len(input$n_individuals))
      results_df$Round <- 0:input$n_rounds

      plot_data <- results_df |>
        pivot_longer(
          cols = starts_with("Individual"),
          names_to = "Individual",
          values_to = "Position"
        )

      plot_data |>
        e_charts(Round) |>
        e_line(Position, groupBy = "Individual") |>
        e_title("Position Convergence") |>
        e_tooltip(trigger = "axis")
    })

    output$comparison_metrics <- renderEcharts4r({
      shiny$req(sds_results(), sjs_results())

      sds_entropy <- -sum(sds_results() * log(sds_results()))
      sjs_final_spread <- sd(sjs_results()[nrow(sjs_results()), ])

      plot_data <- data.frame(
        Metric = c("Decision Entropy", "Final Position Spread"),
        SDS = c(sds_entropy, NA),
        SJS = c(NA, sjs_final_spread)
      ) |>
        pivot_longer(
          cols = c("SDS", "SJS"),
          names_to = "Model",
          values_to = "Value"
        )

      plot_data |>
        e_charts(Metric) |>
        e_bar(Value, groupBy = "Model") |>
        e_title("Model Comparison Metrics") |>
        e_tooltip()
    })
  })
}
