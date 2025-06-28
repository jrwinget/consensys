box::use(
  bsicons[bs_icon],
  bslib[
    input_task_button,
    layout_column_wrap,
    layout_columns,
    tooltip
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
  app/logic/ui_helpers[content_card],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  layout_column_wrap(
    width = 1,
    fill = FALSE,
    heights_equal = "row",
    class = "mb-3",
    # description --------------------------------------------------------------
    content_card(
      title = shiny$tagList(
        "Model Comparison Simulation",
        tooltip(
          bs_icon("info-circle"),
          "Run side-by-side comparisons of SDS and SJS models",
          placement = "right"
        )
      ),
      shiny$tags$p(
        class = "lead mb-3",
        "Compare how SDS and SJS models handle the same initial group
        configuration."
      ),
      shiny$tags$div(
        class = "alert alert-warning alert-dismissible fade show",
        shiny$tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert"
        ),
        bs_icon("exclamation-triangle-fill"),
        shiny$tags$strong("Note:"),
        paste(
          " Both models will start with the same group composition for fair",
          "comparison."
        )
      )
    ),
    # Settings
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      content_card(
        title = "SDS Settings",
        header_content = tooltip(
          bs_icon("gear"),
          "Configure discrete choice model",
          placement = "top"
        ),
        shiny$div(
          class = "mb-3",
          shiny$selectInput(
            ns("sds_scheme"),
            "Decision Scheme:",
            choices = c(
              "Majority Rule" = "majority",
              "Proportional" = "proportional",
              "Two-Thirds Majority" = "two_thirds",
              "Unanimity" = "unanimity",
              "Truth-Wins" = "truth"
            ),
            selected = "majority",
            width = "100%"
          )
        ),
        shiny$div(
          class = "mb-3",
          shiny$numericInput(
            ns("n_alternatives"),
            shiny$tagList(
              "Number of Alternatives",
              shiny$tags$small(class = "text-muted", "(2-5 options)")
            ),
            value = 3,
            min = 2,
            max = 5,
            width = "100%"
          )
        ),
        shiny$uiOutput(ns("alt_weights"))
      ),
      content_card(
        title = "SJS Settings",
        header_content = tooltip(
          bs_icon("gear"),
          "Configure continuous judgment model",
          placement = "top"
        ),
        shiny$div(
          class = "mb-3",
          shiny$numericInput(
            ns("n_rounds"),
            shiny$tagList(
              "Number of Rounds",
              shiny$tags$small(class = "text-muted", "(1-20 iterations)")
            ),
            value = 5,
            min = 1,
            max = 20,
            width = "100%"
          )
        ),
        shiny$div(
          class = "mb-3",
          shiny$numericInput(
            ns("n_individuals"),
            shiny$tagList(
              "Number of Individuals",
              shiny$tags$small(class = "text-muted", "(2-10 members)")
            ),
            value = 5,
            min = 2,
            max = 10,
            width = "100%"
          )
        ),
        shiny$uiOutput(ns("initial_positions"))
      )
    ),
    # Run button
    content_card(
      class = "shadow-sm border-primary",
      shiny$div(
        class = "text-center",
        shiny$tags$p(
          class = "mb-3",
          "Configure both models above, then run the comparison simulation"
        ),
        input_task_button(
          ns("run_comparison"),
          shiny$tagList(
            bs_icon("play-circle-fill"),
            "Run Comparison"
          ),
          class = "btn-lg btn-primary"
        )
      )
    ),
    # Results
    content_card(
      title = shiny$tagList(
        "Comparison Results",
        shiny$tags$span(
          class = "badge bg-secondary ms-2",
          shiny$textOutput(ns("comparison_status"), inline = TRUE)
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        content_card(
          title = "SDS Model Outcome",
          class = "shadow-none border",
          header_class = "bg-white",
          body_class = "p-3",
          echarts4rOutput(ns("sds_plot"), height = "350px")
        ),
        content_card(
          title = "SJS Model Outcome",
          class = "shadow-none border",
          header_class = "bg-white",
          body_class = "p-3",
          echarts4rOutput(ns("sjs_plot"), height = "350px")
        )
      ),
      shiny$tags$div(
        class = "mt-3",
        content_card(
          title = "Insights & Comparison",
          class = "shadow-none border",
          header_class = "bg-info text-white",
          body_class = "p-3",
          shiny$uiOutput(ns("comparison_insights"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # dynamic inputs -----------------------------------------------------------
    output$alt_weights <- shiny$renderUI({
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
      shiny$req(input$run_comparison, input$n_alternatives)

      weights <- map_dbl(
        seq_len(input$n_alternatives),
        ~ input[[paste0("alt_weight_", .x)]] %||% (1 / input$n_alternatives)
      )

      weights <- weights / sum(weights)

      res <- generate_decision_matrix(input$sds_scheme, input$n_alternatives)
      apply_decision_scheme(weights, res)
    }) |>
      shiny$bindEvent(input$run_comparison)

    sjs_results <- shiny$reactive({
      shiny$req(input$run_comparison, input$n_individuals)

      init_positions <- map_dbl(
        seq_len(input$n_individuals),
        ~ input[[paste0("init_pos_", .x)]] %||% 50
      )

      simulate_sjs_process(init_positions, input$n_rounds)
    }) |>
      shiny$bindEvent(input$run_comparison)

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
        e_line(Position, serie = Individual) |>
        e_title("Position Convergence") |>
        e_tooltip(trigger = "axis")
    })

    output$comparison_insights <- shiny$renderUI({
      shiny$req(sds_results(), sjs_results())

      sds_entropy <- -sum(sds_results() * log(sds_results() + 1e-10))
      sjs_final_spread <- sd(sjs_results()[nrow(sjs_results()), ])
      sjs_initial_spread <- sd(sjs_results()[1, ])
      convergence <- sjs_initial_spread - sjs_final_spread
      
      most_likely_alt <- which.max(sds_results())
      
      shiny$tagList(
        shiny$tags$div(
          class = "row",
          shiny$tags$div(
            class = "col-md-6",
            shiny$tags$h6("SDS Model Results:"),
            shiny$tags$ul(
              shiny$tags$li(
                sprintf("Most likely outcome: Alternative %d (%.1f%%)", 
                       most_likely_alt, max(sds_results()) * 100)
              ),
              shiny$tags$li(
                sprintf("Decision entropy: %.3f", sds_entropy)
              )
            )
          ),
          shiny$tags$div(
            class = "col-md-6",
            shiny$tags$h6("SJS Model Results:"),
            shiny$tags$ul(
              shiny$tags$li(
                sprintf("Initial spread: %.2f", sjs_initial_spread)
              ),
              shiny$tags$li(
                sprintf("Final spread: %.2f", sjs_final_spread)
              ),
              shiny$tags$li(
                sprintf("Convergence: %.2f %s", 
                       abs(convergence),
                       if (convergence > 0) "(converged)" else "(diverged)")
              )
            )
          )
        )
      )
    })

    output$comparison_status <- shiny$renderText({
      if (is.null(sds_results()) || is.null(sjs_results())) {
        "Ready"
      } else {
        "Complete"
      }
    })
  })
}
