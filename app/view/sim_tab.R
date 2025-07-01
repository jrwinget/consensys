box::use(
  bsicons[bs_icon],
  bslib[
    input_task_button,
    layout_column_wrap,
    layout_columns,
    tooltip
  ],
  purrr[map, map_dbl],
  shiny,
  stats[sd],
)

box::use(
  app/logic/sds_calculations[apply_decision_scheme, generate_decision_matrix],
  app/logic/sjs_calculations[simulate_sjs_process],
  app/logic/ui_helpers[content_card],
  app/view/plot_bar,
  app/view/plot_line,
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
    # settings -----------------------------------------------------------------
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
              "Truth-Wins" = "truth_wins"
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
    # run button
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
    # results ------------------------------------------------------------------
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
          plot_bar$ui(ns("plot_sds"))
        ),
        content_card(
          title = "SJS Model Outcome",
          class = "shadow-none border",
          header_class = "bg-white",
          body_class = "p-3",
          plot_line$ui(ns("plot_sjs"))
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
    weight_values <- shiny$reactiveValues()
    position_values <- shiny$reactiveValues()

    # init weight values when n_alternatives changes
    shiny$observeEvent(input$n_alternatives, {
      n <- input$n_alternatives
      if (!is.null(n) && n >= 2) {
        default_weight <- 1 / n
        for (i in seq_len(n)) {
          key <- paste0("alt_weight_", i)
          if (is.null(weight_values[[key]])) {
            weight_values[[key]] <- default_weight
          }
        }

        all_keys <- names(weight_values)
        weight_keys <- grep("^alt_weight_\\d+$", all_keys, value = TRUE)
        for (key in weight_keys) {
          weight_num <- as.numeric(sub("alt_weight_", "", key))
          if (weight_num > n) {
            weight_values[[key]] <- NULL
          }
        }
      }
    })

    # init position values when n_individuals changes
    shiny$observeEvent(input$n_individuals, {
      n <- input$n_individuals
      if (!is.null(n) && n >= 2) {
        for (i in seq_len(n)) {
          key <- paste0("init_pos_", i)
          if (is.null(position_values[[key]])) {
            position_values[[key]] <- sample(20:80, 1)
          }
        }

        all_keys <- names(position_values)
        pos_keys <- grep("^init_pos_\\d+$", all_keys, value = TRUE)
        for (key in pos_keys) {
          pos_num <- as.numeric(sub("init_pos_", "", key))
          if (pos_num > n) {
            position_values[[key]] <- NULL
          }
        }
      }
    })

    # dynamic inputs -----------------------------------------------------------
    output$alt_weights <- shiny$renderUI({
      shiny$req(input$n_alternatives)

      map(
        seq_len(input$n_alternatives),
        function(i) {
          key <- paste0("alt_weight_", i)
          initial_value <- weight_values[[key]] %||% (1 / input$n_alternatives)

          shiny$sliderInput(
            inputId = session$ns(key),
            label = paste("Alternative", i, "Weight"),
            min = 0,
            max = 1,
            value = initial_value,
            step = 0.01
          )
        }
      )
    })

    output$initial_positions <- shiny$renderUI({
      shiny$req(input$n_individuals)

      map(
        seq_len(input$n_individuals),
        function(i) {
          key <- paste0("init_pos_", i)
          initial_value <- position_values[[key]] %||% sample(20:80, 1)

          shiny$sliderInput(
            inputId = session$ns(key),
            label = paste("Individual", i, "Position"),
            min = 0,
            max = 100,
            value = initial_value
          )
        }
      )
    })

    # update when sliders change
    shiny$observe({
      shiny$req(input$n_alternatives)
      for (i in seq_len(input$n_alternatives)) {
        key <- paste0("alt_weight_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            weight_values[[key]] <- input[[key]]
          })
        }
      }
    })

    shiny$observe({
      shiny$req(input$n_individuals)
      for (i in seq_len(input$n_individuals)) {
        key <- paste0("init_pos_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            position_values[[key]] <- input[[key]]
          })
        }
      }
    })

    # reactive values ----------------------------------------------------------
    sds_results <- shiny$reactive({
      shiny$req(input$run_comparison > 0, input$n_alternatives)

      weights <- map_dbl(
        seq_len(input$n_alternatives),
        function(i) {
          key <- paste0("alt_weight_", i)
          input[[key]] %||% weight_values[[key]] %||% (1 / input$n_alternatives)
        }
      )

      weights <- weights / sum(weights) # normalize
      res <- generate_decision_matrix(input$sds_scheme, input$n_alternatives)
      apply_decision_scheme(weights, res)
    })

    sjs_results <- shiny$reactive({
      shiny$req(input$run_comparison > 0, input$n_individuals)

      init_positions <- map_dbl(
        seq_len(input$n_individuals),
        function(i) {
          key <- paste0("init_pos_", i)
          input[[key]] %||% position_values[[key]] %||% 50
        }
      )

      simulate_sjs_process(init_positions, input$n_rounds)
    })

    # plots --------------------------------------------------------------------
    output$plot_sds <- plot_bar$server(
      "plot_sds",
      res = sds_results,
      y_var = "Probability",
      title = "Final Decision Probabilities"
    )

    output$plot_sjs <- plot_line$server("plot_sjs", sjs_results)

    output$comparison_insights <- shiny$renderUI({
      shiny$req(sds_results(), sjs_results())

      # SDS metrics
      sds_probs <- sds_results()
      sds_entropy <- -sum(sds_probs * log(sds_probs + 1e-10))
      most_likely_alt <- which.max(sds_probs)

      # SJS metrics
      sjs_final_spread <- sd(sjs_results()[nrow(sjs_results()), ])
      sjs_initial_spread <- sd(sjs_results()[1, ])
      convergence <- sjs_initial_spread - sjs_final_spread

      shiny$tagList(
        shiny$tags$div(
          class = "row",
          shiny$tags$div(
            class = "col-md-6",
            shiny$tags$h6("SDS Model Results:"),
            shiny$tags$ul(
              shiny$tags$li(
                sprintf(
                  "Most likely outcome: Alternative %d (%.1f%%)",
                  most_likely_alt,
                  max(sds_probs) * 100
                )
              ),
              shiny$tags$li(
                sprintf("Decision entropy: %.3f", sds_entropy)
              ),
              shiny$tags$li(
                if (sds_entropy < 0.5) {
                  "Strong consensus on one alternative"
                } else if (sds_entropy < 1.0) {
                  "Moderate agreement with some uncertainty"
                } else {
                  "High uncertainty across alternatives"
                }
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
                sprintf(
                  "Convergence: %.2f %s",
                  abs(convergence),
                  if (convergence > 0) "(converged)" else "(diverged)"
                )
              ),
              shiny$tags$li(
                if (sjs_final_spread < 5) {
                  "Strong consensus reached"
                } else if (sjs_final_spread < 15) {
                  "Moderate convergence achieved"
                } else {
                  "Positions remain dispersed"
                }
              )
            )
          )
        ),
        shiny$tags$hr(),
        shiny$tags$div(
          shiny$tags$h6("Key Comparison:"),
          shiny$tags$p(
            if (sds_entropy < 0.5 && sjs_final_spread < 10) {
              "Both models show strong consensus formation, indicating robust
              agreement within the group."
            } else if (sds_entropy > 1.0 && sjs_final_spread > 20) {
              "Both models show persistent disagreement, suggesting fundamental
              divisions in the group."
            } else {
              "The models show different patterns of consensus, highlighting how
              discrete vs. continuous choice contexts affect group dynamics."
            }
          )
        )
      )
    })

    output$comparison_status <- shiny$renderText({
      if (input$run_comparison == 0) {
        "Ready"
      } else if (is.null(sds_results()) || is.null(sjs_results())) {
        "Ready"
      } else {
        "Complete"
      }
    })
  })
}
