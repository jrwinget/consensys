box::use(
  bsicons[bs_icon],
  bslib,
  shiny,
  stats[sd],
)

box::use(
  app/logic/sds_calculations[
    apply_decision_scheme,
    calculate_entropy,
    calculate_polarization,
    generate_decision_matrix
  ],
  app/logic/sjs_calculations[calculate_sjs_consensus, simulate_sjs_process],
  app/logic/ui_helpers[content_card, metric_box],
  app/view/plot_bar,
  app/view/plot_heatmap,
  app/view/plot_line,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tags$div(
    class = "p-3",
    bslib$accordion(
      id = ns("results_accordion"),
      multiple = FALSE,
      bslib$accordion_panel(
        "SDS Model Results",
        value = "sds_panel",
        icon = bs_icon("diagram-3"),
        shiny$conditionalPanel(
          condition = "output.sds_has_results",
          ns = ns,
          bslib$layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            content_card(
              title = shiny$tags$span(
                "Decision Matrix",
                bslib$tooltip(
                  bs_icon("info-circle"),
                  shiny$tags$div(
                    shiny$tags$p(
                      "This matrix shows the decision scheme rules (D) - how
                      different group compositions lead to final choices."
                    ),
                    shiny$tags$p(
                      class = "mb-0",
                      shiny$tags$b("Note:"),
                      " This matrix represents the social process rules, not
                      individual preferences."
                    )
                  ),
                  placement = "top"
                )
              ),
              class = "shadow-none border h-100",
              header_class = "bg-white",
              body_class = "p-3",
              plot_heatmap$ui(ns("sds_heatmap"))
            ),
            content_card(
              title = "Predicted Outcomes",
              class = "shadow-none border h-100",
              header_class = "bg-white",
              body_class = "p-3",
              plot_bar$ui(ns("sds_outcome"))
            )
          ),
          shiny$tags$div(
            class = "mt-3",
            bslib$layout_column_wrap(
              width = 1 / 3,
              fill = FALSE,
              heights_equal = "row",
              metric_box(
                title = "Most Likely",
                value = shiny$textOutput(ns("sds_most_likely"), inline = TRUE),
                subtitle = shiny$textOutput(
                  ns("sds_probability"),
                  inline = TRUE
                ),
                icon_name = "trophy",
                theme = "primary"
              ),
              metric_box(
                title = "Entropy",
                value = shiny$textOutput(ns("sds_entropy"), inline = TRUE),
                subtitle = "Decision uncertainty",
                icon_name = "shuffle",
                theme = "info"
              ),
              metric_box(
                title = "Polarization",
                value = shiny$textOutput(
                  ns("sds_polarization"),
                  inline = TRUE
                ),
                subtitle = "Distribution spread",
                icon_name = "arrows-expand",
                theme = "warning"
              )
            )
          )
        ),
        shiny$conditionalPanel(
          condition = "!output.sds_has_results",
          ns = ns,
          shiny$tags$div(
            class = "text-center text-muted p-5",
            bs_icon("inbox", size = "3rem"),
            shiny$tags$h5(class = "mt-3", "No SDS results yet"),
            shiny$tags$p(
              "Configure parameters and click 'Run SDS Model' to see results"
            )
          )
        )
      ),
      bslib$accordion_panel(
        "SJS Model Results",
        value = "sjs_panel",
        icon = bs_icon("sliders"),
        shiny$conditionalPanel(
          condition = "output.sjs_has_results",
          ns = ns,
          bslib$layout_columns(
            col_widths = c(8, 4),
            fill = FALSE,
            plot_line$ui(ns("sjs_convergence"), plot_height = "400px"),
            content_card(
              class = "shadow-none border h-100",
              header_class = "bg-light",
              body_class = "p-3",
              shiny$tags$h6("Key Metrics"),
              shiny$uiOutput(ns("sjs_summary")),
              shiny$tags$hr(),
              shiny$tags$h6("Influence Matrix"),
              plot_heatmap$ui(ns("sjs_influence"))
            )
          ),
          shiny$tags$div(
            class = "mt-3",
            content_card(
              title = "Consensus Analysis",
              class = "shadow-none border",
              header_class = "bg-info text-white",
              shiny$uiOutput(ns("sjs_consensus"))
            )
          )
        ),
        shiny$conditionalPanel(
          condition = "!output.sjs_has_results",
          ns = ns,
          shiny$tags$div(
            class = "text-center text-muted p-5",
            bs_icon("inbox", size = "3rem"),
            shiny$tags$h5(class = "mt-3", "No SJS results yet"),
            shiny$tags$p(
              "Configure parameters and click 'Run SJS Model' to see results"
            )
          )
        )
      ),
      bslib$accordion_panel(
        "Model Comparison",
        value = "comparison_panel",
        icon = bs_icon("arrow-left-right"),
        shiny$conditionalPanel(
          condition = "output.comparison_has_results",
          ns = ns,
          bslib$layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            content_card(
              title = "SDS Model Outcome",
              class = "shadow-none border",
              header_class = "bg-white",
              body_class = "p-3",
              plot_bar$ui(ns("comp_sds"))
            ),
            content_card(
              title = "SJS Model Outcome",
              class = "shadow-none border",
              header_class = "bg-white",
              body_class = "p-3",
              plot_line$ui(ns("comp_sjs"))
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
        ),
        shiny$conditionalPanel(
          condition = "!output.comparison_has_results",
          ns = ns,
          shiny$tags$div(
            class = "text-center text-muted p-5",
            bs_icon("inbox", size = "3rem"),
            shiny$tags$h5(class = "mt-3", "No comparison results yet"),
            shiny$tags$p(
              "Configure parameters and click 'Run Comparison' to see results"
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(
    id,
    model_type,
    group_size,
    n_alternatives,
    n_rounds,
    sds_scheme,
    self_weight,
    decay_parameter,
    pref_values,
    position_values,
    run_sds,
    run_sjs,
    run_comparison) {
  shiny$moduleServer(id, function(input, output, session) {
    # Reactive values for results
    sds_results <- shiny$reactiveVal(NULL)
    sjs_results <- shiny$reactiveVal(NULL)
    decision_matrix <- shiny$reactiveVal(NULL)

    # Helper function to get preference values
    get_pref_values <- function() {
      n <- n_alternatives()
      prefs <- numeric(n)
      for (i in 1:n) {
        key <- paste0("pref_", i)
        prefs[i] <- pref_values[[key]] %||% (1 / n)
      }
      prefs / sum(prefs) # normalize
    }

    # Helper function to get position values
    get_position_values <- function() {
      n <- group_size()
      positions <- numeric(n)
      for (i in 1:n) {
        key <- paste0("pos_", i)
        positions[i] <- position_values[[key]] %||% 50
      }
      positions
    }

    # Run SDS model
    shiny$observeEvent(run_sds(), {
      shiny$req(run_sds() > 0, n_alternatives(), sds_scheme())

      # Generate decision matrix
      dm <- generate_decision_matrix(
        sds_scheme(),
        n_alternatives(),
        group_size()
      )
      decision_matrix(dm)

      # Apply decision scheme
      prefs <- get_pref_values()
      results <- apply_decision_scheme(prefs, dm)
      sds_results(results)

      # Open SDS panel
      bslib$accordion_panel_update(
        session = session,
        "results_accordion",
        target = "sds_panel"
      )
    })

    # Run SJS model
    shiny$observeEvent(run_sjs(), {
      shiny$req(run_sjs() > 0, group_size(), n_rounds())

      positions <- get_position_values()
      results <- simulate_sjs_process(
        positions,
        n_rounds(),
        self_weight = self_weight(),
        decay_parameter = decay_parameter()
      )
      sjs_results(results)

      # Open SJS panel
      bslib$accordion_panel_update(
        session = session,
        "results_accordion",
        target = "sjs_panel"
      )
    })

    # Real-time SJS updates when parameters change
    shiny$observe({
      # Only update if we have existing results and SJS model is selected
      shiny$req(
        !is.null(sjs_results()),
        model_type() %in% c("sjs", "comparison"),
        group_size(),
        n_rounds(),
        self_weight(),
        decay_parameter()
      )

      positions <- get_position_values()
      results <- simulate_sjs_process(
        positions,
        n_rounds(),
        self_weight = self_weight(),
        decay_parameter = decay_parameter()
      )
      sjs_results(results)
    })

    # Run comparison
    shiny$observeEvent(run_comparison(), {
      shiny$req(
        run_comparison() > 0,
        n_alternatives(),
        group_size(),
        n_rounds()
      )

      # Run both models
      # SDS
      dm <- generate_decision_matrix(
        sds_scheme(),
        n_alternatives(),
        group_size()
      )
      decision_matrix(dm)
      prefs <- get_pref_values()
      sds_res <- apply_decision_scheme(prefs, dm)
      sds_results(sds_res)

      # SJS
      positions <- get_position_values()
      sjs_res <- simulate_sjs_process(
        positions,
        n_rounds(),
        self_weight = self_weight(),
        decay_parameter = decay_parameter()
      )
      sjs_results(sjs_res)

      # Open comparison panel
      bslib$accordion_panel_update(
        session = session,
        "results_accordion",
        target = "comparison_panel"
      )
    })

    # Output flags for conditional panels
    output$sds_has_results <- shiny$reactive({
      !is.null(sds_results())
    })
    shiny$outputOptions(output, "sds_has_results", suspendWhenHidden = FALSE)

    output$sjs_has_results <- shiny$reactive({
      !is.null(sjs_results())
    })
    shiny$outputOptions(output, "sjs_has_results", suspendWhenHidden = FALSE)

    output$comparison_has_results <- shiny$reactive({
      !is.null(sds_results()) && !is.null(sjs_results())
    })
    shiny$outputOptions(
      output,
      "comparison_has_results",
      suspendWhenHidden = FALSE
    )

    # SDS Results
    plot_heatmap$server("sds_heatmap", decision_matrix)

    plot_bar$server(
      "sds_outcome",
      res = shiny$reactive(get_pref_values()),
      y_var = "Probability",
      title = "Group Decision Probabilities",
      decision_mat = decision_matrix
    )

    output$sds_most_likely <- shiny$renderText({
      shiny$req(sds_results())
      paste("Alternative", which.max(sds_results()))
    })

    output$sds_probability <- shiny$renderText({
      shiny$req(sds_results())
      sprintf("%.1f%% probability", max(sds_results()) * 100)
    })

    output$sds_entropy <- shiny$renderText({
      shiny$req(sds_results())
      sprintf("%.3f", calculate_entropy(sds_results()))
    })

    output$sds_polarization <- shiny$renderText({
      shiny$req(sds_results())
      sprintf("%.3f", calculate_polarization(sds_results()))
    })

    # SJS Results
    plot_line$server("sjs_convergence", sjs_results)
    plot_heatmap$server(
      "sjs_influence",
      sjs_results,
      is_displayed = shiny$reactive(TRUE)
    )

    output$sjs_summary <- shiny$renderUI({
      shiny$req(sjs_results())

      results <- sjs_results()
      initial_spread <- sd(results[1, ])
      final_spread <- sd(results[nrow(results), ])
      convergence <- initial_spread - final_spread

      shiny$tags$div(
        shiny$tags$div(
          class = "mb-2",
          shiny$tags$small(class = "text-muted", "Initial Spread:"),
          shiny$tags$br(),
          shiny$tags$strong(sprintf("%.2f", initial_spread))
        ),
        shiny$tags$div(
          class = "mb-2",
          shiny$tags$small(class = "text-muted", "Final Spread:"),
          shiny$tags$br(),
          shiny$tags$strong(sprintf("%.2f", final_spread))
        ),
        shiny$tags$div(
          class = "mb-2",
          shiny$tags$small(class = "text-muted", "Convergence:"),
          shiny$tags$br(),
          shiny$tags$strong(
            sprintf("%.2f", convergence),
            class = if (convergence > 0) "text-success" else "text-warning"
          )
        )
      )
    })

    output$sjs_consensus <- shiny$renderUI({
      shiny$req(sjs_results())

      consensus <- calculate_sjs_consensus(sjs_results())

      shiny$tags$div(
        shiny$tags$p(
          shiny$tags$b("Variance Reduction:"),
          sprintf(" %.1f%%", consensus$variance_reduction * 100)
        ),
        shiny$tags$p(
          shiny$tags$b("Consensus Level:"),
          sprintf(" %.2f", consensus$consensus_level)
        ),
        shiny$tags$p(
          shiny$tags$b("Range Reduction:"),
          sprintf(" %.1f%%", consensus$range_reduction * 100)
        ),
        shiny$tags$p(
          shiny$tags$b("Status:"),
          if (consensus$variance_reduction > 0) {
            shiny$tags$span(class = "text-success", " Converged")
          } else {
            shiny$tags$span(class = "text-warning", " Did not converge")
          }
        )
      )
    })

    # Comparison Results
    plot_bar$server(
      "comp_sds",
      res = shiny$reactive(get_pref_values()),
      y_var = "Probability",
      title = "Final Decision Probabilities",
      decision_mat = decision_matrix
    )

    plot_line$server("comp_sjs", sjs_results)

    output$comparison_insights <- shiny$renderUI({
      shiny$req(sds_results(), sjs_results())

      # SDS metrics
      sds_probs <- sds_results()
      sds_entropy <- calculate_entropy(sds_probs)
      most_likely_alt <- which.max(sds_probs)

      # SJS metrics
      sjs_final_spread <- sd(sjs_results()[nrow(sjs_results()), ])
      sjs_initial_spread <- sd(sjs_results()[1, ])
      convergence <- sjs_initial_spread - sjs_final_spread

      shiny$tags$div(
        shiny$tags$div(
          class = "row",
          shiny$tags$div(
            class = "col-md-6",
            shiny$tags$h6("SDS Model Results:"),
            shiny$tags$ul(
              shiny$tags$li(sprintf(
                "Most likely outcome: Alternative %d (%.1f%%)",
                most_likely_alt,
                max(sds_probs) * 100
              )),
              shiny$tags$li(sprintf("Decision entropy: %.3f", sds_entropy)),
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
              shiny$tags$li(sprintf(
                "Convergence: %.2f %s",
                abs(convergence),
                if (convergence > 0) "(converged)" else "(diverged)"
              )),
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
  })
}
