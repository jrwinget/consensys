box::use(
  bsicons[bs_icon],
  bslib[
    input_switch,
    input_task_button,
    layout_column_wrap,
    layout_columns,
    popover,
    tooltip
  ],
  purrr[map, map_dbl],
  shiny,
  stats[sd],
)

box::use(
  app/logic/sjs_calculations[simulate_sjs_process],
  app/logic/ui_helpers[content_card],
  app/view/plot_heatmap,
  app/view/plot_line,
)

`%||%` <- function(a, b) if (is.null(a)) b else a

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
        "Social Judgment Scheme (SJS) Model",
        tooltip(
          bs_icon("info-circle"),
          "Model how opinions converge or polarize through group interaction",
          placement = "right"
        )
      ),
      shiny$tags$p(
        class = "lead mb-3",
        "Watch how group members influence each other's continuous judgments
        over time."
      ),
      shiny$tags$div(
        class = "alert alert-info alert-dismissible fade show",
        shiny$tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert"
        ),
        bs_icon("lightbulb"),
        shiny$tags$strong("Tip:"),
        " Members with similar positions have more influence on each other!"
      )
    ),
    # settings -----------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      content_card(
        title = "Model Parameters",
        header_content = popover(
          bs_icon("question-circle"),
          title = "Parameter Guide",
          shiny$tags$div(
            shiny$tags$p(
              shiny$tags$b("Individuals:"),
              " Group size affects convergence speed"
            ),
            shiny$tags$p(
              shiny$tags$b("Rounds:"),
              " More rounds show long-term dynamics"
            ),
            shiny$tags$p(
              class = "mb-0",
              shiny$tags$b("Weights:"),
              " See how similarity drives influence"
            )
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
          class = "mb-4",
          input_switch(
            ns("show_weights"),
            label = shiny$tagList(
              "Show Influence Weights",
              tooltip(
                bs_icon("info-circle"),
                "Visualize how members influence each other",
                placement = "top"
              )
            ),
            value = FALSE,
            width = "100%"
          )
        ),
        shiny$div(
          class = "d-grid",
          input_task_button(
            ns("simulate"),
            shiny$tagList(
              bs_icon("play-circle-fill"),
              "Run Simulation"
            ),
            class = "btn-lg btn-primary"
          )
        )
      ),
      content_card(
        title = "Initial Positions",
        header_content = tooltip(
          bs_icon("dice-5"),
          "Set starting opinions or randomize",
          placement = "right"
        ),
        shiny$uiOutput(ns("position_inputs")),
        shiny$tags$hr(class = "my-3"),
        shiny$tags$div(
          class = "d-flex justify-content-between align-items-center",
          shiny$tags$small(
            class = "text-muted",
            bs_icon("arrow-left-right"),
            "Positions range from 0 (strongly against) to 100 (strongly for)"
          ),
          input_task_button(
            ns("randomize_positions"),
            bs_icon("shuffle"),
            class = "btn-sm btn-outline-secondary",
            title = "Randomize positions"
          )
        )
      )
    ),
    # results ------------------------------------------------------------------
    content_card(
      title = shiny$tagList(
        "Simulation Results",
        shiny$tags$span(
          class = "badge bg-secondary ms-2",
          shiny$textOutput(ns("status_badge"), inline = TRUE)
        )
      ),
      layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        plot_line$ui(ns("plot_convergence"), plot_height = "400px"),
        content_card(
          class = "shadow-none border h-100",
          header_class = "bg-light",
          body_class = "p-3",
          shiny$tags$h6("Key Metrics"),
          shiny$uiOutput(ns("summary_stats")),
          shiny$conditionalPanel(
            condition = "input.show_weights",
            ns = ns,
            shiny$tags$hr(),
            shiny$tags$h6("Influence Matrix"),
            plot_heatmap$ui(ns("plot_influence_mat"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    position_values <- shiny$reactiveValues()

    # init position values when n_individuals changes
    shiny$observe({
      n <- input$n_individuals
      if (!is.null(n) && n >= 2) {
        for (i in seq_len(n)) {
          key <- paste0("pos_", i)
          if (is.null(position_values[[key]])) {
            position_values[[key]] <- sample(20:80, 1)
          }
        }

        all_keys <- names(position_values)
        pos_keys <- grep("^pos_\\d+$", all_keys, value = TRUE)
        for (key in pos_keys) {
          pos_num <- as.numeric(sub("pos_", "", key))
          if (pos_num > n) {
            position_values[[key]] <- NULL
          }
        }
      }
    })

    # dynamic inputs -----------------------------------------------------------
    output$position_inputs <- shiny$renderUI({
      shiny$req(input$n_individuals)
      map(
        seq_len(input$n_individuals),
        function(i) {
          key <- paste0("pos_", i)
          initial_value <- position_values[[key]] %||% sample(20:80, 1)

          shiny$sliderInput(
            inputId = session$ns(key),
            label = paste("Individual", i),
            min = 0,
            max = 100,
            value = initial_value
          )
        }
      )
    })

    # update stored values when sliders change
    shiny$observe({
      shiny$req(input$n_individuals)
      for (i in seq_len(input$n_individuals)) {
        key <- paste0("pos_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            position_values[[key]] <- input[[key]]
          })
        }
      }
    })

    # reactive values ----------------------------------------------------------
    # positions from input values
    sim_results <- shiny$reactive({
      shiny$req(input$n_individuals)
      initial_positions <- map_dbl(
        seq_len(input$n_individuals),
        function(i) {
          key <- paste0("pos_", i)
          input[[key]] %||% position_values[[key]] %||% sample(20:80, 1)
        }
      )

      simulate_sjs_process(initial_positions, input$n_rounds)
    }) |> 
      shiny$bindEvent(input$simulate)

    # randomize positions
    shiny$observe({
      shiny$req(input$n_individuals)
      for (i in seq_len(input$n_individuals)) {
        key <- paste0("pos_", i)
        new_value <- sample(20:80, 1)
        position_values[[key]] <- new_value
        shiny$updateSliderInput(
          session,
          key,
          value = new_value
        )
      }
    }) |> 
      shiny$bindEvent(input$randomize_positions)

    # plots --------------------------------------------------------------------
    output$plot_convergence <- plot_line$server("plot_convergence", sim_results)

    output$plot_influence_mat <- plot_heatmap$server(
      "plot_influence_mat",
      sim_results,
      input$show_weights
    )

    # status and summary outputs
    output$status_badge <- shiny$renderText({
      if (input$simulate == 0) {
        "Ready"
      } else if (is.null(sim_results())) {
        "Ready"
      } else {
        "Complete"
      }
    })

    output$summary_stats <- shiny$renderUI({
      shiny$req(sim_results())

      results <- sim_results()
      initial_spread <- sd(results[1, ])
      final_spread <- sd(results[nrow(results), ])
      convergence <- initial_spread - final_spread

      shiny$tagList(
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
  })
}
