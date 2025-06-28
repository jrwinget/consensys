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
  dplyr[starts_with],
  echarts4r,
  purrr[map, map_dbl],
  shiny,
  tidyr[pivot_longer],
)

box::use(
  app/logic/sjs_calculations[calculate_sjs_weights, simulate_sjs_process],
  app/logic/ui_helpers[content_card],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  layout_column_wrap(
    width = 1,
    fill = FALSE,
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
        echarts4r$echarts4rOutput(ns("convergence_plot"), height = "400px"),
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
            echarts4r$echarts4rOutput(ns("weights_plot"), height = "250px")
          )
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
