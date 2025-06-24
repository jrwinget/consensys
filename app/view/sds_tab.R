box::use(
  bslib[
    card,
    card_body,
    card_header,
    input_task_button,
    layout_column_wrap,
    layout_columns
  ],
  echarts4r[
    e_axis,
    e_bar,
    e_charts,
    e_title,
    e_tooltip,
    echarts4rOutput,
    renderEcharts4r
  ],
  shiny,
)

box::use(
  app/logic/sds_calculations[generate_decision_matrix],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    # description --------------------------------------------------------------
    card(
      card_header("Social Decision Scheme (SDS) Model"),
      card_body(
        shiny$tags$p(
          "The SDS model transforms distributions of individual preferences into
          group decision probabilities using transition matrices."
        )
      )
    ),
    # settings -----------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Decision Scheme Settings"),
        card_body(
          shiny$selectInput(
            ns("sds_type"),
            "Decision Scheme Type:",
            choices = c(
              "Majority Rule" = "majority",
              "Proportionality Rule" = "proportional",
              "Two-Thirds Majority" = "twothirds",
              "Unanimity" = "unanimity",
              "Truth-Wins" = "truth",
              "Custom" = "custom"
            )
          ),
          shiny$numericInput(
            ns("num_alternatives"),
            "Number of Alternatives:",
            value = 3,
            min = 2,
            max = 5
          ),
          shiny$uiOutput(ns("custom_matrix_input")),
          input_task_button(ns("update_sds"), "Update Model")
        )
      ),
      card(
        card_header("Individual Preference Distribution"),
        card_body(
          shiny$uiOutput(ns("preference_sliders")),
          echarts4rOutput(ns("preference_plot"))
        )
      )
    ),
    # results ------------------------------------------------------------------
    card(
      card_header("Results"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          echarts4rOutput(ns("decision_matrix_plot")),
          echarts4rOutput(ns("outcome_plot"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # reactive values ----------------------------------------------------------
    decision_matrix <- shiny$reactiveVal(generate_decision_matrix(
      "majority",
      3
    ))
    individual_prefs <- shiny$reactiveVal(rep(1 / 3, 3))

    # update pref sliders
    output$preference_sliders <- shiny$renderUI({
      n <- input$num_alternatives

      sliders <- lapply(1:n, function(i) {
        shiny$sliderInput(
          inputId = session$ns(paste0("pref_", i)),
          label = paste("Alternative", i, "preference proportion:"),
          min = 0,
          max = 1,
          value = 1 / n,
          step = 0.01
        )
      })

      shiny$div(sliders)
    })

    # update model -------------------------------------------------------------
    shiny$observe({
      shiny$req(input$num_alternatives)
      n <- input$num_alternatives

      # update individual prefs
      prefs <- numeric(n)
      for (i in 1:n) {
        pref_value <- input[[paste0("pref_", i)]]
        prefs[i] <- if (!is.null(pref_value)) pref_value else 1 / n
      }

      # normalize prefs
      prefs <- prefs / sum(prefs)
      individual_prefs(prefs)

      # update matrix
      decision_matrix(generate_decision_matrix(input$sds_type, n))
    }) |>
      shiny$bindEvent(input$update_sds)

    # plots --------------------------------------------------------------------
    output$preference_plot <- renderEcharts4r({
      prefs <- individual_prefs()

      data.frame(
        Alternative = paste("Alt", seq_along(prefs)),
        Proportion = prefs
      ) |>
        e_charts(Alternative) |>
        e_bar(Proportion) |>
        e_title("Individual Preference Distribution") |>
        e_axis(axis = "y", max = 1, name = "Proportion") |>
        e_tooltip()
    })

    output$decision_matrix_plot <- renderEcharts4r({
      shiny$req(decision_matrix())
      matrix_data <- as.data.frame(decision_matrix())

      matrix_data |>
        e_charts() |>
        e_bar(V1) |> # TODO: assuming first column; create/verify with template
        e_title("Decision Matrix") |>
        e_tooltip()
    })

    output$outcome_plot <- renderEcharts4r({
      shiny$req(decision_matrix(), individual_prefs())
      outcome <- as.vector(decision_matrix() %*% individual_prefs())

      data.frame(
        Alternative = paste("Alt", seq_along(outcome)),
        Probability = outcome
      ) |>
        e_charts(Alternative) |>
        e_bar(Probability) |>
        e_title("Group Decision Probabilities") |>
        e_axis(axis = "y", max = 1, name = "Probability") |>
        e_tooltip()
    })
  })
}
