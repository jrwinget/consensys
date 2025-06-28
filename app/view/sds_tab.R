box::use(
  bsicons[bs_icon],
  bslib[
    input_task_button,
    layout_column_wrap,
    layout_columns,
    popover,
    tooltip
  ],
  echarts4r,
  reactable[JS],
  shiny,
)

box::use(
  app/logic/sds_calculations[generate_decision_matrix],
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
        "Social Decision Scheme (SDS) Model",
        tooltip(
          bs_icon("info-circle"),
          "Transform individual preferences into group decisions",
          placement = "right"
        )
      ),
      shiny$tags$p(
        class = "lead mb-3",
        "The SDS model transforms distributions of individual preferences into
        group decision probabilities using transition matrices."
      ),
      shiny$tags$div(
        class = "alert alert-info alert-dismissible fade show",
        shiny$tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "alert"
        ),
        bs_icon("lightbulb"),
        shiny$tags$strong("Key Insight:"),
        " Different decision rules can lead to vastly different group outcomes!"
      )
    ),
    # settings -----------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      content_card(
        title = "Decision Scheme Settings",
        header_content = popover(
          bs_icon("question-circle"),
          title = "Decision Rules Explained",
          shiny$tags$ul(
            class = "mb-0",
            shiny$tags$li(
              shiny$tags$b("Majority:"),
              " More than 50% agreement"
            ),
            shiny$tags$li(
              shiny$tags$b("Proportional:"),
              " Probability matches vote share"
            ),
            shiny$tags$li(
              shiny$tags$b("Two-Thirds:"),
              " Requires 67% agreement"
            ),
            shiny$tags$li(
              shiny$tags$b("Unanimity:"),
              " Everyone must agree"
            ),
            shiny$tags$li(
              shiny$tags$b("Truth-Wins:"),
              " Correct answer always prevails"
            )
          )
        ),
        shiny$div(
          class = "mb-3",
          shiny$selectInput(
            ns("sds_type"),
            "Decision Scheme Type:",
            choices = c(
              "Majority Rule" = "majority",
              "Proportionality Rule" = "proportional",
              "Two-Thirds Majority" = "two_thirds",
              "Unanimity" = "unanimity",
              "Truth-Wins" = "truth",
              "Custom" = "custom"
            ),
            selected = "majority",
            width = "100%"
          )
        ),
        shiny$div(
          class = "mb-3",
          shiny$numericInput(
            ns("num_alternatives"),
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
        shiny$uiOutput(ns("custom_matrix_input")),
        shiny$div(
          class = "d-grid mt-3",
          input_task_button(
            ns("update_sds"),
            shiny$tagList(
              bs_icon("arrow-clockwise"),
              "Update Model"
            ),
            class = "btn-lg btn-primary"
          )
        )
      ),
      content_card(
        title = "Individual Preference Distribution",
        header_content = tooltip(
          bs_icon("bar-chart"),
          "Set how many group members prefer each option",
          placement = "top"
        ),
        shiny$uiOutput(ns("preference_sliders")),
        shiny$tags$hr(class = "my-3"),
        echarts4r$echarts4rOutput(ns("preference_plot"), height = "200px")
      )
    ),
    # results ------------------------------------------------------------------
    content_card(
      title = shiny$tagList(
        "Results",
        shiny$tags$span(
          class = "badge bg-secondary ms-2",
          shiny$textOutput(ns("model_status"), inline = TRUE)
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        content_card(
          title = "Decision Matrix",
          class = "shadow-none border h-100",
          header_class = "bg-white",
          body_class = "p-3",
          echarts4r$echarts4rOutput(ns("decision_matrix_plot"), height = "350px")
        ),
        content_card(
          title = "Predicted Outcomes",
          class = "shadow-none border h-100",
          header_class = "bg-white",
          body_class = "p-3",
          echarts4r$echarts4rOutput(ns("outcome_plot"), height = "350px")
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

    # auto-update when inputs change
    shiny$observe({
      shiny$req(input$num_alternatives, input$sds_type)
      n <- input$num_alternatives
      
      # update matrix when scheme or alternatives change
      decision_matrix(generate_decision_matrix(input$sds_type, n))
      
      # update prefs when alternatives change
      prefs <- numeric(n)
      for (i in 1:n) {
        pref_value <- input[[paste0("pref_", i)]]
        prefs[i] <- if (!is.null(pref_value)) pref_value else 1 / n
      }
      prefs <- prefs / sum(prefs)
      individual_prefs(prefs)
    })

    # plots --------------------------------------------------------------------
    output$preference_plot <- echarts4r$renderEcharts4r({
      prefs <- individual_prefs()

      data.frame(
        Alternative = paste("Alt", seq_along(prefs)),
        Proportion = prefs
      ) |>
        echarts4r$e_charts(Alternative) |>
        echarts4r$e_bar(Proportion) |>
        echarts4r$e_title("Individual Preference Distribution") |>
        echarts4r$e_axis(axis = "y", max = 1, name = "Proportion") |>
        echarts4r$e_tooltip()
    })

    output$decision_matrix_plot <- echarts4r$renderEcharts4r({
      shiny$req(decision_matrix())
      matrix_data <- decision_matrix()
      n <- nrow(matrix_data)
      
      # Create heatmap data
      heatmap_data <- expand.grid(
        From = paste("Alt", 1:n),
        To = paste("Alt", 1:n)
      )
      heatmap_data$Value <- as.vector(matrix_data)
      
      heatmap_data |>
        echarts4r$e_charts(From) |>
        echarts4r$e_heatmap(To, Value) |>
        echarts4r$e_visual_map(min = 0, max = 1) |>
        echarts4r$e_title("Decision Matrix (Transition Probabilities)") |>
        echarts4r$e_tooltip(formatter = JS(
          "function(params) {
            return params.marker + params.data[0] + ' → ' + params.data[1] + '<br/>Probability: ' + params.data[2].toFixed(3);
          }"
        ))
    })

    output$model_status <- shiny$renderText({
      if (is.null(decision_matrix()) || is.null(individual_prefs())) {
        "Ready"
      } else {
        "Active"
      }
    })

    output$outcome_plot <- echarts4r$renderEcharts4r({
      shiny$req(decision_matrix(), individual_prefs())
      outcome <- as.vector(decision_matrix() %*% individual_prefs())

      data.frame(
        Alternative = paste("Alt", seq_along(outcome)),
        Probability = outcome
      ) |>
        echarts4r$e_charts(Alternative) |>
        echarts4r$e_bar(Probability) |>
        echarts4r$e_title("Group Decision Probabilities") |>
        echarts4r$e_axis(axis = "y", max = 1, name = "Probability") |>
        echarts4r$e_tooltip()
    })
  })
}
