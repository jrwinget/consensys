box::use(
  bsicons[bs_icon],
  bslib[
    input_task_button,
    layout_column_wrap,
    layout_columns,
    popover,
    tooltip
  ],
  shiny,
)

box::use(
  app/logic/sds_calculations[generate_decision_matrix],
  app/logic/ui_helpers[content_card],
  app/view/plot_bar,
  app/view/plot_heatmap,
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
              "Proportional" = "proportional",
              "Equiprobability" = "equiprobability",
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
        shiny$uiOutput(ns("preference_sliders"))
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
          title = shiny$tagList(
            "Decision Matrix",
            tooltip(
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
          plot_heatmap$ui(ns("plot_decision_mat")),
        ),
        content_card(
          title = "Predicted Outcomes",
          class = "shadow-none border h-100",
          header_class = "bg-white",
          body_class = "p-3",
          plot_bar$ui(ns("plot_outcome"))
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

    pref_values <- shiny$reactiveValues()

    # init preference values when num_alternatives changes
    shiny$observeEvent(input$num_alternatives, {
      n <- input$num_alternatives
      if (!is.null(n) && n >= 2) {
        for (i in seq_len(n)) {
          key <- paste0("pref_", i)
          if (is.null(pref_values[[key]])) {
            pref_values[[key]] <- 1 / n
          }
        }

        all_keys <- names(pref_values)
        pref_keys <- grep("^pref_\\d+$", all_keys, value = TRUE)
        for (key in pref_keys) {
          pref_num <- as.numeric(sub("pref_", "", key))
          if (pref_num > n) {
            pref_values[[key]] <- NULL
          }
        }
      }
    })

    # update pref sliders
    output$preference_sliders <- shiny$renderUI({
      shiny$req(input$num_alternatives)
      n <- input$num_alternatives

      sliders <- lapply(1:n, function(i) {
        key <- paste0("pref_", i)
        initial_value <- pref_values[[key]] %||% (1 / n)

        shiny$sliderInput(
          inputId = session$ns(key),
          label = paste("Alternative", i, "preference proportion:"),
          min = 0,
          max = 1,
          value = initial_value,
          step = 0.01
        )
      })

      shiny$div(sliders)
    })

    shiny$observe({
      shiny$req(input$num_alternatives)
      for (i in seq_len(input$num_alternatives)) {
        key <- paste0("pref_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            pref_values[[key]] <- input[[key]]
          })
        }
      }
    })

    # update model only when button is clicked
    shiny$observeEvent(input$update_sds, {
      shiny$req(input$update_sds > 0, input$num_alternatives, input$sds_type)
      n <- input$num_alternatives

      # update individual prefs
      prefs <- numeric(n)
      for (i in 1:n) {
        key <- paste0("pref_", i)
        prefs[i] <- input[[key]] %||% pref_values[[key]] %||% (1 / n)
      }

      prefs <- prefs / sum(prefs) # normalize prefs
      individual_prefs(prefs)

      scheme_name <- switch(input$sds_type,
        "truth" = "truth_wins",
        input$sds_type
      )

      # update matrix
      decision_matrix(generate_decision_matrix(scheme_name, n))
    })

    # plots --------------------------------------------------------------------
    plot_heatmap$server(
      "plot_decision_mat",
      decision_matrix
    )

    output$model_status <- shiny$renderText({
      if (is.null(input$update_sds) || input$update_sds == 0) {
        "Ready"
      } else {
        "Complete"
      }
    })

    plot_bar$server(
      "plot_outcome",
      res = individual_prefs,
      y_var = "Probability",
      title = "Group Decision Probabilities",
      decision_mat = decision_matrix
    )
  })
}
