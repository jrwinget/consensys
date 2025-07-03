box::use(
  bsicons[bs_icon],
  bslib,
  shiny,
)

box::use(
  app/logic/theme[create_app_theme],
  app/view/intro_tab,
  app/view/results_tab,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_fillable(
    title = shiny$tags$span(
      "Consensys: Interactive tools for understanding group decision processes",
    ),
    theme = create_app_theme(),
    padding = 0,
    gap = 0,
    fillable_mobile = TRUE,
    bslib$layout_sidebar(
      open = "always",
      title = shiny$tags$div(
        class = "d-flex flex-column align-items-start gap-2 m-3",
        padding = 0,
        gap = 0,
        shiny$tags$img(src = "static/logo.png", height = 50),
        "Settings"
      ),
      sidebar = bslib$sidebar(
        class = "rounded-0",
        padding = 0,
        gap = 0,
        height = "100vh",
        width = "26%",
        shiny$tags$div(
          class = "sidebar-content p-3",
          # model selection
          shiny$tags$div(
            class = "mb-4",
            shiny$radioButtons(
              ns("model_type"),
              "Select Model:",
              choices = c(
                "Social Decision Scheme (SDS)" = "sds",
                "Social Judgment Scheme (SJS)" = "sjs",
                "Model Comparison" = "comparison"
              ),
              selected = "sds"
            )
          ),
          # common parameters
          shiny$tags$div(
            class = "mb-3",
            shiny$numericInput(
              ns("group_size"),
              shiny$tags$span(
                "Group Size",
                bslib$tooltip(
                  bs_icon("info-circle"),
                  "Number of individuals in the group",
                  placement = "right"
                )
              ),
              value = 5,
              min = 2,
              max = 10,
              width = "100%"
            )
          ),
          # conditional parameters based on model selection
          shiny$conditionalPanel(
            condition = paste(
              "input.model_type == 'sds' ||",
              "input.model_type == 'comparison'"
            ),
            ns = ns,
            shiny$tags$hr(class = "my-3"),
            shiny$tags$h6("SDS Parameters", class = "text-muted mb-3"),
            shiny$tags$div(
              class = "mb-3",
              shiny$selectInput(
                ns("sds_scheme"),
                "Decision Scheme:",
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
            shiny$tags$div(
              class = "mb-3",
              shiny$numericInput(
                ns("n_alternatives"),
                shiny$tags$span(
                  "Number of Alternatives",
                  shiny$tags$small(class = "text-muted", "(2-5 options)")
                ),
                value = 3,
                min = 2,
                max = 5,
                width = "100%"
              )
            ),
            shiny$uiOutput(ns("preference_sliders"))
          ),
          shiny$conditionalPanel(
            condition = "input.model_type == 'sjs' || input.model_type == 'comparison'",
            ns = ns,
            shiny$tags$hr(class = "my-3"),
            shiny$tags$h6("SJS Parameters", class = "text-muted mb-3"),
            shiny$tags$div(
              class = "mb-3",
              shiny$numericInput(
                ns("n_rounds"),
                shiny$tags$span(
                  "Number of Rounds",
                  shiny$tags$small(class = "text-muted", "(1-20 iterations)")
                ),
                value = 5,
                min = 1,
                max = 20,
                width = "100%"
              )
            ),
            shiny$tags$div(
              class = "mb-3",
              shiny$sliderInput(
                ns("self_weight"),
                "Self Weight:",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1,
                width = "100%"
              )
            ),
            shiny$uiOutput(ns("position_sliders"))
          ),
          # buttons
          shiny$tags$hr(class = "my-3"),
          shiny$tags$div(
            class = "d-grid gap-2",
            shiny$conditionalPanel(
              condition = "input.model_type == 'sds'",
              ns = ns,
              bslib$input_task_button(
                ns("run_sds"),
                shiny$tags$span(bs_icon("play-circle-fill"), "Run SDS Model"),
                class = "btn-primary btn-lg"
              )
            ),
            shiny$conditionalPanel(
              condition = "input.model_type == 'sjs'",
              ns = ns,
              bslib$input_task_button(
                ns("run_sjs"),
                shiny$tags$span(bs_icon("play-circle-fill"), "Run SJS Model"),
                class = "btn-success btn-lg"
              )
            ),
            shiny$conditionalPanel(
              condition = "input.model_type == 'comparison'",
              ns = ns,
              bslib$input_task_button(
                ns("run_comparison"),
                shiny$tags$span(bs_icon("play-circle-fill"), "Run Comparison"),
                class = "btn-info btn-lg"
              )
            )
          )
        )
      ),
      bslib$nav_panel_hidden(
        "home",
        bslib$navset_card_tab(
          id = ns("nav_tabs"),
          height = "100vh",
          bslib$nav_panel(
            title = "Introduction",
            value = "intro",
            icon = bs_icon("house-door"),
            intro_tab$ui(ns("intro"))
          ),
          bslib$nav_panel(
            title = "Results",
            value = "results",
            icon = bs_icon("graph-up"),
            results_tab$ui(ns("results"))
          ),
          bslib$nav_spacer(),
          bslib$nav_menu(
            title = bs_icon("three-dots"),
            align = "right",
            bslib$nav_item(
              shiny$tags$a(
                href = "https://github.com/jrwinget/consensys",
                shiny$tags$span(bs_icon("github"), "About")
              )
            ),
            bslib$nav_item(
              shiny$tags$a(
                href = "https://github.com/jrwinget/consensys/issues",
                shiny$tags$span(bs_icon("bug"), "Report Issue")
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # Reactive values for storing preferences and positions
    pref_values <- shiny$reactiveValues()
    position_values <- shiny$reactiveValues()

    # Initialize preference values when n_alternatives changes
    shiny$observeEvent(input$n_alternatives, {
      n <- input$n_alternatives
      if (!is.null(n) && n >= 2) {
        for (i in seq_len(n)) {
          key <- paste0("pref_", i)
          if (is.null(pref_values[[key]])) {
            pref_values[[key]] <- 1 / n
          }
        }

        # Remove extra values
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

    # Initialize position values when group_size changes
    shiny$observeEvent(input$group_size, {
      n <- input$group_size
      if (!is.null(n) && n >= 2) {
        for (i in seq_len(n)) {
          key <- paste0("pos_", i)
          if (is.null(position_values[[key]])) {
            position_values[[key]] <- sample(20:80, 1)
          }
        }

        # Remove extra values
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

    # Dynamic preference sliders for SDS
    output$preference_sliders <- shiny$renderUI({
      shiny$req(input$n_alternatives)
      n <- input$n_alternatives

      sliders <- lapply(1:n, function(i) {
        key <- paste0("pref_", i)
        initial_value <- pref_values[[key]] %||% (1 / n)

        shiny$sliderInput(
          inputId = session$ns(key),
          label = paste("Alternative", i, "preference:"),
          min = 0,
          max = 1,
          value = initial_value,
          step = 0.01,
          width = "100%"
        )
      })

      shiny$tags$div(sliders)
    })

    # Dynamic position sliders for SJS
    output$position_sliders <- shiny$renderUI({
      shiny$req(input$group_size)
      n <- input$group_size

      sliders <- lapply(1:n, function(i) {
        key <- paste0("pos_", i)
        initial_value <- position_values[[key]] %||% sample(20:80, 1)

        shiny$sliderInput(
          inputId = session$ns(key),
          label = paste("Individual", i, "position:"),
          min = 0,
          max = 100,
          value = initial_value,
          width = "100%"
        )
      })

      shiny$tags$div(sliders)
    })

    # Update stored preference values when sliders change
    shiny$observe({
      shiny$req(input$n_alternatives)
      for (i in seq_len(input$n_alternatives)) {
        key <- paste0("pref_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            pref_values[[key]] <- input[[key]]
          })
        }
      }
    })

    # Update stored position values when sliders change
    shiny$observe({
      shiny$req(input$group_size)
      for (i in seq_len(input$group_size)) {
        key <- paste0("pos_", i)
        if (!is.null(input[[key]])) {
          shiny$isolate({
            position_values[[key]] <- input[[key]]
          })
        }
      }
    })

    # Navigate to results tab and open appropriate accordion when buttons are clicked
    shiny$observeEvent(input$run_sds, {
      bslib$nav_select(session = session, "nav_tabs", selected = "results")
    })

    shiny$observeEvent(input$run_sjs, {
      bslib$nav_select(session = session, "nav_tabs", selected = "results")
    })

    shiny$observeEvent(input$run_comparison, {
      bslib$nav_select(session = session, "nav_tabs", selected = "results")
    })

    # Call child modules
    intro_tab$server("intro")

    # Pass all necessary reactive values to results tab
    results_tab$server(
      "results",
      model_type = shiny$reactive(input$model_type),
      group_size = shiny$reactive(input$group_size),
      n_alternatives = shiny$reactive(input$n_alternatives),
      n_rounds = shiny$reactive(input$n_rounds),
      sds_scheme = shiny$reactive(input$sds_scheme),
      self_weight = shiny$reactive(input$self_weight),
      pref_values = pref_values,
      position_values = position_values,
      run_sds = shiny$reactive(input$run_sds),
      run_sjs = shiny$reactive(input$run_sjs),
      run_comparison = shiny$reactive(input$run_comparison)
    )
  })
}
