box::use(
  bsicons[bs_icon],
  bslib[layout_column_wrap, accordion, accordion_panel],
  shiny[tags],
)

box::use(
  app/logic/ui_helpers[content_card, metric_box],
)

#' @export
ui <- function(id) {
  tags$div(
    # visual hierarchy: key metrics first
    layout_column_wrap(
      width = 1 / 3,
      fill = FALSE,
      heights_equal = "row",
      class = "mb-3",
      metric_box(
        title = "Interactive Models",
        value = "2",
        subtitle = "SDS & SJS Theories",
        icon_name = "diagram-3",
        theme = "primary"
      ),
      metric_box(
        title = "Visualization Types",
        value = "5+",
        subtitle = "Dynamic charts & plots",
        icon_name = "graph-up",
        theme = "success"
      ),
      metric_box(
        title = "Learning Time",
        value = "< 10 min",
        subtitle = "Quick to understand",
        icon_name = "clock",
        theme = "info"
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      class = "bslib-gap-spacing",
      # dual-processing theory: quick overview + detailed info
      content_card(
        title = tags$span(
          bs_icon("rocket-takeoff"),
          "Quick Start Guide"
        ),
        header_class = "card-header bg-primary text-white",
        tags$div(
          class = "lead mb-3",
          paste(
            "Explore how groups make decisions together through interactive",
            "simulations."
          )
        ),
        # progressive disclosure with accordion
        accordion(
          id = "intro_accordion",
          open = "panel1",
          class = "mt-2",
          accordion_panel(
            "What is Social Decision Scheme (SDS)?",
            value = "panel1",
            icon = bs_icon("people-fill"),
            tags$p(
              "SDS theory explains how groups combine individual preferences into 
              collective decisions using mathematical rules like majority vote, 
              unanimity, or proportional representation."
            ),
            tags$div(
              class = "alert alert-info mb-0",
              bs_icon("lightbulb"),
              tags$strong("Try it:"),
              " See how different voting rules change group outcomes!"
            )
          ),
          accordion_panel(
            "What is Social Judgment Scheme (SJS)?",
            value = "panel2",
            icon = bs_icon("clipboard2-pulse-fill"),
            tags$p(
              "SJS theory models how group members influence each other's 
              continuous judgments (like ratings or estimates) through discussion 
              and persuasion."
            ),
            tags$div(
              class = "alert alert-success mb-0",
              bs_icon("lightbulb"),
              tags$strong("Try it:"),
              " Watch opinions converge or polarize over rounds!"
            )
          ),
          accordion_panel(
            "Getting Started",
            value = "panel3",
            icon = bs_icon("play-circle-fill"),
            tags$ol(
              class = "mb-0",
              tags$li(
                tags$strong("Choose a model:"),
                paste(
                  " Start with SDS for discrete choices or SJS for continuous",
                  "judgments"
                )
              ),
              tags$li(
                tags$strong("Set parameters:"),
                " Adjust group size, decision rules, and initial positions"
              ),
              tags$li(
                tags$strong("Run simulation:"),
                " Watch the dynamics unfold in real-time visualizations"
              ),
              tags$li(
                tags$strong("Explore results:"),
                " Compare different scenarios to understand group dynamics"
              )
            )
          )
        )
      )
    ),
    # information scent: clear next steps
    content_card(
      class = "card-body py-4",
      tags$div(
        class = "text-center",
        tags$h5(class = "mb-2", "Ready to explore?"),
        tags$p(
          class = "text-muted mb-3", 
          "Choose a model from the navigation above to begin"
        ),
        tags$div(
          class = "d-flex justify-content-center gap-3",
          tags$a(
            href = "#",
            class = "btn btn-primary",
            onclick = "document.querySelector('[data-value=\"sds\"]').click();",
            bs_icon("diagram-3"),
            "Try SDS Model"
          ),
          tags$a(
            href = "#",
            class = "btn btn-success",
            onclick = "document.querySelector('[data-value=\"sjs\"]').click();",
            bs_icon("sliders"),
            "Try SJS Model"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  # TODO: remove after updating tests - no server logic needed for intro tab
}
