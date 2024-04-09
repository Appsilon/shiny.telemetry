# ui section -------------------------------------------------------------------
empty_app_ui <- function() {
  ## header section ------------------------------------------------------------
  header <- semantic.dashboard::dashboardHeader(
    style = "min-height: 100%;",
    shiny::includeCSS(
      system.file(
        "examples", "app", "analytics", "www", "styles.css", package = "shiny.telemetry"
      )
    ),
    shiny::tags$div(
      class = "right menu",
      shiny::tags$a(
        class = "icon item", href = "../../logout",
        semantic.dashboard::icon("power off")
      )
    )
  )

  ## sidebar section -----------------------------------------------------------
  sidebar <- semantic.dashboard::dashboardSidebar(
    disable = TRUE
  )

  ## main body section ---------------------------------------------------------
  body <- semantic.dashboard::dashboard_body(
    shiny::tags$div(
      class = "ui two column centered grid",
      shiny::tags$div(
        class = "column",
        shiny::tags$div(
          class = "ui placeholder segment",
          shiny::tags$div(
            class = "ui icon header red",
            shiny::tags$i(class = "exclamation triangle icon"),
            shiny::tags$div(
              class = "content",
              "No telemetry data available!!",
              shiny::tags$div(
                class = "sub header",
                "Please run the Instrumentation app first to generate some data"
              )
            )
          )
        )
      )
    )
  )

  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header,
    sidebar,
    body
  )
}

# server section ---------------------------------------------------------------
empty_app_server <- function() {
  shiny::shinyServer(function(input, output, session) {})
}

#' Shiny app to run when no telemetry data is available
#'
#' @return An object that represents the empty shiny app. Printing the object or
#' passing it to `shiny::runApp()` will run it.
#'
#' @export
empty_app <- function() {
  shiny::shinyApp(
    ui = empty_app_ui(),
    server = empty_app_server()
  )
}
