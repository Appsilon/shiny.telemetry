analytics_ui <- function() {
  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header = analytics_header(
      css_path = system.file(
        "examples", "app", "analytics", "www", "styles.css", package = "shiny.telemetry"
      )
    ),
    sidebar = analytics_sidebar,
    body = analytics_body
  )
}

analytics_server <- function(data_storage) {
  shiny::shinyServer(function(input, output, session) {
    session$user <- get_user(session)

    data_storage <- data_storage

    prepare_admin_panel_components(input, output, session, data_storage = data_storage)
  })
}

#' Run example telemetry analytics dashboard
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @export
analytics_app <- function(data_storage) {
  shiny::shinyApp(
    ui = analytics_ui(),
    server = analytics_server(data_storage = data_storage)
  )
}
