telemetry_ui <- function(css_path) {
  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header = header_telemetry(css_path = css_path),
    sidebar = sidebar_telemetry,
    body = body_telemetry
  )
}

telemetry_server <- function(input, output, session, data_storage) {
  session$user <- get_user(session)

  prepare_admin_panel_components(input, output, session, data_storage = data_storage)
}

#' Run example telemetry analytics dashboard
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @export
run_analytics_dashboard <- function(data_storage, css_path = NULL) {
  shiny::shinyApp(
    ui = telemetry_ui(css_path = NULL),
    server = telemetry_server(input, output, session, data_storage = data_storage)
  )
}
