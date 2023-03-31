analytics_ui <- function(css_path) {
  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header = analytics_header(css_path = css_path),
    sidebar = analytics_sidebar,
    body = analytics_body
  )
}

analytics_server <- function(input, output, session, data_storage) {
  session$user <- get_user(session)

  prepare_admin_panel_components(input, output, session, data_storage = data_storage)
}

#' Run example telemetry analytics dashboard
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @export
analytics_app <- function(data_storage, css_path = NULL) {
  shiny::shinyApp(
    ui = analytics_ui(css_path = NULL),
    server = analytics_server(
      input, output, session, # nolint: object_usage_linter
      data_storage = data_storage
    )
  )
}
