#' Run example telemetry analytics dashboard
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @export
run_analytics_dashboard <- function(data_storage) {
  shiny::shinyAppDir(appDir = system.file("templates/app/analytics", package = "shiny.telemetry"))
}
