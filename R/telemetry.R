# REMOVE ME!!
logger::log_threshold("DEBUG", namespace = "shiny.telemetry")

#' Telemetry class to manage telemetry at a global level (outside a session)
#'
#' @description
#'
#'
#' @export
#' @examples
#' telemetry <- Telemetry$new()
#'
#' mock_session <- list(clientData = list(url_search = ""))
#' ts <- telemetry$start_session(mock_session, track_inputs = FALSE, logout = FALSE)
#'
#' telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |> tail()
Telemetry <- R6::R6Class( # nolint object_name_linter
  classname = "Telemetry",
  public = list(

    data_storage = NULL,

    #' @description
    #'
    #' @param name (optional) string that identifies the name of the dashboard.
    #' By default it will store data with `(dashboard)`.
    #' @param version (optional) string that identifies the version of the
    #' dashboard. By default it will use `v0.0.0`.
    #' @param data_storage (optional) DataStorage instance where telemetry
    #' data is being stored.
    #' It can take any of data storage providers by this package,
    #' By default it will store in a sqlite local database on the current
    #' working directory with filename `telemetry.sqlite`

    initialize = function(
      name = "(dashboard)",
      version = "v0.0.0",
      data_storage = DataStorageRSQLite$new(
        db_path = file.path("telemetry.sqlite")
      )
    ) {
      self$data_storage <- data_storage
      private$.name <- name
      private$.version <- version
    },

    #' @description
    #' Setup basic telemetry
    #'
    #' @param session

    start_session = function(
      input,
      session = shiny::getDefaultReactiveDomain(),
      track_inputs = TRUE,
      track_values = FALSE,
      login = TRUE,
      logout = TRUE,
      browser_version = TRUE,
      save_in_session = TRUE
    ) {

      username <- shiny::isolate(
        shiny::parseQueryString(session$clientData$url_search)$username
      )
      if (is.null(username)) username <- "unknown_user"

      telemetry_proxy <- TelemetryProxy$new(
        self, username, session$token
      )

      if (isTRUE(track_inputs) && isFALSE(private$track_all_inputs_flag)) {
        telemetry_proxy$log_all_inputs(input, track_values)
        private$track_all_inputs_flag <- TRUE
      }

      if (isTRUE(login)) {
        telemetry_proxy$log_login()
      }

      if (isTRUE(logout)) {
        telemetry_proxy$log_logout(session)
      }

      if (isTRUE(browser_version)) {
        telemetry_proxy$log_browser_version(input)
      }

      if (isTRUE(save_in_session)) {
        session$userData$telemetry_proxy <- telemetry_proxy
      }

      telemetry_proxy
    }
  ),
  active = list(
    dashboard = function() private$.name,
    version = function() private$.version
  ),
  private = list(
    track_all_inputs_flag = FALSE,
    .name = NULL,
    .version = NULL
  )
)
