#' Data storage class for JSON Log File
#'
#' @description
#' Implementation of the DataStorage R6 class to a JSON log file backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' data_storage <- DataStorageLogFile$new(
#'   log_file_path = tempfile(pattern = "user_stats", fileext = ".json"),
#'   session_file_path = tempfile(pattern = "session_details", fileext = ".json")
#' )
#'
#' log_login(data_storage)
#'
#' log_click(data_storage, "an_id")
#' log_click(data_storage, "a_different_id")
#'
#' log_session_detail(data_storage, detail = "some detail")
#'
#' data_storage$read_user_data("2020-01-01", "2025-01-01")
#' data_storage$read_session_data("2020-01-01", "2025-01-01")
DataStorageLogFile <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageLogFile",
  inherit = DataStorage,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param log_file_path string with path to JSON log file user actions
    #' @param session_file_path string with path to JSON log file for the session details

    initialize = function(
      log_file_path, session_file_path
    ) {
      super$initialize()
      logger::log_debug("path to file: {log_file_path}", namespace = "shiny.telemetry")
      private$connect(log_file_path = log_file_path, session_file_path = session_file_path)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket path to log file; defaults to `log_file_path` used when initialized

    insert = function(values, bucket = private$log_file_path) {
      values <- private$insert_checks(values, bucket = bucket)

      checkmate::assert_choice(
        bucket, c(self$action_bucket, self$session_bucket)
      )

      if (!is.null(values$value))
        values$value <- as.character(values$value)

      private$write(values, bucket = bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      log_data <- private$read_data(
        private$log_file_path,
        date_from,
        date_to,
        empty_template = dplyr::tibble(
          time = character(),
          dashboard = character(),
          version = character(),
          session = character(),
          username = character(),
          action = character(),
          id = character(),
          value = character()
        )
      )

      if (NROW(log_data) > 0) {
        return(dplyr::mutate(log_data, date = as.Date(.data$time)))
      }
      log_data
    },

    #' @description read all session data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_session_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      db_data <- private$read_data(
        private$session_file_path,
        date_from,
        date_to,
        empty_template = dplyr::tibble(
          time = character(),
          dashboard = character(),
          version = character(),
          session = character(),
          detail = character()
        )
      )

      db_data %>%
        dplyr::select("session", "detail") %>%
        dplyr::group_by(.data$session) %>%
        dplyr::summarise(title = paste(.data$detail, collapse = " | "))
    },

    #' @description
    #' Does nothing, but needs to be kept here because log_logout calls this
    #' for database backends further discussion needed if closing connecting
    #' is really necessary.
    #' @description does nothing, defined for API consistency
    close = function() {
    }
  ),
  active = list(

    #' @field action_bucket string that identifies the file path to store user
    #' related and action data

    action_bucket = function() {
      private$log_file_path
    },

    #' @field session_bucket string that identifies the bucket to store session
    #' details data

    session_bucket = function() {
      private$session_file_path
    }
  ),
  #
  # Private
  private = list(
    # Private fields
    log_file_path = NULL,

    session_file_path = NULL,

    # Private methods

    # @name connect
    # Makes connection to database based on passed config data
    # @param log_file_path string with path to file for user actions
    # @param session_file_path string with path to file for session details

    connect = function(log_file_path, session_file_path) {
      private$log_file_path <- log_file_path
      private$session_file_path <- session_file_path
    },

    write = function(values, bucket) {
      cat(jsonlite::toJSON(values), file = bucket, sep = "\n", append = TRUE)
    },

    # @name read_data
    # Reads the JSON log file
    # @param bucket string with path to file
    read_data = function(
      bucket, date_from, date_to, empty_template = dplyr::tibble()
    ) {

      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from)
      checkmate::assert_date(date_to)

      if (!file.exists(bucket)) {
        return(empty_template)
      }

      readLines(bucket) %>%
        lapply(jsonlite::fromJSON) %>%
        dplyr::bind_rows() %>%
        dplyr::filter(
          time >=  date_from,
          time <= date_to
        )
    }
  )
)
