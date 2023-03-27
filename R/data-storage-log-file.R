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
#'   username = "test_user",
#'   log_file_path = tempfile(pattern = "user_stats", fileext = ".json"),
#'   session_file_path = tempfile(pattern = "session_details", fileext = ".json")
#' )
#' data_storage$insert(list(id = "an_id", action = "click"))
#' data_storage$insert(list(id = "another_id", action = "click"))
#' data_storage$read_user_data(as.Date("2020-01-01"), as.Date("2025-01-01"))
DataStorageLogFile <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageLogFile",
  inherit = DataStorage,
  #
  # Public
  public = list(
    # @title initialize RSQLite storage provider
    # @param username string with username of the current session
    #

    #' @description
    #' Initialize the data storage class
    #' @param username string with username of the current session
    #' @param session_id string with custom session id (should not be used)
    #' @param log_file_path string with path to JSON log file user actions
    #' @param session_file_path string with path to JSON log file for the session details

    initialize = function(
    username, session_id = NULL, log_file_path, session_file_path
    ) {
      super$initialize(username, session_id)
      logger::log_info("path to file: {log_file_path}")
      private$connect(log_file_path = log_file_path, session_file_path = session_file_path)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket path to log file; defaults to `log_file_path` used when initialized
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session

    insert = function(values, bucket = private$log_file_path, add_username = TRUE) {
      checkmate::assert_list(values)
      checkmate::assert_string(bucket)
      checkmate::assert_logical(add_username)

      values <- private$insert_checks(values, add_username = add_username)

      private$write(values, bucket = bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
      log_data <- private$read_data(private$log_file_path, date_from, date_to)

      if (NROW(log_data) > 0) {
        return(dplyr::mutate(log_data, date = as.Date(.data$time)))
      }
      log_data
    },

    #' @description read all session data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_session_data = function(date_from, date_to) {
      db_data <- private$read_data(private$session_file_path, date_from, date_to)

      db_data %>%
        dplyr::select("session", "detail") %>%
        dplyr::group_by(.data$session) %>%
        dplyr::summarise(title = paste(.data$detail, collapse = " | "))
    },

    #' @description read all session data

    close = function() {
      private$close_connection()
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

    # @description reverts logger settings to default

    close_connection = function() {
    },

    insert_checks = function(values, add_username) {
      if ("time" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'time' value into database.",
          " It is set automatically."
        ))
      }

      if ("session" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'session' value into database.",
          " It is set automatically."
        ))
      }

      if ("username" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'username' value into database.",
          " It is set automatically."
        ))
      }

      values$time <- Sys.time()
      values$session <- private$.session_id

      if (add_username) {
        values$username <- private$.username
      }

      values
    },

    write = function(values, bucket) {
      cat(jsonlite::toJSON(values), file = bucket, sep = "\n", append = TRUE)
    },

    # @name read_data
    # Reads the JSON log file
    # @param bucket string with path to file
    read_data = function(bucket, date_from, date_to) {
      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from)
      checkmate::assert_date(date_to)

      json_log_msg <- readLines(bucket)
      json_log <- dplyr::bind_rows(lapply(json_log_msg, jsonlite::fromJSON))
      json_log %>%
        dplyr::filter(
          time >=  date_from,
          time <= date_to
        )
    }
  )
)
