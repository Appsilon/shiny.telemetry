
#' Data Storage abstract class to handle all the read/write operations
#'
#' @description
#' Abstract R6 Class that encapsulates all the operations needed by
#' Shiny.telemetry to read and write. This removes the complexity from the
#' functions and uses a unified API.
DataStorage <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorage",
  public = list(

    #' @description initialize data storage object
    #'
    #' @param username string with username of the current session
    #' @param session_id string with custom session id. We recommend using the
    #' session$token from Shiny

    initialize = function(username, session_id = NULL) {
      checkmate::assert_string(username)
      checkmate::assert_string(session_id, null.ok = TRUE)

      private$.username <- username
      private$.session_id <- session_id

      # generate if session_id doesn't exist null
      if (is.null(session_id)) {
        private$.session_id <- private$generate_session_id()
      }
    },

    #' @description Insert new data
    #'
    #' @param values list of values to write to storage provider.
    #' @param bucket string with name of type of data to write (example, for
    #' SQL it should represent a table).
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session.
    #' @param force_params boolean flag that indicates if `session`,
    #' `username` and `time` parameters should be added automatically
    #' (the default behavior).

    insert = function(
      values, bucket = "user_log", add_username = TRUE, force_params = TRUE
    ) {
      values <- private$insert_checks(
        values, bucket, add_username, force_params
      )

      rlang::abort("Method not implemented.")
    },

    #' @description read all user data
    #'
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
      rlang::abort("Method not implemented.")
    },

    #' @description read all session data
    #'
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_session_data = function(date_from, date_to) {
      rlang::abort("Method not implemented.")
    },
    #' @description read all session data
    close = function() {
      private$close_connection()
    }
  ),
  active = list(

    #' @field username string field that returns the current username

    username = function() {
      private$.username
    },

    #' @field session_id string field that returns the current session_id

    session_id = function() {
      private$.session_id
    },

    #' @field action_bucket string that identifies the bucket to store user
    #' related and action data

    action_bucket = function() "user_log",

    #' @field session_bucket string that identifies the bucket to store session
    #' details data

    session_bucket = function() "session_details"
  ),
  private = list(
    .username = NULL,
    .session_id = NULL,
    check_date = function(date_value, .var_name) {
      # required parameter
      checkmate::assert_string(.var_name)

      tryCatch({
        date_value <- as.Date(date_value)
        checkmate::assert_date(date_value, .var.name = .var_name)
        date_value
      }, error = function(err) {
        date_value
        rlang::abort(glue::glue(
          "Assertion on '{.var_name}' failed: Must be of class 'Date' ",
          "or a valid date format of class 'String' ('yyyy-mm-dd')."
        ))
      })
    },
    generate_session_id = function() {
      paste(
        c(
          sample(c(letters, LETTERS, 0:9), 10),
          format(Sys.time(), "%Y%d%m%H%M%S")
        ),
        collapse = ""
      )
    },
    close_connection = function() {
      rlang::abort("Method not implemented.")
    },
    insert_checks = function(values, bucket, add_username, force_params) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)
      checkmate::assert_flag(add_username)
      checkmate::assert_flag(force_params)

      if (isTRUE(force_params) && "time" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'time' value into database.",
          " It is set automatically."
        ))
      }

      if (isTRUE(force_params) && "session" %in% names(values)) {
        rlang::abort(glue::glue(
          "You must not pass 'session' value into database.",
          " It is set automatically."
        ))
      }

      if (isTRUE(force_params) && "username" %in% names(values)) {
        rlang::abort(glue::glue(
          "You must not pass 'username' value into database.",
          " It is set automatically."
        ))
      }

      values$time <- as.character(Sys.time())

      if (
        isFALSE(force_params) && isFALSE(checkmate::test_string(values$session))
      ) {
        rlang::abort(glue::glue(
          "When the argument 'force_params' is FALSE then 'values' list must",
          " contain 'session.'"
        ))
      }

      if (isFALSE(checkmate::test_string(values$session))) {
        values$session <- private$.session_id
      }

      if (isTRUE(add_username)) {
        values$username <- private$.username
      }

      values
    }
  )
)
