
#' Data Storage abstract class to handle all the read/write operations
#'
#' @description
#' Abstract R6 Class that encapsulates all the operations needed by
#' Shiny.telemetry to read and write. This removes the complexity from the
#' functions and uses a unified API.
DataStorage <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorage",
  public = list(

    #' @description initialize data storage object common with all providers

    initialize = function() {

    },

    #' @description Insert new data
    #'
    #' @param values list of values to write to storage provider.
    #' @param bucket string with name of type of data to write (example, for
    #' SQL it should represent a table).

    insert = function(values, bucket = self$action_bucket) {
      values <- private$insert_checks(values, bucket)

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
    insert_checks = function(values, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)


      if ("time" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'time' value into database.",
          " It is set automatically."
        ))
      }

      if (!"session" %in% names(values)) {
        rlang::abort("You must pass 'session' value into database.")
      }

      values$time <- as.character(Sys.time())

      values
    }
  )
)
