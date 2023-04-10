
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
    #' @param app_name string with name of dashboard (the version can be also
    #' included in this string)
    #' @param type string that identifies the event type to store
    #' @param session (optional) string that identifies a session where the
    #' event was logged
    #' @param details atomic element of list with data to save in storage
    #' @param insert_time boolean flag that indicates if `time` parameters
    #' should be added automatically

    insert = function(
      app_name, type, session = NULL, details = NULL, insert_time = TRUE
    ) {
      values <- private$insert_checks(
        app_name, type, session, details, insert_time
      )

      private$write(values = values, bucket = self$event_bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_event_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      db_data <- private$read_data(self$event_bucket, date_from, date_to)

      if (NROW(db_data) > 0) {
        return(dplyr::mutate(db_data, date = as.Date(.data$time)))
      }
      db_data
    },

    #' @description
    #' Close the connection if necessary

    close = function() {
      private$close_connection()
    }
  ),
  active = list(

    #' @field event_bucket string that identifies the bucket to store user
    #' related and action data

    event_bucket = function() "event_log"

  ),
  private = list(
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

    close_connection = function() {
      rlang::abort("Method not implemented.")
    },

    write = function(values, bucket) {
      rlang::abort("Method not implemented.")
    },

    read_data = function(date_from, date_to) {
      rlang::abort("Method not implemented.")
    },

    insert_checks = function(app_name, type, session, details, insert_time) {
      checkmate::assert_string(app_name)
      checkmate::assert_string(type)
      checkmate::assert_string(session, null.ok = TRUE)
      checkmate::assert_flag(insert_time)
      checkmate::assert(
        .combine = "or",
        checkmate::check_scalar(details),
        checkmate::check_list(details, null.ok = TRUE)
      )

      values <- list(
        app_name = app_name,
        type = type,
        session = session,
        details = details
      )

      if (isFALSE(insert_time)) {
        return(values)
      }

      if ("time" %in% names(details)) {
        rlang::abort(paste0(
          "You must not pass 'time' value into database.",
          " It is set automatically."
        ))
      }

      values$time <- as.character(Sys.time())

      values
    }
  )
)
