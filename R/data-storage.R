
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
    #' @param time date time value indicates the moment the record was
    #' generated in UTC. By default it should be NULL and determined
    #' automatically, but in cases where it should be defined, use Sys.time()
    #' to generate it.

    insert = function(
      app_name, type, session = NULL, details = NULL, time = NULL
    ) {
      values <- private$insert_checks(
        app_name, type, session, details, time
      )

      private$write(values = values, bucket = self$event_bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_event_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      db_data <- private$read_data(date_from, date_to, self$event_bucket)

      if (NROW(db_data) > 0) {
        return(dplyr::mutate(db_data, date = as.Date(.data$time)))
      }
      db_data %>%
        dplyr::bind_rows(dplyr::tibble(
          date = character(0),
          id = character(0),
          value = character(0),
          username = character(0)
        ))
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

    read_data = function(date_from, date_to, bucket) {
      rlang::abort("Method not implemented.")
    },

    insert_checks = function(app_name, type, session, details, time) {
      checkmate::assert_string(app_name)
      checkmate::assert_string(type, null.ok = TRUE)
      checkmate::assert_string(session, null.ok = TRUE)
      checkmate::assert_class(time, "POSIXct", null.ok = TRUE)
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

      if (checkmate::test_list(details)) {
        values$details <- jsonlite::toJSON(details)
      }

      values$time <- dplyr::coalesce(time, Sys.time())
      values$time <- as.character(values$time)
      values
    },

    # Unnest JSON column from data.frame

    unnest_json = function(x, column_name) {
      checkmate::assert_data_frame(x)
      checkmate::assert_string(column_name)

      x[[column_name]] <- x[[column_name]] %>%
        purrr::map(
          function(.x) {
            if (is.null(.x) || is.na(.x) || .x == "") {
              return(data.frame(.empty = "true"))
            }

            # fromJSON() cannot be called with vector input, it needs to
            #  iterated one by one. It also does not allow for NULL, NA nor
            #  empty strings.
            tmp_result <- .x  %>%
              jsonlite::fromJSON() %>%
              purrr::compact() %>%
              as.data.frame()  %>%
              # All un-nested columns have to be character type.
              dplyr::mutate(dplyr::across(
                dplyr::everything(),
                as.character
              ))

            # Catch for when `details` json is valid, but empty.
            if (NROW(tmp_result) == 0) {
              return(data.frame(.empty = "true"))
            }

            tmp_result
          }
        ) %>%
        dplyr::bind_rows()

      x <- tidyr::unnest(x, cols = dplyr::all_of(column_name))

      if (".empty" %in% colnames(x)) {
        x <- x %>%
          dplyr::select(-dplyr::all_of(".empty"))
      }

      x
    }
  )
)
