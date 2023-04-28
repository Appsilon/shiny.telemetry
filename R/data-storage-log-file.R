#' Data storage class for JSON Log File
#'
#' @description
#' Implementation of the DataStorage R6 class to a JSON log file backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' log_file_path <- tempfile(fileext = ".txt")
#' data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)
#'
#' data_storage$insert("example", "test_event", "session1")
#' data_storage$insert("example", "input", "s1", list(id = "id"))
#' data_storage$insert("example", "input", "s1", list(id = "id2", value = 32))
#'
#' data_storage$insert(
#'   "example", "test_event_3_days_ago", "session1",
#'   time = lubridate::as_datetime(lubridate::today() - 3)
#' )
#'
#' data_storage$read_event_data()
#' data_storage$read_event_data(Sys.Date() - 1, Sys.Date() + 1)
#'
#' file.remove(log_file_path)
DataStorageLogFile <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageLogFile",
  inherit = DataStorage,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param log_file_path string with path to JSON log file user actions

    initialize = function(log_file_path) {
      logger::log_debug(
        "path to file: {log_file_path}",
        namespace = "shiny.telemetry"
      )
      private$connect(log_file_path = log_file_path)
    }

  ),
  active = list(

    #' @field event_bucket string that identifies the file path to store user
    #' related and action data

    event_bucket = function() {
      private$log_file_path
    }

  ),
  #
  # Private
  private = list(
    # Private fields
    log_file_path = NULL,

    # Private methods

    # @name connect
    # Makes connection to database based on passed config data
    # @param log_file_path string with path to file for user actions

    connect = function(log_file_path) {
      checkmate::assert_path_for_output(log_file_path, overwrite = TRUE)
      private$log_file_path <- log_file_path
    },

    # @name close_connection
    # Does nothing, implemented for API consistency

    close_connection = function() {
      # Do nothing
    },

    # @name write
    # Writes to log file
    # @param values list of values to write as json line
    # @bucket string with path to file

    write = function(values, bucket) {

      values$time <- values$time %>% as.double()

      values %>%
        purrr::compact() %>%
        jsonlite::toJSON() %>%
        cat(file = bucket, sep = "\n", append = TRUE)
    },

    table_schema = dplyr::tibble(
      time = double(0),
      app_name = character(0),
      type = character(0),
      session = character(0)
    ),

    # @name read_data
    # Reads the JSON log file
    # @param bucket string with path to file
    # @param date_from date or string that indicates start of range
    # @param date_to date or string that indicates end of range

    read_data = function(date_from, date_to, bucket) {

      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from, null.ok = TRUE)
      checkmate::assert_date(date_to, null.ok = TRUE)

      if (!file.exists(bucket)) {
        return(
          private$table_schema %>%
            # Schema stores time as double
            dplyr::mutate(time = lubridate::as_datetime(time, tz = "UTC"))
        )
      }

      result <- readLines(bucket) %>%
        lapply(function(x) jsonlite::fromJSON(x, flatten = TRUE)) %>%
        dplyr::bind_rows() %>%
        dplyr::bind_rows(private$table_schema) %>%
        dplyr::mutate(time = lubridate::as_datetime(.data$time, tz = "UTC"))

      if (!is.null(date_from)) {
        result <- dplyr::filter(
          result,
          lubridate::as_date(.data$time) >= lubridate::as_date(date_from)
        )
      }
      if (!is.null(date_to)) {
        result <- dplyr::filter(
          result,
          lubridate::as_date(.data$time) <= lubridate::as_date(date_to)
        )
      }

      private$unnest_json(result, "details")
    }
  )
)
