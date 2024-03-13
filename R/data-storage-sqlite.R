#' Data storage class with SQLite provider
#'
#' @description
#' Implementation of the [`DataStorage`] R6 class to SQLite backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' db_path <- tempfile(fileext = ".sqlite")
#' data_storage <- DataStorageSQLite$new(db_path = db_path)
#'
#' data_storage$insert("example", "test_event", "session1")
#' data_storage$insert("example", "input", "s1", list(id = "id1"))
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
#' file.remove(db_path)
DataStorageSQLite <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageSQLite",
  inherit = DataStorageSQLFamily,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param db_path string with path to `SQLite` file.

    initialize = function(
      db_path = "user_stats.sqlite"
    ) {

      logger::log_debug("path to db: {db_path}", namespace = "shiny.telemetry")
      private$connect(db_path)

      private$initialize_connection()
    }

  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,

    # Private methods

    connect = function(db_path) {
      # Initialize connection with sqlite database
      private$db_con <- odbc::dbConnect(RSQLite::SQLite(), dbname = db_path)
    }
  )
)
