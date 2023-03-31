#' Data storage class with SQLite provider
#'
#' @description
#' Implementation of the DataStorage R6 class to SQLite backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' data_storage <- DataStorageRSQLite$new(
#'   db_path = tempfile(pattern = "user_stats", fileext = ".sqlite")
#' )
#' log_login(data_storage)
#'
#' log_click(data_storage, "an_id")
#' log_click(data_storage, "a_different_id")
#'
#' log_session_detail(data_storage, detail = "some detail")
#'
#' data_storage$read_user_data(as.Date("2020-01-01"), as.Date("2025-01-01"))
#' data_storage$read_user_data("2020-01-01", "2025-01-01")
DataStorageRSQLite <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageRSQLite",
  inherit = DataStorage,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param db_path string with path to sqlfile

    initialize = function(
      db_path = "user_stats.sqlite"
    ) {
      super$initialize()

      logger::log_debug("path to db: {db_path}", namespace = "shiny.telemetry")
      private$connect(db_path)

      private$initialize_connection()
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write

    insert = function(values, bucket = self$action_bucket) {
      values <- private$insert_checks(values, bucket)

      checkmate::assert_string(
        bucket,
        pattern = c(self$action_bucket, self$session_bucket) %>%
          paste(collapse = "|")
      )

      private$write(values, bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      db_data <- private$read_data("user_log", date_from, date_to)

      if (NROW(db_data) > 0) {
        return(dplyr::mutate(db_data, date = as.Date(.data$time)))
      }
      db_data
    },

    #' @description read all session data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_session_data = function(date_from, date_to) {
      db_data <- private$read_data(self$session_bucket, date_from, date_to)

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
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,

    # Private methods

    connect = function(db_path) {
      # Initialize connection with sqlite database
      private$db_con <- odbc::dbConnect(RSQLite::SQLite(), dbname = db_path)
    },

    close_connection = function() {
      odbc::dbDisconnect(private$db_con)
    },

    initialize_connection = function() {
      table_schemes <- list(
        user_log = c(
          time = "TIMESTAMP",
          dashboard = "TEXT",
          version = "TEXT",
          session = "TEXT",
          username = "TEXT",
          action = "TEXT",
          id = "TEXT",
          value = "TEXT"
        ),
        session_details = c(
          time = "TIMESTAMP",
          dashboard = "TEXT",
          version = "TEXT",
          session = "TEXT",
          detail = "TEXT"
        )
      )

      table_names <- names(table_schemes)

      purrr::walk2(
        table_names, table_schemes, private$create_table_from_schema
      )
      NULL
    },

    create_table_from_schema = function(table_name, table_scheme) {
      if (!(table_name %in% odbc::dbListTables(private$db_con))) {
        logger::log_debug(
          "Creating table {table_name}", namespace = "shiny.telemetry"
        )
        create_table_query <- odbc::sqlCreateTable(
          con = private$db_con,
          table = table_name,
          fields = table_scheme,
          row.names = FALSE
        )
        res <- odbc::dbSendQuery(conn = private$db_con, create_table_query)
        odbc::dbClearResult(res)
      }
      NULL
    },

    write = function(values, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)

      send_query_df <- as.data.frame(values, stringsAsFactors = FALSE)

      odbc::dbWriteTable(
        private$db_con,
        bucket,
        send_query_df,
        overwrite = FALSE,
        append = TRUE,
        row.names = FALSE
      )
    },

    read_data = function(bucket, date_from, date_to) {
      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from)
      checkmate::assert_date(date_to)

      query <- glue::glue(
        .sep = " ",
        "SELECT *",
        "FROM {bucket}",
        "WHERE date(time) >= '{date_from}' AND date(time) <= '{date_to}'"
      )

      odbc::dbGetQuery(private$db_con, query) %>%
        dplyr::tibble()
    }
  )
)
