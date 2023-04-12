#' Data storage class with SQLite provider
#'
#' @description
#' Implementation of the DataStorage R6 class to SQLite backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' data_storage <- DataStorageSQLite$new(
#'   db_path = tempfile(pattern = "user_stats", fileext = ".sqlite")
#' )
#'
#' data_storage$insert("example", "test_event", "session1")
#' data_storage$insert("example", "input", "s1", list(id = "id"))
#' data_storage$insert("example", "input", "s1", list(id = "id2", value = 32))
#'
#' data_storage$read_event_data(Sys.date() - 365, Sys.date() + 365)
DataStorageSQLite <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageSQLite",
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
        c(
          time = "TIMESTAMP",
          app_name = "TEXT",
          session = "TEXT",
          type = "TEXT",
          details = "TEXT"
        )
      )

      table_names <- c(self$event_bucket)
      names(table_schemes) <- table_names

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
      checkmate::assert_choice(bucket, choices = c(self$event_bucket))
      checkmate::assert_list(values)

      send_query_df <- dplyr::bind_rows(values)

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
