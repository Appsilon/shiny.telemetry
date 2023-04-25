#' Data storage abstract class for SQL providers
#'
#' @description
#' Abstract subclass of the DataStorage R6 class for the SQL family of
#' providers
DataStorageSQLFamily <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageSQLFamily",
  inherit = DataStorage,
  #
  # Public
  public = list(
  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,
    timestamp_wrapper = NULL,

    # Private methods

    connect = function(...) {
      # Initialize connection with sqlite database
      rlang::abort("Method not implemented.")
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

        odbc::dbSendQuery(conn = private$db_con, create_table_query) %>%
          odbc::dbClearResult()
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

    read_data = function(date_from, date_to, bucket) {
      checkmate::assert_choice(bucket, c(self$event_bucket))

      query <- build_query_sql(
        bucket, date_from, date_to, private$timestamp_wrapper
      )

      odbc::dbGetQuery(private$db_con, query) %>%
        dplyr::tibble() %>%
        private$unnest_json("details")

    }
  )
)
