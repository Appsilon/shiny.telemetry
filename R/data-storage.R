
#' Data storage abstract class
DataStorage <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorage",
  #
  #
  public = list(

    #' @description initialize data storage object
    #'
    #' @param username string with username of the current session
    #' @param session_id string with custom session id (should not be used)

    initialize = function(username, session_id = NULL) {
      checkmate::expect_string(username)
      checkmate::expect_string(session_id, null.ok = TRUE)

      private$.username <- username
      private$.session_id <- session_id

      # generate if session_id doesn't exist null
      if (is.null(session_id)) {
        private$.session_id <- private$generate_session_id()
      }
    },

    #' @description Insert new data
    #'
    #' @param values list of values to write to storage provider
    #' @param bucket string with name of type of data to write (example, for
    #' SQL it should represent a table)

    insert = function(values, bucket = "user_log") {
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
    }
  ),
  private = list(
    .username = NULL,
    .session_id = NULL,
    generate_session_id = function() {
      uuid::UUIDgenerate()
    },
    close_connection = function() {
      rlang::abort("Method not implemented.")
    }
  )
)

#' Data storage class with SQLite provider
#' @export
DataStorageRSQLite <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageRSQLite",
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
    #' @param db_path string with path to sqlfile

    initialize = function(
      username, session_id = NULL, db_path = "user_stats.sqlite"
    ) {
      super$initialize(username, session_id)

      checkmate::expect_string(username)
      logger::log_info("path to db: {db_path}")
      private$connect(db_path)

      private$initialize_connection(username)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #'
    #' @param bucket name of table to write

    insert = function(values, bucket = "user_log") {
      checkmate::expect_string(bucket)
      checkmate::expect_list(values)

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

      private$write(values, bucket)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
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
      db_data <- private$read_data("session_details", date_from, date_to)

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
    #
    #
    # Private methods

    # @name connect
    # Makes connection to database based on passed config data
    # @param db_path string with path to file

    connect = function(db_path) {
      # Initialize connection with sqlite database
      private$db_con <- odbc::dbConnect(
        RSQLite::SQLite(), dbname = db_path
      )
    },

    # @description disconnect with sql database

    close_connection = function() {
      odbc::dbDisconnect(private$db_con)
    },
    #
    #
    initialize_connection = function(username, table_schemes) {
      table_schemes <- list(
        user_log = c(
          time = "TIMESTAMP",
          session = "TEXT",
          username = "TEXT",
          action = "TEXT",
          id = "TEXT",
          value = "TEXT"
        ),
        session_details = c(
          time = "TIMESTAMP", session = "TEXT", detail = "TEXT"
        )
      )

      table_names <- names(table_schemes)

      purrr::walk2(
        table_names, table_schemes, private$create_table_from_schema
      )
      NULL
    },
    #
    create_table_from_schema = function(table_name, table_scheme) {
      if (!(table_name %in% odbc::dbListTables(private$db_con))) {
        logger::log_debug("Creating table {table_name}")
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
    #
    #
    write = function(values, bucket) {
      checkmate::expect_string(bucket)
      checkmate::expect_list(values)

      send_query_df <- as.data.frame(
        c(
          time = as.character(Sys.time()),
          session = private$.session_id,
          values
        ),
        stringsAsFactors = FALSE
      )

      odbc::dbWriteTable(
        private$db_con,
        bucket,
        send_query_df,
        overwrite = FALSE,
        append = TRUE,
        row.names = FALSE
      )
    },
    #
    #
    read_data = function(bucket, date_from, date_to) {
      checkmate::expect_string(bucket)
      checkmate::expect_date(date_from)
      checkmate::expect_date(date_to)

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
