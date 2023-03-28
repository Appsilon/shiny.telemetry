
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
    #' @param values list of values to write to storage provider
    #' @param bucket string with name of type of data to write (example, for
    #' SQL it should represent a table)
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session
    #' @param force_params boolean flag that indicates if parameters should
    #' be generated automatically

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
      if (checkmate::test_string(date_value)) {
        date_value <- tryCatch(
          as.Date(date_value),
          error = function(err) {
            date_value
          }
        )
      }
      checkmate::assert_date(date_value, .var.name = .var_name)
      date_value
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
#'   username = "test_user",
#'   db_path = tempfile(pattern = "user_stats", fileext = ".sqlite")
#' )
#' data_storage$insert(list(id = "an_id", action = "click"))
#' data_storage$insert(list(id = "another_id", action = "click"))
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
    #' @param username string with username of the current session
    #' @param session_id string with custom session id (should not be used)
    #' @param db_path string with path to sqlfile

    initialize = function(
    username, session_id = NULL, db_path = "user_stats.sqlite"
    ) {
      super$initialize(username, session_id)

      checkmate::assert_string(username)
      logger::log_debug("path to db: {db_path}", namespace = "shiny.telemetry")
      private$connect(db_path)

      private$initialize_connection(username)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session
    #' @param force_params boolean flag that indicates if parameters should
    #' be generated automatically

    insert = function(
      values, bucket = self$action_bucket, add_username = TRUE, force_params = TRUE
    ) {
      values <- private$insert_checks(
        values, bucket, add_username, force_params
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
