
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

    insert = function(values, bucket = "user_log", add_username = TRUE) {
      private$insert_checks(values, bucket, add_username)

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

    session_bucket = function() "session_bucket"
  ),
  private = list(
    .username = NULL,
    .session_id = NULL,
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
    insert_checks = function(values, bucket, add_username) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)

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

      if ("username" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'username' value into database.",
          " It is set automatically."
        ))
      }

      values$time <- as.character(Sys.time())
      values$session <- private$.session_id

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
      logger::log_info("path to db: {db_path}")
      private$connect(db_path)

      private$initialize_connection(username)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session

    insert = function(values, bucket = "user_log", add_username = TRUE) {
      values <- private$insert_checks(values, bucket, add_username)

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

#' Data storage class for JSON Log File
#'
#' @description
#' Implementation of the DataStorage R6 class to a JSON log file backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' data_storage <- DataStorageLogFile$new(
#'   username = "test_user",
#'   log_file_path = tempfile(pattern = "user_stats", fileext = ".json")
#' )
#' data_storage$insert(list(id = "an_id", action = "click"))
#' data_storage$insert(list(id = "another_id", action = "click"))
#' data_storage$read_user_data(as.Date("2020-01-01"), as.Date("2025-01-01"))
DataStorageLogFile <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageLogFile",
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
    #' @param log_file_path string with path to JSON log file

    initialize = function(
    username, session_id = NULL, log_file_path = "user_logs.json"
    ) {
      super$initialize(username, session_id)
      # rlang::inform(glue::glue("path to file: {log_file_path}"))
      logger::log_info("path to file: {log_file_path}")
      private$connect(log_file_path = log_file_path)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write (not used)
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session (not used)

    insert = function(values, bucket = "user_log", add_username = TRUE) {
      values <- private$insert_checks(values)

      private$write(values)
    },

    #' @description read all user data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_user_data = function(date_from, date_to) {
      log_data <- private$read_data(private$log_file_path, date_from, date_to)

      if (NROW(db_data) > 0) {
        return(dplyr::mutate(log_data, date = as.Date(.data$time)))
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
    # Private fields
    log_file_path = NULL,

    # Private methods

    # @name connect
    # Makes connection to database based on passed config data
    # @param log_file_path string with path to file

    connect = function(log_file_path) {
      private$log_file_path <- log_file_path
    },

    # @description reverts logger settings to default

    close_connection = function() {
    },

    insert_checks = function(values) {
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

      if ("username" %in% names(values)) {
        rlang::abort(paste0(
          "You must not pass 'username' value into database.",
          " It is set automatically."
        ))
      }

      values$time <- Sys.time()
      values$username <- private$.username
      values$session <- private$.session_id

      values
    },

    write = function(values) {
      checkmate::expect_list(values)

      cat(jsonlite::toJSON(values), file = private$log_file_path, sep = "\n", append = TRUE)
    },

    # @name read_data
    # Reads the JSON log file
    # @param bucket string with path to file
    read_data = function(bucket, date_from, date_to) {
      checkmate::expect_string(bucket)
      checkmate::expect_date(date_from)
      checkmate::expect_date(date_to)

      json_log_msg <- readLines(bucket)
      json_log <- dplyr::bind_rows(lapply(json_log_msg, unnest_msg))
      json_log %>%
        dplyr::filter(
          date >=  date_from,
          date <= date_to
        )
    },

    unnest_msg = function(json_log_msg) {
      json_log_msg %>%
        jsonlite::fromJSON(json_log_msg) %>%
        as.data.frame(json_log_msg) %>%
        dplyr::mutate(msg = jsonlite::fromJSON(msg)) %>%
        tidyr::unnest(msg)
    }
  )
)
