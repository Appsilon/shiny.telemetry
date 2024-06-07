#' Data storage class with MongoDB provider
#'
#' @description
#' Implementation of the [`DataStorage`] R6 class to MongoDB backend using a
#' unified API for read/write operations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_storage <- DataStorageMongoDB$new(
#'   host = "localhost",
#'   db = "test",
#'   ssl_options = mongolite::ssl_options()
#' )
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
#' data_storage$close()
#' }
DataStorageMongoDB <- R6::R6Class( # nolint object_name.
  classname = "DataStorageMongoDB",
  inherit = DataStorage,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param hostname the hostname or IP address of the MongoDB server.
    #' @param port the port number of the MongoDB server (default is 27017).
    #' @param username the username for authentication (optional).
    #' @param password the password for authentication (optional).
    #' @param authdb the default authentication database (optional).
    #' @param dbname name of database (default is "shiny_telemetry").
    #' @param options Additional connection options in a named list format
    #' (e.g., list(ssl = "true", replicaSet = "myreplicaset")) (optional).
    #' @param ssl_options additional connection options such as SSL keys/certs
    #' (default is [`mongolite::ssl_options()`]).

    initialize = function(
      hostname = "localhost",
      port = 27017,
      username = NULL,
      password = NULL,
      authdb = NULL,
      dbname = "shiny_telemetry",
      options = NULL,
      ssl_options = mongolite::ssl_options()
    ) {
      # create the connection string for mongodb
      checkmate::assert_string(hostname)
      checkmate::assert_int(port)
      checkmate::assert_string(username, null.ok = TRUE)
      checkmate::assert_string(password, null.ok = TRUE)
      checkmate::assert_string(authdb, null.ok = TRUE)
      checkmate::assert_string(dbname)
      checkmate::assert_list(options, null.ok = TRUE)
      checkmate::assert_list(ssl_options, null.ok = TRUE)

      password_debug <- if (is.null(password)) {
        "(empty)"
      } else {
        digest::digest(password, algo = "sha256")
      }

      options_debug <- if (is.null(options)) {
        "(empty)"
      } else {
        jsonlite::toJSON(options, auto_unbox = TRUE)
      }

      ssl_options_debug <- if (is.null(ssl_options)) {
        "(empty)"
      } else {
        jsonlite::toJSON(unclass(mongolite::ssl_options()), auto_unbox = TRUE)
      }

      logger::log_debug(
        "Parameters for MongoDB:\n",
        "  *          username: {username %||% \"(empty)\"}\n",
        "  * password (sha256): {password_debug}\n",
        "  *     hostname:port: {hostname}:{port}\n",
        "  *           db name: {dbname}\n",
        "  *            authdb: {authdb %||% \"(empty)\"}\n",
        "  *           options: {options_debug}\n",
        "  *       ssl_options: {ssl_options_debug}\n",
        namespace = "shiny.telemetry"
      )

      private$connect(
        url = build_mongo_connection_string(
          hostname = hostname,
          port = port,
          username = username,
          password = password,
          authdb = authdb,
          options = options
        ),
        dbname,
        options = ssl_options
      )
    }
  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,

    # Private methods
    connect = function(url, dbname, options) {
      # Initialize connection with database
      private$db_con <- mongolite::mongo(
        url = url,
        db = dbname,
        collection = self$event_bucket,
        options = options
      )
    },

    close_connection = function() {
      private$db_con$disconnect()
    },

    write = function(values, bucket) {
      checkmate::assert_choice(bucket, choices = c(self$event_bucket))
      checkmate::assert_list(values)

      if (!is.null(values$details)) {
        values$details <- jsonlite::fromJSON(values$details)
      }

      private$db_con$insert(values, auto_unbox = TRUE, POSIXt = "epoch")
    },

    read_data = function(date_from, date_to, bucket) {
      checkmate::assert_choice(bucket, c(self$event_bucket))

      event_data <- private$db_con$find(
        query = build_query_mongodb(date_from, date_to),
        fields = '{"_id": false}'
      )

      if (nrow(event_data) > 0) {
        result <- event_data %>%
          dplyr::tibble() %>%
          tidyr::unnest(cols = "details") %>%
          dplyr::mutate(time = lubridate::as_datetime(as.integer(time / 1000)))

        # Force value column to be a character data type
        if ("value" %in% colnames(result)) {
          dplyr::mutate(result, value = format(value))
        } else {
          # If there is no column, then it should still be a character data type
          dplyr::mutate(result, value = NA_character_)
        }
      } else {
        dplyr::tibble(
          app_name = character(),
          type = character(),
          session = character(),
          username = character(),
          id = character(),
          value = character(),
          time = lubridate::as_datetime(NULL, tz = "UTC")
        )
      }
    }
  )
)
