#' Data storage class with SQLite provider
#'
#' @description
#' Implementation of the DataStorage R6 class to SQLite backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' data_storage <- DataStoragePlumber$new(
#'   username = "test_user",
#'   hostname = "127.0.0.1",
#'   port = 8087,
#'   protocol = "http",
#'   token = "9600bdee40db447fb372dd50e11e3f14"
#' )
#' data_storage$insert(list(id = "an_id", action = "click"))
#' data_storage$insert(list(id = "another_id", action = "click"))
#' data_storage$read_user_data(as.Date("2020-01-01"), as.Date("2025-01-01"))
DataStoragePlumber <- R6::R6Class( # nolint object_name_linter
  classname = "DataStoragePlumber",
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
      username,
      session_id = NULL,
      hostname = "127.0.0.1",
      port = 8087,
      protocol = "http",
      token = NULL
    ) {
      super$initialize(username, session_id)

      checkmate::assert_string(username)
      logger::log_debug("path: {hostname}:{port}", namespace = "shiny.telemetry")

      private$hostname <- hostname
      private$port <- port
      private$protocol <- protocol
      private$token <- token
      private$id <- digest::digest(token, algo = "sha256") %>%
        stringr::str_sub(end = 8)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session

    insert = function(
      values, bucket = "user_log", add_username = TRUE, force_params = TRUE
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
    hostname = NULL,
    port = NULL,
    protocol = NULL,
    token = NULL,
    id = NULL,

    # Private methods

    build_token = function(...) {
      params <- as.list(...)

      if (is.null(private$id)) {
        return(digest::digest(params, algo = "sha256"))
      }

      if (!checkmate::test_string(private$id)) {
        rlang::abort("ID must be NULL or a string")
      }

      digest::digest(list(params, private$token), algo = "sha256")
    },

    build_url = function(path) {
      glue::glue(
        "{private$protocol}://{private$hostname}:{private$port}/{path}"
      )
    },

    write = function(values, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)

      httr2::request(private$build_url("user")) %>%
        httr2::req_headers("Accept" = "application/json") %>%
        httr2::req_body_json(
          list(
            id = private$id,
            token = private$build_token(values),
            data = jsonlite::serializeJSON(values)
          )
        ) %>%
        httr2::req_method("POST") %>%
        httr2::req_perform()
    },

    read_data = function(bucket, date_from, date_to) {
      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from)
      checkmate::assert_date(date_to)

      url_path <- dplyr::case_when(
        bucket == "user_log" ~ "read_user_data",
        bucket == "session_details" ~ "read_session_data",
        .default = NULL
      )

      if (is.null(url_path)) {
        rlang::abort("reading data from invalid bucket.")
      }

      body <- httr2::request(private$build_url(url_path)) %>%
        httr2::req_url_query(
          from = date_from,
          to = date_to,
          token = private$build_token(list(from = date_from, to = date_to)),
          id = private$id
        ) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      body$result %>%
        purrr::pluck(1) %>%
        jsonlite::unserializeJSON()
    }
  )
)
