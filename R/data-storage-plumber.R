#' Data storage class with SQLite provider
#'
#' @description
#' Implementation of the DataStorage R6 class to SQLite backend using a unified
#' API for read/write operations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_storage <- DataStoragePlumber$new(
#'   username = "test_user",
#'   hostname = "127.0.0.1",
#'   port = 8087,
#'   protocol = "http",
#'   secret = "9600bdee40db447fb372dd50e11e3f14"
#' )
#'
#' data_storage$insert(list(id = "an_id", action = "click"))
#' data_storage$insert(list(id = "another_id", action = "click"))
#'
#' data_storage$insert(
#'   list(detail = "a detail"),
#'   add_username = FALSE,
#'   bucket = data_storage$session_bucket
#' )
#'
#' data_storage$read_user_data("2020-01-01", "2025-01-01")
#' data_storage$read_session_data("2020-01-01", "2025-01-01")
#' }
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
    #' @param hostname string with hostname of plumber instance
    #' @param port numeric value with port number of plumber instance
    #' @param protocol string with protocol of the connection of the plumber
    #' instance
    #' @param secret string with secret to sign communication with plumber (can
    #' be NULL for disabling communication signing)

    initialize = function(
      username,
      session_id = NULL,
      hostname = "127.0.0.1",
      port = 8087,
      protocol = "http",
      secret = NULL
    ) {
      super$initialize(username, session_id)

      checkmate::assert_string(username)
      logger::log_debug("path: {hostname}:{port}", namespace = "shiny.telemetry")

      private$hostname <- hostname
      private$port <- port
      private$protocol <- protocol
      private$secret <- secret
      private$id <- build_id_from_secret(secret)
    },

    #' @description Insert new data
    #' @param values list of values to write to database
    #' @param bucket name of table to write
    #' @param add_username boolean flag that indicates if line should include
    #' the username of the current session
    #' @param force_params boolean flag that indicates if parameters should
    #' be generated automatically

    insert = function(
      values,
      bucket = self$action_bucket,
      add_username = TRUE,
      force_params = TRUE
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

      db_data <- private$read_data(
        self$action_bucket,
        as.Date(date_from),
        as.Date(date_to)
      )

      if (NROW(db_data) > 0) {
        return(dplyr::mutate(db_data, date = as.Date(.data$time)))
      }
      db_data
    },

    #' @description read all session data from SQLite
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_session_data = function(date_from, date_to) {
      date_from <- private$check_date(date_from, .var_name = "date_from")
      date_to <- private$check_date(date_to, .var_name = "date_to")

      private$read_data(
        self$session_bucket,
        date_from,
        date_to
      )
    },

    #' @description read all session data

    close = function() {
      private$close_connection()
    }
  ),
  active = list(

    #' @field action_read_endpoint string field that returns read action
    #' endpoint

    action_read_endpoint = function() { "read_user_data" },

    #' @field session_read_endpoint string field that returns read session
    #' endpoint

    session_read_endpoint = function() { "read_session_data" },

    #' @field action_insert_endpoint string field that returns insert action
    #' endpoint

    action_insert_endpoint = function() { "user_log" },

    #' @field session_insert_endpoint string field that returns insert session
    #' endpoint

    session_insert_endpoint = function() { "session_details" }
  ),
  #
  # Private
  private = list(
    # Private Fields
    hostname = NULL,
    port = NULL,
    protocol = NULL,
    secret = NULL,
    id = NULL,

    # Private methods

    build_url = function(path) {
      glue::glue(
        "{private$protocol}://{private$hostname}:{private$port}/{path}"
      )
    },

    write = function(values, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)

      endpoint <- bucket

      logger::log_debug(
       "values (names): ({NROW(names(values))}) ",
       "{names(values) |> paste(collapse = \",\")}"
      )
      logger::log_debug(
        "values (class): ({NROW(values)}) ",
        "{sapply(values, class) |> paste(collapse = \", \")}"
      )
      logger::log_debug(
        "values (el hash): ({NROW(values)}) ",
        "{",
        "purrr::map(",
        "  values,",
        "  ~ substr(",
        "    digest::digest(.x, algo = 'sha256'), start = 1, stop = 6)",
        "  ) |> ",
        "paste(collapse = \", \")",
        "}"
      )
      logger::log_debug(
        "secret: {substr(private$secret, start = 1, stop = 6)}..."
      )
      logger::log_debug("secret: {private$id}")

      httr2::request(private$build_url(endpoint)) %>%
        httr2::req_headers("Accept" = "application/json") %>%
        httr2::req_body_json(
          list(
            id = private$id,
            token = build_token(values, secret = private$secret),
            data = jsonlite::serializeJSON(values)
          )
        ) %>%
        httr2::req_method("POST") %>%
        httr2::req_perform()

      invisible(TRUE)
    },

    read_data = function(bucket, date_from, date_to) {
      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from)
      checkmate::assert_date(date_to)

      endpoint <- dplyr::case_when(
        # API endpoints
        bucket == self$action_bucket ~ self$action_read_endpoint,
        bucket == self$session_bucket ~ self$session_read_endpoint,
        .default = NULL
      )

      if (is.null(endpoint)) {
        rlang::abort("reading data from invalid bucket.")
      }

      body <- httr2::request(private$build_url(endpoint)) %>%
        httr2::req_url_query(
          from = date_from,
          to = date_to,
          token = build_token(
            list(from = date_from, to = date_to),
            secret = private$secret
          ),
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
