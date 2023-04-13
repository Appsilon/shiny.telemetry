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
#' # Make sure the PLUMBER_SECRET environment variable is valid before
#' # running these examples (NULL or a valid secret)
#'
#' data_storage <- DataStoragePlumber$new(
#'   hostname = "connect.appsilon.com",
#'   path = "shiny_telemetry_plumber",
#'   port = 443,
#'   protocol = "https",
#'   authorization = Sys.getenv("CONNECT_AUTHORIZATION_KEY"),
#'   secret = Sys.getenv("PLUMBER_SECRET")
#' )
#'
#' data_storage <- DataStoragePlumber$new(
#'   hostname = "127.0.0.1",
#'   path = NULL,
#'   port = 8087,
#'   protocol = "http",
#'   secret = Sys.getenv("PLUMBER_SECRET")
#' )
#'
#' data_storage$insert("example", "test_event", "session1")
#' data_storage$insert("example", "input", "s1", list(id = "id"))
#' data_storage$insert("example", "input", "s1", list(id = "id2", value = 32))
#'
#' data_storage$insert(
#'   "example", "test_event_3_days_ago", "session1",
#'   time = lubridate::as_datetime(lubridate::today() - 3)
#' )
#'
#' data_storage$read_event_data()
#' data_storage$read_event_data(Sys.Date() - 1, Sys.Date() + 1)
#' }
DataStoragePlumber <- R6::R6Class( # nolint object_name_linter
  classname = "DataStoragePlumber",
  inherit = DataStorage,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param hostname string with hostname of plumber instance,
    #' @param port numeric value with port number of plumber instance.
    #' @param path string with sub-path of plumber deployment.
    #' @param protocol string with protocol of the connection of the plumber
    #' instance.
    #' @param secret string with secret to sign communication with plumber (can
    #' be NULL for disabling communication signing).
    #' @param authorization string to use in HTTP headers for authorization
    #' (for example: to authenticate to a plumber deployment behind a connect
    #' server).

    initialize = function(
      hostname = "127.0.0.1",
      port = 80,
      protocol = "http",
      path = NULL,
      secret = NULL,
      authorization = NULL
    ) {
      super$initialize()

      private$hostname <- hostname
      private$port <- port
      private$path <- path
      private$protocol <- protocol
      private$secret <- secret
      private$id <- build_id_from_secret(secret)
      private$authorization <- authorization

      logger::log_debug(
        "path: {private$build_url(\"health_check\")}",
        namespace = "shiny.telemetry"
      )
    }

  ),
  active = list(

    #' @field event_read_endpoint string field that returns read action
    #' endpoint

    event_read_endpoint = function() "read_data",

    #' @field event_insert_endpoint string field that returns insert action
    #' endpoint

    event_insert_endpoint = function() "insert"

  ),
  #
  # Private
  private = list(
    # Private Fields
    hostname = NULL,
    port = 80,
    path = NULL,
    protocol = NULL,
    secret = NULL,
    id = NULL,
    authorization = NULL,

    # Private methods

    build_url = function(path) {
      if (is.null(private$path)) {
        return(
          glue::glue(
            "{private$protocol}://{private$hostname}:{private$port}/{path}"
          )
        )
      }

      glue::glue(
        "{private$protocol}://{private$hostname}:{private$port}/",
        "{private$path}/{path}"
      )
    },

    # @name close_connection
    # Does nothing, implemented for API consistency

    close_connection = function() {
      # Do nothing
    },

    write = function(values, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_list(values)

      endpoint <- dplyr::case_when(
        # API endpoints
        bucket == self$event_bucket ~ self$event_insert_endpoint,
        .default = NULL
      )

      if (is.null(endpoint)) {
        rlang::abort("writing to invalid bucket.")
      }

      logger::log_debug(
       "values (names): ({NROW(names(values))}) ",
       "{names(values) |> paste(collapse = \",\")}",
       namespace = "shiny.telemetry"
      )
      logger::log_debug(
        "values (class): ({NROW(values)}) ",
        "{sapply(values, class) |> paste(collapse = \", \")}",
        namespace = "shiny.telemetry"
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
        "}",
        namespace = "shiny.telemetry"
      )
      logger::log_debug(
        "secret: {substr(private$secret, start = 1, stop = 6)}...",
        namespace = "shiny.telemetry"
      )
      logger::log_debug("secret: {private$id}", namespace = "shiny.telemetry")

      logger::log_debug(
        "endpoint {private$build_url(endpoint)}", namespace = "shiny.telemetry"
      )

      request <- httr2::request(private$build_url(endpoint)) %>%
        httr2::req_headers(
          "Accept" = "application/json"
        ) %>%
        httr2::req_body_json(
          list(
            id = private$id,
            token = build_token(values, secret = private$secret),
            data = jsonlite::serializeJSON(values)
          )
        ) %>%
        httr2::req_method("POST")

      # Adds authorization
      if (!is.null(private$authorization)) {
        request <- request %>%
          httr2::req_headers(
            "Authorization" = glue::glue("Key {private$authorization}")
          )
      }

      # Perform the HTTP request
      request %>%
        httr2::req_perform()

      invisible(TRUE)
    },

    read_data = function(date_from, date_to, bucket) {
      checkmate::assert_string(bucket)
      checkmate::assert_date(date_from, null.ok = TRUE)
      checkmate::assert_date(date_to, null.ok = TRUE)

      if (is.null(date_from)) {
        date_from <- as.Date("0000-01-01")
      }

      if (is.null(date_to)) {
        date_to <- as.Date("9999-12-31")
      }

      endpoint <- dplyr::case_when(
        # API endpoints
        bucket == self$event_bucket ~ self$event_read_endpoint,
        .default = NULL
      )

      if (is.null(endpoint)) {
        rlang::abort("reading data from invalid bucket.")
      }

      request <- httr2::request(private$build_url(endpoint)) %>%
        httr2::req_url_query(
          from = date_from,
          to = date_to,
          token = build_token(
            list(from = date_from, to = date_to),
            secret = private$secret
          ),
          id = private$id
        )

      # Adds authorization
      if (!is.null(private$authorization)) {
        request <- request %>%
          httr2::req_headers(
            "Authorization" = glue::glue("Key {private$authorization}")
          )
      }

      body <- request %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      body$result %>%
        purrr::pluck(1) %>%
        jsonlite::unserializeJSON()
    }
  )
)
