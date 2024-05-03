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
#' data_storage <- DataStorageMariaDB$new(
#'   url = "mongodb://localhost",
#'   db = "test",
#'   collection = "test",
#'   options = mongolite::ssl_options()
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
    #' @param url address of the mongodb server in mongo connection string
    #' [URI format](https://www.mongodb.com/docs/manual/reference/connection-string/).
    #' Default is "mongodb://localhost".
    #' @param db name of database. Default is "shiny_telemetry".
    #' @param collection name of collection. Default is "event_log".
    #' @param options additional connection options such as SSL keys/certs (optional).

    initialize = function(
      url = "mongodb://localhost",
      db = "shiny_telemetry",
      collection = "event_log",
      options = mongolite::ssl_options()
    ) {
      private$connect(url, db, collection, options)
      private$db_name <- db
      private$collection_name <- collection
    }
  ),
  active = list(

    #' @field event_bucket string that identifies the bucket to store user
    #' related and action data
    event_bucket = function() private$collection_name
  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,
    db_name = NULL,
    collection_name = NULL,

    # Private methods
    connect = function(url, db, collection, options) {
      # Initialize connection with database
      private$db_con <- mongolite::mongo(
        url = url,
        db = db,
        collection = collection,
        options = options
      )
    },

    close_connection = function() {
      private$db_con$disconnect()
    },

    write = function(values, bucket) {
      checkmate::assert_choice(bucket, choices = c(self$event_bucket))
      checkmate::assert_list(values)

      values$details <- jsonlite::fromJSON(values$details)

      private$db_con$insert(values, auto_unbox = TRUE, POSIXt = "epoch")
    },

    read_data = function(date_from, date_to, bucket) {
      checkmate::assert_choice(bucket, c(self$event_bucket))

      event_data <- private$db_con$find(
        query = build_query_mongodb(date_from, date_to),
        fields = '{"_id": false}'
      )

      if (nrow(event_data) > 0) {
        event_data %>%
          dplyr::tibble() %>%
          tidyr::unnest(cols = "details") %>%
          dplyr::mutate(time = lubridate::as_datetime(as.integer(time / 1000)))
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
