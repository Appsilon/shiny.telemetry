#' Data storage class with MariaDB / MySQL provider
#'
#' @description
#' Implementation of the DataStorage R6 class to MariaDB backend using a
#' unified API for read/write operations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_storage <- DataStorageMariaDB$new(user = "mariadb", password = "mysecretpassword")
#'
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
DataStorageMariaDB <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageMariaDB",
  inherit = DataStorageSQLFamily,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param db_path string with path to sqlfile

    initialize = function(
    username = NULL,
    password = NULL,
    host = "127.0.0.1",
    port = 3306,
    db_name = "shiny_telemetry"
    ) {
      super$initialize()
      checkmate::assert_string(password)
      checkmate::assert_string(username)
      checkmate::assert_string(host)
      checkmate::assert_int(port)
      checkmate::assert_string(db_name)

      logger::log_debug(
        "Parameters for MariaDB:\n",
        "  *          username: {username}\n",
        "  * password (sha256): {digest::digest(password, algo = 'sha256')}\n",
        "  *         host:port: {host}:{port}\n",
        "  *           db name: {db_name}\n",
        namespace = "shiny.telemetry"
      )
      private$connect(username, password, host, port, db_name)
      private$initialize_connection()
    }

  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,
    timestamp_wrapper = "FROM_UNIXTIME",

    # Private methods

    connect = function(user, password, host, port, db_name) {
      # Initialize connection with database
      private$db_con <- odbc::dbConnect(
        RMariaDB::MariaDB(),
        user = user,
        password = password,
        dbname = db_name,
        host = host,
        port = port
      )
    }
  )
)

