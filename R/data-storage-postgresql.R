#' Data storage class with PostgreSQL provider
#'
#' @description
#' Implementation of the [`DataStorage`] R6 class to PostgreSQL backend using a
#' unified API for read/write operations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_storage <- DataStoragePostgreSQL$new(user = "postgres", password = "mysecretpassword")
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
DataStoragePostgreSQL <- R6::R6Class( # nolint object_name.
  classname = "DataStoragePostgreSQL",
  inherit = DataStorageSQLFamily,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param username string with a PostgreSQL username.
    #' @param password string with the password for the username.
    #' @param hostname string with hostname of PostgreSQL instance.
    #' @param port numeric value with the port number of PostgreSQL instance.
    #' @param dbname string with the name of the database in the PostgreSQL instance.
    #' @param driver string, to select PostgreSQL driver among `c("RPostgreSQL", "RPostgres")`.

    initialize = function(
      username = NULL,
      password = NULL,
      hostname = "127.0.0.1",
      port = 5432,
      dbname = "shiny_telemetry",
      driver = "RPostgreSQL"
    ) {
      checkmate::assert_string(password)
      checkmate::assert_string(username)
      checkmate::assert_string(hostname)
      checkmate::assert_int(port)
      checkmate::assert_string(dbname)
      checkmate::assert_choice(driver, choices = c("RPostgreSQL", "RPostgres"))

      logger::log_debug(
        "Parameters for PostgreSQL:\n",
        "  *          username: {username}\n",
        "  * password (sha256): {digest::digest(password, algo = 'sha256')}\n",
        "  *     hostname:port: {hostname}:{port}\n",
        "  *           db name: {dbname}\n",
        namespace = "shiny.telemetry"
      )
      # private method to select the driver
      selected_driver <- private$select_driver(driver)
      private$connect(username, password, hostname, port, dbname, selected_driver)
      private$initialize_connection()
    }

  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,
    timestamp_wrapper = "to_timestamp({seconds})",
    select_driver = function(driver) {
      if (driver == "RPostgres") {
        return(RPostgres::Postgres())
      } else { # Default to RPostgreSQL if input is not recognized
        return(RPostgreSQL::PostgreSQL())
      }
    },
    # Private methods
    connect = function(user, password, hostname, port, dbname, driver) {
      # Initialize connection with database
      private$db_con <- odbc::dbConnect(
        driver,
        user = user,
        password = password,
        dbname = dbname,
        host = hostname,
        port = port
      )
    }
  )
)
