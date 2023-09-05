#' Data storage class with PostgreSQL provider
#'
#' @description
#' Implementation of the DataStorage R6 class to PostgreSQL backend using a
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
DataStorageMSSQLServer <- R6::R6Class( # nolint object_name_linter
  classname = "DataStorageMSSQLServer",
  inherit = DataStorageSQLFamily,
  #
  # Public
  public = list(

    #' @description
    #' Initialize the data storage class
    #' @param username string with a MS SQL Server username.
    #' @param password string with the password for the username.
    #' @param hostname string with hostname of the MS SQL Server instance.
    #' @param port numeric value with the port number of MS SQL Server instance.
    #' @param dbname string with the name of the database in the MS SQL Server
    #' @param driver string with the name of the ODBC driver class for MS SQL.
    #' @param trust_server_certificate string with "NO" or "YES", setting 
    #' whether or not to trust the server's certificate implicitly.
    #' instance.

    initialize = function(
      username = NULL,
      password = NULL,
      hostname = "127.0.0.1",
      port = 1433,
      dbname = "shiny_telemetry",
      driver = "ODBC Driver 17 for SQL Server",
      trust_server_certificate = "NO"
    ) {
      checkmate::assert_string(password)
      checkmate::assert_string(username)
      checkmate::assert_string(hostname)
      checkmate::assert_int(port)
      checkmate::assert_string(dbname)
      checkmate::assert_string(driver)
      checkmate::assert_string(trust_server_certificate)

      logger::log_debug(
        "Parameters for MS SQL Server:\n",
        "  *                 username: {username}\n",
        "  *        password (sha256): {digest::digest(password, algo = 'sha256')}\n",
        "  *            hostname:port: {hostname}:{port}\n",
        "  *                  db name: {dbname}\n",
        "  *                   driver: {driver}\n",
        "  * trust_server_certificate: {trust_server_certificate}\n",
        namespace = "shiny.telemetry"
      )
      private$connect(username, password, hostname, port, dbname, driver,
                      trust_server_certificate)
      private$initialize_connection()
    }

  ),
  #
  # Private
  private = list(
    # Private Fields
    db_con = NULL,
    timestamp_wrapper = "DATEADD(s, {seconds}, '1970-01-01')",

    # Private methods

    connect = function(user, password, hostname, port, dbname, driver, 
                       trust_server_certificate) {
      # Initialize connection with database
      private$db_con <- odbc::dbConnect(
        drv = odbc::odbc(),
        uid = user,
        pwd = password,
        database = dbname,
        driver = driver,
        server = hostname,
        port = port,
        TrustServerCertificate = trust_server_certificate
        
      )
    },
    
    initialize_connection = function() {
      table_schemes <- list(
        c(
          time = "datetime",
          app_name = "TEXT",
          session = "TEXT",
          type = "TEXT",
          details = "TEXT"
        )
      )
      
      table_names <- c(self$event_bucket)
      names(table_schemes) <- table_names
      
      purrr::walk2(
        table_names, table_schemes, private$create_table_from_schema
      )
      NULL
    }
  )
)
