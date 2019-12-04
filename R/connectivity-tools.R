#' @export
get_users_data <- function(users_credentials_db_config) {
  db_config <- users_credentials_db_config
  names(db_config) <- stats_db_config_variables
  db_config_list <- get_config(db_config)
  connection <- connect_to_db(db_config_list)
  users_data <- retrieve_users(connection)
  odbc::dbDisconnect(connection)
  users_data
}

#' @export
source_renviron <- function(path, variable_name) {
  readRenviron(path)
  Sys.getenv(variable_name)
}

#' @export
list_stats_targets <- function(path) {
  variable_names <- target_env_variables
  target_db_configs <- list.files(path, full.names = TRUE, include.dirs = TRUE) %>%
    purrr::map(source_renviron, variable_name = variable_names)
  target_ids <- target_db_configs %>%
    purrr::map_chr("value")
  target_labels <- target_db_configs %>%
    purrr::map_chr("label")
  names(target_db_configs) <- target_ids
  list(
    target_db_configs = target_db_configs,
    target_ids = target_ids,
    target_labels = target_labels
  )
}

#' Connect with sql database
#' @description Makes connection to database based on passed config data
#' @param config_list data.frame with database config parameters
#' @export
connect_to_db <- function(db_credentials) {
  drv <- DBI::dbDriver(db_credentials$DB_DRIVER)
  odbc::dbConnect(
    drv,
    dbname = db_credentials$DB_NAME, host = db_credentials$DB_HOST, user = db_credentials$DB_USERNAME,
    password = db_credentials$DB_PASSWORD, port = db_credentials$DB_PORT
  )
}

#' Register connection to database
#' @description This connects to specified sqlite database (or create new one when it doesn't exist).
#' Should be used outside server and ui function.
#' @param db_connection Connection to database object.
#' @param username Username of logged user.
#' @param session_id Logged user session id (if NULL, id is randomly generated).
#' @return Return list with two objects:
#' session_id (useful to specify user session after app is opened),
#' db_connection the same object that was passed as db_connection parameter.
#'
#' It also sets empty 'user_log' table with:
#' \code{CREATE TABLE user_log(time TIMESTAMP, session TEXT, username TEXT, action TEXT, id TEXT, value TEXT)}
#' and 'session_details' table with:
#' \code{CREATE TABLE session_details(time TIMESTAMP, session TEXT, detail TEXT)} inside DB.
#' @export
initialize_connection <- function(db_connection, username, session_id = NULL) {
  initialize_custom_connection(
    db_connection,
    username,
    session_id,
    table_schemes = list(
      user_log = c(time = "TIMESTAMP", session = "TEXT", username = "TEXT",
                   action = "TEXT", id = "TEXT", value = "TEXT"),
      session_details = c(time = "TIMESTAMP", session = "TEXT", detail = "TEXT"))
  )
}

#' @param table_schemes Specific table schemes that should be created or connected with inside 'db_connection'.
#' Each list object specifies separate table and should be of the form:
#' table_name = c(col_1_name = col_1_type, ...)
#' @rdname initialize_connection
#' @export
initialize_custom_connection <- function(db_connection, username, session_id, table_schemes) {
  if (is.null(session_id)) {
    session_id <- generate_session_id()
  }

  table_names <-  names(table_schemes)

  purrr::walk2(table_names, table_schemes,
               function(table_name, table_scheme) create_table_from_schema(db_connection, table_name, table_scheme)
  )

  list(
    username = username,
    session_id = session_id,
    db_connection = db_connection
  )
}

create_table_from_schema <- function(db_connection, table_name, table_scheme) {
  if (!(table_name %in% odbc::dbListTables(db_connection))) {
    create_table_query <- odbc::sqlCreateTable(con = db_connection,
                                               table = table_name,
                                               fields = table_scheme,
                                               row.names = FALSE)
    res <- odbc::dbSendQuery(conn = db_connection, create_table_query)
    odbc::dbClearResult(res)
  }

  return(invisible(db_connection))
}

create_user_log_table <- function(db_connection) {
  table_name <- "user_log"
  table_scheme <- c(
    time = "TIMESTAMP", session = "TEXT", username = "TEXT", action = "TEXT", id = "TEXT", value = "TEXT")

  create_table_from_schema(db_connection, table_name, table_scheme)
}

create_users_table <- function(db_connection) {
  table_name <- USERS_TABLE_NAME
  table_scheme <- c(
    username = "TEXT", password = "TEXT", roles = "TEXT", info = "TEXT")

  create_table_from_schema(db_connection, table_name, table_scheme)
}

#' Prepare sql database config object
#' @description Converts config database file into data.frame
#' Configuration file should contain following parameters (each parameter on separate line
#' with value separated with \code{=} sign):
#' In case of sqlite file:
#' DB_NAME - path to database file (absolute or relative to application directory), DRV - "sqlite"
#' In case of external PostgreSQL database: DB_NAME, DB_HOST, DB_PORT, DB_USER, DB_PASSWORD, DRV - respective
#' parameters of RPostgreSQL.
#' @param config_variables Named vector with environmental variables concerning db connection.
#' @return Return data.frame with passed parameters and its valies.
#' @export
get_config <- function(config_variables) {
  config <- na.omit(data.frame(key = names(config_variables), value = config_variables, stringsAsFactors = FALSE))
  split(config$value, config$key)
}

make_connection_parameters <- function(config_list) {
  list(dbname = config_list$DB_NAME,
       host = config_list$DB_HOST,
       port = config_list$DB_PORT,
       user = config_list$DB_USER,
       password = config_list$DB_PASSWORD)
}

generate_session_id <- function() {
  paste(c(sample(c(letters, LETTERS, 0:9), 10), format(Sys.time(), "%d%m%H%M%S")), collapse = "")
}

parse_val <- function(val) {
  jsonlite::fromJSON(ifelse(is.null(val), "\"\"", val))
}
