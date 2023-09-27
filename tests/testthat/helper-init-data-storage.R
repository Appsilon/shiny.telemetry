
#' Skip if storage configuration is not defined
#' @param storage_config list of characters with configuration
#' @param provider_name string with name of data storage provider
#' @keywords internal
skip_if_storage_config_missing <- function(storage_config, provider_name = NULL) {
  provider_string <- ""
  if (!is.null(provider_name)) {
    provider_string <- glue::glue(" `{provider_name}`")
  }
  message_string <- glue::glue(
    "DataStorage",
    "{provider_string}",
    " config: Not available"
  )


  testthat::skip_if_not(
    checkmate::test_list(storage_config, min.len = 1, types = "character"),
    message_string
  )

  testthat::skip_if_not(
    all(vapply(
      storage_config,
      function(.x) checkmate::test_string(.x, min.chars = 1), logical(1)
    )),
    message_string
  )
}

init_test_postgres <- function(.local_envir = parent.frame()) {
  storage_config <- list(
    user = Sys.getenv("TEST_POSTGRESQL_USER"),
    password = Sys.getenv("TEST_POSTGRESQL_PASSWORD"),
    port = Sys.getenv("TEST_POSTGRESQL_PORT"),
    dbname = Sys.getenv("TEST_POSTGRESQL_DBNAME"),
    hostname = Sys.getenv("TEST_POSTGRESQL_HOSTNAME")
  )
  testthat::skip_on_cran()
  skip_if_storage_config_missing(storage_config, "PostgreSQL")

  storage_config$port <- as.numeric(storage_config$port)

  do.call(DataStoragePostgreSQL$new, storage_config)
}

init_test_mariadb <- function(.local_envir = parent.frame()) {
  storage_config <- list(
    user = Sys.getenv("TEST_MARIADB_USER"),
    password = Sys.getenv("TEST_MARIADB_PASSWORD"),
    port = Sys.getenv("TEST_MARIADB_PORT"),
    dbname = Sys.getenv("TEST_MARIADB_DBNAME"),
    hostname = Sys.getenv("TEST_MARIADB_HOSTNAME")
  )

  testthat::skip_on_cran()
  skip_if_storage_config_missing(storage_config, "MariaDB/MySQL")

  storage_config$port <- as.numeric(storage_config$port)

  do.call(DataStorageMariaDB$new, storage_config)
}

init_test_mssql <- function(.local_envir = parent.frame()) {
  storage_config <- list(
    user = Sys.getenv("TEST_MSSQLSERVER_USER"),
    password = Sys.getenv("TEST_MSSQLSERVER_PASSWORD"),
    hostname = Sys.getenv("TEST_MSSQLSERVER_HOSTNAME"),
    port = Sys.getenv("TEST_MSSQLSERVER_PORT"),
    dbname = Sys.getenv("TEST_MSSQLSERVER_DBNAME"),
    driver = Sys.getenv("TEST_MSSQLSERVER_DRIVER"),
    trust_server_certificate = Sys.getenv("TEST_MSSQLSERVER_TRUST_SERVER_CERTIFICATE")
  )

  testthat::skip_on_cran()
  skip_if_storage_config_missing(storage_config, "MSSQLServer")

  storage_config$port <- as.numeric(storage_config$port)

  do.call(DataStorageMSSQLServer$new, storage_config)
}

init_test_logfile <- function(.local_envir = parent.frame()) {
  log_file_path <- withr::local_file("logfile.txt", .local_envir = .local_envir)

  DataStorageLogFile$new(log_file_path = log_file_path)
}

init_test_sqlite <- function(.local_envir = parent.frame()) {
  db_path <- withr::local_file("telemtry.sqlite", .local_envir = .local_envir)

  DataStorageSQLite$new(db_path = db_path)
}
