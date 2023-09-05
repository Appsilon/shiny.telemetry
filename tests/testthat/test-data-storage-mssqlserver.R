test_that("data-storage-mssqlserver: Can write to database via DataStorage", {
  storage_config <- list(
    user = Sys.getenv("TEST_MSSQLSERVER_USER"),
    password = Sys.getenv("TEST_MSSQLSERVER_PASSWORD"),
    hostname = Sys.getenv("TEST_MSSQLSERVER_HOSTNAME"),
    port = Sys.getenv("TEST_MSSQLSERVER_PORT"),
    dbname = Sys.getenv("TEST_MSSQLSERVER_DBNAME"),
    driver = Sys.getenv("TEST_MSSQLSERVER_DRIVER"),
    trust_server_certificate = Sys.getenv("TEST_MSSQLSERVER_TRUST_SERVER_CERTIFICATE")
  )

  skip_if_storage_config_missing(storage_config)

  storage_config$port <- as.numeric(storage_config$port)

  dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

  data_storage <- do.call(DataStorageMSSQLServer$new, storage_config)

  test_common_data_storage(data_storage, dashboard_name)
})
