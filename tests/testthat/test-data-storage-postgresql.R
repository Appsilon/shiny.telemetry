
test_that("data-storage-postgresql: Can write to database via DataStorage", {
  storage_config <- list(
    user = Sys.getenv("TEST_POSTGRESQL_USER"),
    password = Sys.getenv("TEST_POSTGRESQL_PASSWORD"),
    port = Sys.getenv("TEST_POSTGRESQL_PORT"),
    dbname = Sys.getenv("TEST_POSTGRESQL_DBNAME"),
    hostname = Sys.getenv("TEST_POSTGRESQL_HOSTNAME")
  )

  skip_if_storage_config_missing(storage_config)

  storage_config$port <- as.numeric(storage_config$port)
  dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

  data_storage <- do.call(DataStoragePostgreSQL$new, storage_config)

  test_common_data_storage(data_storage, dashboard_name)
})
