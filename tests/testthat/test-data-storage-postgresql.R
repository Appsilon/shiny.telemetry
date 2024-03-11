
# Test suite common to data storages (see `helper-data_storage.R`)
withr::with_envvar(
  c("TEST_POSTGRESQL_DRIVER" = "RPostgres"),
  code = test_that_common_data_storage(init_test_postgres, "PostgreSQL")
)

withr::with_envvar(
  c("TEST_POSTGRESQL_DRIVER" = "RPostgreSQL"),
  code = test_that_common_data_storage(init_test_postgres, "PostgreSQL")
)
