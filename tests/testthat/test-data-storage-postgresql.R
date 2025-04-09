
# Test suite common to data storages (see `helper-data_storage.R`)
withr::with_envvar(
  c("TEST_POSTGRESQL_DRIVER" = "RPostgres"),
  code = describe("PostgreSQL (RPostgres)", it_common_data_storage(init_test_postgres))
)

withr::with_envvar(
  c("TEST_POSTGRESQL_DRIVER" = "RPostgreSQL"),
  code = describe("PostgreSQL (RPostgreSQL)", it_common_data_storage(init_test_postgres))
)
