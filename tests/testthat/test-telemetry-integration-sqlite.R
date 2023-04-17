# Test that performs integration checks on how Telemetry class works with a
#  valid data storage provider.
test_that("[SQLite] Telemetry writes and reads events (integration)", {
  sqlite_file_path <- tempfile(fileext = ".sqlite")
  withr::defer({
    if (file.exists(sqlite_file_path)) file.remove(sqlite_file_path)
  })
  data_storage <- DataStorageSQLite$new(db_path = sqlite_file_path)

  test_common_telemetry(data_storage)
})
