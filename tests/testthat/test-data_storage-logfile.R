
test_that("DataStorageLogFile should be able to insert and read", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)

  test_common(data_storage)
})
