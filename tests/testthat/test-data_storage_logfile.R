logger::log_threshold("ERROR", namespace = "shiny.telemetry")

test_that("DataStorageLogFile shoulb be able to insert and read", {
  event_file_path <- tempfile(fileext = ".txt")

  withr::defer(file.remove(event_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = event_file_path)

  test_common(data_storage)
})
