# Test that performs integration checks on how Telemetry class works with a
#  valid data storage provider.
test_that("[LogFile] Telemetry writes and reads events (integration)", {
  event_file_path <- tempfile(fileext = ".txt")

  withr::defer({
    if (file.exists(event_file_path)) file.remove(event_file_path)
  })

  data_storage <- DataStorageLogFile$new(log_file_path = event_file_path)

  test_common_telemetry(data_storage)
})
