
test_that("[LogFile] DataStorage should be able to insert and read", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)

  test_common_data_storage(data_storage)
})

test_that("[LogFile] DataStorage should be able to insert and read events without details", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)
  test_common_empty_details(data_storage)
})

test_that("[LogFile] Insert and read custom fields with length > 1", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)

  test_common_len_gt_1(data_storage)
})

test_that("[LogFile] Insert and read custom fields with length > 1 on a pre-populated file", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)

  test_common_len_gt_1_alt(data_storage)
})
