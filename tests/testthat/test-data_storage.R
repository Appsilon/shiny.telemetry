logger::log_threshold("ERROR")

test_that("Data storage initializes a dummy class", {
  data_storage <- DataStorage$new("sample_user_name")

  date_1 <- Sys.Date() - 365 * 10
  date_2 <- Sys.Date()

  error_msg <- "Method not implemented."

  expect_true(checkmate::test_string(data_storage$username))

  expect_true(checkmate::test_string(data_storage$session_id))

  expect_error(data_storage$insert(list(value = "some value")), error_msg)
  expect_error(data_storage$read_user_data(date_1, date_2), error_msg)
  expect_error(data_storage$read_session_data("some value"), error_msg)
  expect_error(data_storage$insert(list(value = "some value")), error_msg)
  expect_error(data_storage$close(), error_msg)

})

test_that("SQL Data storage inserts / reads", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageRSQLite$new("sample_user_name", db_path = db_path)

  test_common(data_storage)
})

test_that("DataStorageLogFile shoulb be able to insert and read", {
  log_file_path <- tempfile(fileext = ".json")
  session_file_path <- tempfile(fileext = ".json")
  withr::defer(file.remove(log_file_path))
  withr::defer(file.remove(session_file_path))

  data_storage <-
    DataStorageLogFile$new(
      "sample_user_name",
      log_file_path = log_file_path,
      session_file_path = session_file_path
    )

  test_common(data_storage)
})
