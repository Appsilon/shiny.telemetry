test_common <- function(data_storage) {
  expect_true(checkmate::test_string(data_storage$username))

  expect_true(checkmate::test_string(data_storage$session_id))
  expect_true(uuid::is.UUID(uuid::UUIDparse(data_storage$session_id)))

  expect_error(data_storage$insert("some value"), "Must be of type 'list'")

  data_storage$insert(values = list(action = "logout"), "user_log") %>%
    expect_silent()

  data_storage$insert(list(action = "click", id = "some_button_id")) %>%
    expect_silent()

  data_storage$insert(list(action = "click", id = "some_button_id_2")) %>%
    expect_silent()

  data_storage$insert(list(detail = "bla"), bucket = "session_details") %>%
    expect_silent()

  data_storage$insert(list(detail = "yada"), bucket = "session_details") %>%
    expect_silent()

  date_1 <- Sys.Date() - 365 * 10
  date_2 <- Sys.Date()

  user_data <- data_storage$read_user_data(date_1, date_2)
  session_data <- data_storage$read_session_data(date_1, date_2)

  expect_true(checkmate::test_tibble(user_data))
  expect_true(checkmate::test_tibble(session_data))

  expect_equal(NROW(user_data), 3)

  # 1 result per session
  expect_equal(NROW(session_data), 1)
}

logger::log_threshold("ERROR")

test_that("Data storage initializes a dummy class", {
  data_storage <- DataStorage$new("sample_user_name")

  date_1 <- Sys.Date() - 365 * 10
  date_2 <- Sys.Date()

  error_msg <- "Method not implemented."

  expect_true(checkmate::test_string(data_storage$username))

  expect_true(checkmate::test_string(data_storage$session_id))
  expect_true(uuid::is.UUID(uuid::UUIDparse(data_storage$session_id)))

  expect_error(data_storage$insert("some value"), error_msg)
  expect_error(data_storage$read_user_data(date_1, date_2), error_msg)
  expect_error(data_storage$read_session_data("some value"), error_msg)
  expect_error(data_storage$insert("some value"), error_msg)
  expect_error(data_storage$close(), error_msg)

})

test_that("SQL Data storage inserts / reads", {
  db_path <- tempfile(fileext = ".sqlite")

  data_storage <- DataStorageRSQLite$new("sample_user_name", db_path = db_path)

  test_common(data_storage)

  file.remove(db_path)
})
