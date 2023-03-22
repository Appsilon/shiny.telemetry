test_common <- function(data_storage) {
  require(testthat)

  expect_true(checkmate::test_string(data_storage$username))

  expect_true(checkmate::test_string(data_storage$session_id))
  expect_true(uuid::is.UUID(uuid::UUIDparse(data_storage$session_id)))

  expect_error(data_storage$insert("some value"), "Must be of type 'list'")

  expect_silent({
    data_storage$insert(values = list(action = "logout"), "user_log")
    data_storage$insert(list(action = "click", id = "some_button_id"))
    data_storage$insert(list(action = "click", id = "some_button_id_2"))
    data_storage$insert(
      list(detail = "bla"), bucket = "session_details", add_username = FALSE
    )
    data_storage$insert(
      list(detail = "yada"), bucket = "session_details", add_username = FALSE
    )
  })

  date_from <- Sys.Date() - 365 * 10
  date_to <- Sys.Date() + 10

  user_data <- data_storage$read_user_data(date_from, date_to)
  session_data <- data_storage$read_session_data(date_from, date_to)

  expect_true(checkmate::test_tibble(user_data))
  expect_true(checkmate::test_tibble(session_data))

  expect_equal(NROW(user_data), 3)

  # 1 result per session
  expect_equal(NROW(session_data), 1)
}
