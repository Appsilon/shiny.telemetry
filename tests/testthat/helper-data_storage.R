test_common <- function(data_storage) {
  require(testthat)
  withr::defer(data_storage$close())

  #
  # Necessary constants for the tests

  date_from <- Sys.Date() - 365 * 10
  date_to <- Sys.Date() + 10

  #
  # Empty results should be allowed to run smoothly and without problems

  user_data_empty <- data_storage$read_user_data(date_from, date_to)
  session_data_empty <- data_storage$read_session_data(date_from, date_to)

  expect_true(checkmate::test_tibble(user_data_empty))
  expect_true(checkmate::test_tibble(session_data_empty))

  expect_equal(NROW(user_data_empty), 0)
  expect_equal(NROW(session_data_empty), 0)

  #
  # Write and read data

  expect_error(data_storage$insert("some value"), "Must be of type 'list'")

  expect_silent({
    data_storage$insert(
      values = list(action = "logout"), bucket = data_storage$action_bucket
    )
    data_storage$insert(list(action = "click", id = "some_button_id"))
    data_storage$insert(list(action = "click", id = "some_button_id_2"))
    data_storage$insert(
      list(detail = "bla"), bucket = data_storage$session_bucket
    )
    data_storage$insert(
      list(detail = "yada"), bucket = data_storage$session_bucket
    )

    # insert with custom session and username
    data_storage$insert(
      list(
        action = "click", id = "id1", session = "s1", username = "u1"
      )
    )

    data_storage$insert(
      list(
        action = "click", id = "id1", session = "s2", username = "u1"
      )
    )
  })

  user_data <- data_storage$read_user_data(date_from, date_to)
  session_data <- data_storage$read_session_data(date_from, date_to)

  expect_true(checkmate::test_tibble(user_data))
  expect_true(checkmate::test_tibble(session_data))

  expect_equal(NROW(user_data), 5)

  user_data %>%
    dplyr::filter(.data$username == "u1") %>%
    NROW() %>%
    expect_equal(2)

  # 1 result per session
  expect_equal(NROW(session_data), 1)
}
