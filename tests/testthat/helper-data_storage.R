arg_missing_msg <- function(var_name) {
  glue::glue("argument \"{var_name}\" is missing, with no default")
}

test_common <- function(data_storage) {
  require(testthat)
  withr::defer(data_storage$close())

  # Necessary constants for the tests

  date_from <- Sys.Date() - 365 * 10
  date_to <- Sys.Date() + 10

  app_name <- "test_dashboard"

  # Empty results should be allowed to run smoothly and without problems

  user_data_empty <- data_storage$read_event_data(date_from, date_to)

  expect_true(checkmate::test_tibble(user_data_empty))
  expect_equal(NROW(user_data_empty), 0)

  # Write and read data
  expect_error(data_storage$insert(), arg_missing_msg("app_name"))
  expect_error(data_storage$insert("dash"), arg_missing_msg("type"))

  data_storage$insert(
    app_name = app_name,
    type = "without_session"
  ) %>% expect_silent()

  data_storage$insert(
    app_name = app_name,
    type = "logout",
    session = "some_session_id"
  ) %>% expect_silent()

  data_storage$insert(
    app_name = app_name,
    type = "click",
    details = list(id = "some_button_id"),
    session = "some_session_id"
  ) %>% expect_silent()

  data_storage$insert(
    app_name = app_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id"
  ) %>% expect_silent()

  user_data <- data_storage$read_event_data(date_from, date_to)

  expect_true(checkmate::test_tibble(user_data))
  expect_equal(NROW(user_data), 4)
}
