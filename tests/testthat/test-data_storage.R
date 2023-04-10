logger::log_threshold("ERROR", namespace = "shiny.telemetry")

test_that("Data storage initializes a dummy class", {
  data_storage <- DataStorage$new()

  date_from <- Sys.Date() - 365 * 10
  date_to <- Sys.Date() + 10

  app_name <- "test_dashboard"

  error_msg <- "Method not implemented."

  expect_error(data_storage$insert(), arg_missing_msg("app_name"))
  expect_error(data_storage$insert("dash"), arg_missing_msg("type"))

  data_storage$insert(
    app_name = app_name,
    type = "without_session"
  ) %>% expect_error(error_msg)

  data_storage$insert(
    app_name = app_name,
    type = "without_session",
    details = list(value = "132")
  ) %>% expect_error(error_msg)

  expect_error(data_storage$read_event_data(date_from, date_to), error_msg)

  data_storage$insert(
    app_name = app_name,
    session = "some_session_id",
    type = "login"
  ) %>% expect_error(error_msg)

  expect_error(data_storage$close(), error_msg)

})
