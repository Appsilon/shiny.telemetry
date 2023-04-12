logger::log_threshold("ERROR", namespace = "shiny.telemetry")

test_that("Data storage initializes a dummy class", {
  data_storage <- DataStorage$new()

  date_1 <- Sys.Date() - 365 * 10
  date_2 <- Sys.Date()

  error_msg <- "Method not implemented."

  expect_error(data_storage$insert(list(value = "some value")), error_msg)
  expect_error(data_storage$read_user_data(date_1, date_2), error_msg)
  expect_error(data_storage$read_session_data("some value"), error_msg)
  expect_error(data_storage$insert(list(value = "some value")), error_msg)
  expect_error(data_storage$close(), error_msg)

})
