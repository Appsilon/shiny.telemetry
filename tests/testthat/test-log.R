test_that("log_input", {
  data_storage <- list(
    insert = function(values, bucket) {
      message(glue::glue(
        "Writing to {bucket} value: ",
        "{jsonlite::toJSON(values, auto_unbox = TRUE)}"
      ))
    },
    action_bucket = "user_log"
  )

  mocked_observe_event <- function(eventExpr, handlerExpr, ...) { # nolint object_name_linter
    shiny::isolate(handlerExpr)
  }

  telemetry <- Telemetry$new(data_storage = data_storage)

  # Mock ShinySession (with required class added to pass assertions)
  session <- shiny::MockShinySession$new()
  class(session) <- c("ShinySession", class(session))
  session$setInputs(sample = 53, sample2 = 31)

  testthat::local_mocked_bindings(
    observeEvent = mocked_observe_event,
    .package = "shiny"
  )

  ShinySessionMock <- R6::R6Class( # nolint object_name_linter
    classname = "ShinySession",
    public = list(
      input = list(),
      initialize = function(input) {
        self$input <- input
      }
    )
  )

  # Test simple usage of log_input
  session$setInputs(sample = 53, sample2 = 31)
  expect_message(
    telemetry$log_input(
      input_id = "sample",
      matching_values = NULL,
      track_value = TRUE,
      input_type = "text",
      session = session
    ),
    "Writing to user_log value: .*\"value\":53.*"
  )

  session$setInputs(sample = 63, sample2 = 41)
  # Test simple usage of log_input with matching values
  expect_silent(
    telemetry$log_input(
      "sample",
      track_value = TRUE,
      matching_values = c(62, "62"),
      input_type = "text",
      session = session
    )
  )

  session$setInputs(sample = 73, sample2 = 51)
  expect_message(
    telemetry$log_input(
      "sample",
      track_value = TRUE,
      matching_values = 73,
      input_type = "text",
      session = session
    ),
    "Writing to user_log value: .*\"value\":73.*"
  )

  session$setInputs(sample = 83, sample2 = 61)
  # Allow to test inputs that keep a list
  telemetry$log_input(
    "sample",
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>%
    expect_message("Writing to user_log value: .*\"id\":\"sample\".*")

  session$setInputs(sample = 1:10, sample2 = 31)
  telemetry$log_input(
    "sample",
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>%
    expect_message("Writing to user_log value: .*\"id\":\"sample\".*")

  # Allow to test inputs that keep a list
  session$setInputs(sample = list(1, 2, 3), sample2 = 31)
  telemetry$log_input(
    "sample",
    track_value = TRUE,
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>%
    expect_message("Writing to user_log value: .*\"id\":\"sample_1\".*") %>%
    expect_message("Writing to user_log value: .*\"id\":\"sample_2\".*") %>%
    expect_message("Writing to user_log value: .*\"id\":\"sample_3\".*")

  withr::deferred_run()
})
