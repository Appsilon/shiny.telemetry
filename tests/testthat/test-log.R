test_that("log_input", {
  data_storage <- list(
    insert = function(values, bucket) {
      if (!is.null(values$value)) {
        message(glue::glue("Writing to {bucket} value: {values$value} id: {values$id}"))
      } else {
        message(glue::glue("Writing to {bucket} value change with id: {values$id}"))
      }
    },
    action_bucket = "user_log"
  )

  telemetry <- Telemetry$new(data_storage = data_storage)

  mockery::stub(
    telemetry$log_input,
    "shiny::observeEvent",
    function(eventExpr, handlerExpr, ...) { # nolint object_name_linter
      handlerExpr
    }
  )

  # Test simple usage of log_input
  expect_message(
    telemetry$log_input(
      input = list(sample = 53, sample2 = 31),
      input_id = "sample",
      matching_values = NULL,
      track_value = TRUE,
      input_type = "text"
    ),
    "Writing to user_log value: 53 id: sample"
  )

  # Test simple usage of log_input with matching values
  expect_silent(
    telemetry$log_input(
      list(sample = 53, sample2 = 31),
      "sample",
      track_value = TRUE,
      matching_values = c(52, "52"),
      input_type = "text"
    )
  )

  expect_message(
    telemetry$log_input(
      list(sample = 53, sample2 = 36),
      "sample",
      track_value = TRUE,
      matching_values = 53,
      input_type = "text"
    ),
    "Writing to user_log value: 53 id: sample"
  )

  # Allow to test inputs that keep a list
  telemetry$log_input(
    list(sample = 23, sample2 = 31),
    "sample",
    matching_values = NULL,
    input_type = "text"
  ) %>%
    expect_message("Writing to user_log value change with id: sample")

  telemetry$log_input(
    list(sample = 1:10, sample2 = 31),
    "sample",
    matching_values = NULL,
    input_type = "text"
  ) %>%
    expect_message("Writing to user_log value change with id: sample")

  # Allow to test inputs that keep a list
  telemetry$log_input(
    list(sample = list(1, 2, 3), sample2 = 31),
    "sample",
    track_value = TRUE,
    matching_values = NULL,
    input_type = "text"
  ) %>%
    expect_message("Writing to user_log value: 1 id: sample_1") %>%
    expect_message("Writing to user_log value: 2 id: sample_2") %>%
    expect_message("Writing to user_log value: 3 id: sample_3")
})
