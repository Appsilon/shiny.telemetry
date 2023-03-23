test_that("log_input", {
  data_storage <- list(
    insert = function(values, bucket) {
      message(glue::glue("Writing to {bucket} value: {values$value} id: {values$id}"))
    },
    action_bucket = "user_log"
  )

  mockery::stub(
    log_input,
    "shiny::observeEvent",
    function(eventExpr, handlerExpr, priority, ignoreInit) {
      handlerExpr
    }
  )

  # Test simple usage of log_input
  expect_message(
    log_input(
      data_storage,
      list(sample = 53, sample2 = 31),
      "sample",
      matching_values = NULL,
      input_type = "text"
    ),
    "Writing to user_log value: 53 id: sample"
  )

  # Test simple usage of log_input with matching values
  expect_silent(
    log_input(
      data_storage,
      list(sample = 53, sample2 = 31),
      "sample",
      matching_values = c(52, "52"),
      input_type = "text"
    )
  )

  expect_message(
    log_input(
      data_storage,
      list(sample = 53, sample2 = 31),
      "sample",
      matching_values = 53,
      input_type = "text"
    ),
    "Writing to user_log value: 53 id: sample"
  )

  # Allow to test inputs that keep a list
  log_input(
    data_storage,
    list(sample = list(1, 2, 3), sample2 = 31),
    "sample",
    matching_values = NULL,
    input_type = "text"
  ) %>%
    expect_message("Writing to user_log value: 1 id: sample_1") %>%
    expect_message("Writing to user_log value: 2 id: sample_2") %>%
    expect_message("Writing to user_log value: 3 id: sample_3")
})
