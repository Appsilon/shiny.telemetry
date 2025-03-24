test_that("Telemetry tests with mock data_storage layer", {
  data_storage <- list(
    insert = function(
      app_name, type, session = NULL, details = NULL, time = NULL
    ) {
      message(glue::glue(
        "Writing type={type} value: ",
        "{jsonlite::toJSON(details, auto_unbox = TRUE)}"
      ))
    },
    event_bucket = "event_log"
  )

  telemetry <- Telemetry$new(data_storage = data_storage)

  # Mock ShinySession (with required class added to pass assertions)
  session <- shiny::MockShinySession$new()
  class(session) <- c("ShinySession", class(session))

  # Inputs are changed via session to best mimick that behaviour
  session$setInputs(sample = 53, sample2 = 31)

  # 'log_input' uses 'observeEvent' internally, thus the function needs to be
  # mocked. This cannot be done with 'mockery::stub()' as this function
  # cannot scope the private methods of a class.
  # `testthat::local_mocked_bindings` allows for it.
  testthat::local_mocked_bindings(
    observeEvent = function(
      eventExpr, handlerExpr, ... # nolint object_name.
    ) {
      shiny::isolate(handlerExpr)
    },
    .package = "shiny"
  )

  #
  # Test login and logout (last one shouldn't produce anything)

  telemetry$log_login(
    username = "ben",
    session = session
  ) %>% expect_message("Writing type=login value: .*\"username\":\"ben\".*")

  telemetry$log_logout(
    session = session
  ) %>% expect_silent()

  #
  # Test simple usage of log_input
  session$setInputs(sample = 53, sample2 = 31)

  telemetry$log_input(
    input_id = "sample",
    matching_values = NULL,
    track_value = TRUE,
    input_type = "text",
    session = session
  ) %>% expect_message("Writing type=input value: .*\"value\":53.*")

  #
  # Test simple usage of log_input with matching values that don't match
  session$setInputs(sample = 63, sample2 = 41)

  telemetry$log_input(
    "sample",
    track_value = TRUE,
    matching_values = c(62, "62"),
    input_type = "text",
    session = session
  ) %>% expect_silent()

  #
  # Test simple usage of log_input with matching values
  session$setInputs(sample = 73, sample2 = 51)

  telemetry$log_input(
    "sample",
    track_value = TRUE,
    matching_values = 73,
    input_type = "text",
    session = session
  ) %>% expect_message("Writing type=input value: .*\"value\":73.*")

  #
  # Test simple usage of log_input without tracking values
  session$setInputs(sample = 83, sample2 = 61)
  telemetry$log_input(
    "sample",
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>% expect_message("Writing type=input value: .*\"id\":\"sample\".*")

  #
  # Test simple usage of log_input without tracking values
  # (where value is not atomic)
  session$setInputs(sample = 1:10, sample2 = 31)
  telemetry$log_input(
    "sample",
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>% expect_message("Writing type=input value: .*\"id\":\"sample\".*")

  #
  # Test simple usage of log_input (where value is not atomic)
  session$setInputs(sample = list(1, 2, 3), sample2 = 31)
  telemetry$log_input(
    "sample",
    track_value = TRUE,
    matching_values = NULL,
    input_type = "text",
    session = session
  ) %>%
    expect_message("Writing type=input value: .*\"id\":\"sample_1\".*") %>%
    expect_message("Writing type=input value: .*\"id\":\"sample_2\".*") %>%
    expect_message("Writing type=input value: .*\"id\":\"sample_3\".*")

  #
  # Test simple usage of log_input
  session$setInputs(uisidebar = "tab1")
  telemetry$log_navigation(
    input_id = "uisidebar",
    session = session
  ) %>%
    expect_message("Writing type=navigation value: .*\"value\":\"tab1\".*")

  telemetry$log_navigation_manual(
    navigation_id = "sample",
    value = "tab2",
    session = session
  ) %>%
    expect_message("Writing type=navigation value: .*\"value\":\"tab2\".*")

  # Manual call to revert mock_binding of 'observeEvent'
  withr::deferred_run()
})

describe("excluded_inputs_regex invalid regular expressions", {
  data_storage <- list(
    insert = function(
    app_name, type, session = NULL, details = NULL, time = NULL
    ) {
      message(glue::glue(
        "Writing type={type} value: ",
        "{jsonlite::toJSON(details, auto_unbox = TRUE)}"
      ))
    },
    event_bucket = "event_log"
  )

  telemetry <- Telemetry$new(data_storage = data_storage)

  session <- shiny::MockShinySession$new()
  class(session) <- c("ShinySession", class(session))

  it("throws error with invalid expression", {
    expect_error(
      telemetry$log_all_inputs(FALSE, excluded_inputs_regex = c("(", "[b-z]", ")"), session = session),
      "Regular expression is not valid"
    ) %>%
      expect_warning("pattern compilation error")
  })

  it("throws error with invalid expression", {
    expect_no_error(
      telemetry$log_all_inputs(FALSE, excluded_inputs_regex = c("(a)", "[b-z]", "(b)"), session = session)
    )
  })
})

