test_common_telemetry <- function(data_storage) {
  require(testthat)

  telemetry <- Telemetry$new(data_storage = data_storage)

  testthat::local_mocked_bindings(
    observeEvent =  function(
    eventExpr, handlerExpr, ... # nolint object_name.
    ) {
      tryCatch(
        shiny::isolate(handlerExpr),
        error = function(err) {
          stop(err)
        }
      )
    },
    onSessionEnded = function(fun, session = session) {
      tryCatch(
        fun,
        error = function(err) {
          stop(err)
        }
      )
    },
    .package = "shiny"
  )

  date_from <- Sys.Date() - 365 * 10
  date_to <- Sys.Date() + 10

  session <- shiny::MockShinySession$new()
  class(session) <- c(class(session), "ShinySession")
  session$setInputs(
    sample_a = 53,
    sample_b = 54,
    sample_c = 55,
    sample_d = 56,
    sample_e = list(1, 2, 3),
    browser_version = "Chrome 108",
    sample_button = "zz"
  )

  telemetry$log_browser_version(session = session)

  telemetry$log_click(id = "manual_click", session = session)

  telemetry$log_login(session = session)
  telemetry$log_login("ben", session = session)
  telemetry$log_logout(session = session)

  telemetry$log_button(input_id = "sample_button", session = session)

  telemetry$log_button(
    input_id = "sample_button",
    track_value = TRUE,
    session = session
  )

  telemetry$log_input(
    input_id = "sample_a",
    matching_values = NULL,
    track_value = FALSE,
    input_type = "text",
    session = session
  )

  telemetry$log_input(
    input_id = "sample_b",
    matching_values = NULL,
    track_value = TRUE,
    input_type = "text",
    session = session
  )

  telemetry$log_input(
    "sample_c",
    track_value = TRUE,
    matching_values = c(52, "52"),
    input_type = "text",
    session = session
  )

  telemetry$log_input(
    "sample_d",
    track_value = TRUE,
    matching_values = 56,
    input_type = "text",
    session = session
  )

  telemetry$log_input(
    "sample_e",
    track_value = TRUE,
    matching_values = NULL,
    input_type = "text",
    session = session
  )

  results <- data_storage$read_event_data(date_from, date_to)

  expect_equal(NROW(results), 12)

  results %>%
    dplyr::filter(.data$id == "sample_a") %>%
    purrr::pluck("value") %>%
    expect_equal(NA_character_)

  results %>%
    dplyr::filter(.data$id == "sample_b") %>%
    purrr::pluck("value") %>%
    expect_equal(as.character(54))

  results %>%
    dplyr::filter(.data$id == "sample_c") %>%
    NROW() %>%
    expect_equal(0)

  results %>%
    dplyr::filter(.data$id == "sample_d") %>%
    purrr::pluck("value") %>%
    expect_equal(as.character(56))

  results %>%
    dplyr::filter(grepl("sample_e_", .data$id)) %>%
    NROW() %>%
    expect_equal(3)

  results %>%
    dplyr::filter(grepl("sample_button", .data$id)) %>%
    NROW() %>%
    expect_equal(2)

  results %>%
    dplyr::filter(.data$id == "sample_button") %>%
    purrr::pluck("value") %>%
    expect_equal(c(NA_character_, "zz"))

  results %>%
    dplyr::filter(.data$type == "browser") %>%
    purrr::pluck("value") %>%
    expect_equal("Chrome 108")


  results %>%
    dplyr::filter(.data$type == "login") %>%
    NROW() %>%
    expect_equal(2)

  results %>%
    dplyr::filter(.data$type == "login", is.na(.data$username)) %>%
    NROW() %>%
    expect_equal(1)

  results %>%
    dplyr::filter(.data$type == "login", .data$username == "ben") %>%
    NROW() %>%
    expect_equal(1)

}
