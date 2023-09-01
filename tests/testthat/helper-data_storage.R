arg_missing_msg <- function(var_name) {
  glue::glue("argument \"{var_name}\" is missing, with no default")
}

test_common_data_storage <- function(data_storage, dashboard_name = "test_dashboard") {
  require(testthat)
  withr::defer(data_storage$close())

  # Necessary constants for the tests

  date_from <- lubridate::today(tzone = "UTC") - 365 * 10
  date_to <- lubridate::today(tzone = "UTC") + 10

  # Empty results should be allowed to run smoothly and without problems

  user_data_empty <- data_storage$read_event_data(date_from, date_to) %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name))

  expect_true(checkmate::test_tibble(user_data_empty))
  expect_equal(NROW(user_data_empty), 0)

  #
  # Required fields when reading data (without any rows in data storage)
  #  username, id and value are not stored directly, but within details field
  user_data_empty %>%
    colnames() %>%
    sort() %>%
    expect_equal(c(
      "app_name", "date", "id", "session", "time", "type", "username", "value"
    ))

  # Write and read data
  expect_error(data_storage$insert(), arg_missing_msg("app_name"))
  expect_error(data_storage$insert("dash"), arg_missing_msg("type"))

  data_storage$insert(
    app_name = dashboard_name,
    type = "without_session"
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "logout",
    session = "some_session_id"
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "some_button_id"),
    session = "some_session_id"
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id"
  )

  user_data <- data_storage$read_event_data(date_from, date_to) %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name))

  expect_true(checkmate::test_tibble(user_data))
  expect_equal(NROW(user_data), 4)

  # Empty call (no dates)
  data_storage$read_event_data() %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name)) %>%
    NROW() %>%
    expect_equal(4)

  #
  # Required fields when reading data
  #  username, id and value are not stored directly, but within details field
  data_storage$read_event_data() %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name)) %>%
    colnames() %>%
    sort() %>%
    expect_equal(c(
      "app_name", "date", "id", "session", "time", "type", "username", "value"
    ))

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id",
    time = lubridate::as_datetime(lubridate::today() + 5)
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id",
    time = lubridate::as_datetime(lubridate::today() + 1)
  )

  data_storage$read_event_data(
    lubridate::today() + 1,
    lubridate::today() + 5
  ) %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name)) %>%
    NROW() %>%
    expect_equal(2)

  data_storage$read_event_data(
    lubridate::today() + 2,
    lubridate::today() + 5
  ) %>%
    dplyr::filter(grepl(dashboard_name, .data$app_name)) %>%
    NROW() %>%
    expect_equal(1)
}

<<<<<<< HEAD
test_common_empty_details <- function(data_storage) {
  require(testthat)
  withr::defer(data_storage$close())

  app_name <- "test_dashboard_empty_details"

  data_storage$insert(app_name = app_name, type = "without_session")

  expect_equal(NROW(data_storage$read_event_data()), 1)
}

test_common_len_gt_1 <- function(data_storage) {
  require(testthat)
  withr::defer(data_storage$close())

  app_name <- "test_dashboard"

  data_storage$insert(
    app_name = app_name,
    type = "click",
    details = list(id = "vector_selected", value = 1:10, custom = 2),
    session = "some_session_id"
  )

  result <- data_storage$read_event_data()

  result %>%
    purrr::pluck("value") %>%
    expect_type("character")

  result %>%
    purrr::pluck("value") %>%
    unname() %>%
    expect_equal(format(paste(1:10, collapse = ", ")))
}

test_common_len_gt_1_alt <- function(data_storage) {
  require(testthat)
  withr::defer(data_storage$close())

  app_name <- "test_dashboard_len_gt_1_alt"

  data_storage$insert(
    app_name = app_name,
    type = "without_session"
  )

  data_storage$insert(
    app_name = app_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id"
  )

  data_storage$insert(
    app_name = app_name,
    type = "click",
    details = list(id = "vector_selected", value = 1:10, custom = 2),
    session = "some_session_id"
  )

  result <- data_storage$read_event_data()

  result %>%
    dplyr::filter(.data$id == "vector_selected") %>%
    purrr::pluck("value") %>%
    expect_type("character")

  result %>%
    dplyr::filter(.data$id == "vector_selected") %>%
    purrr::pluck("value") %>%
    unname() %>%
    expect_equal(format(paste(1:10, collapse = ", ")))
}

#' Skip if storage configuration is not defined
#' @param storage_config list of characters with configuration
#' @keywords internal
skip_if_storage_config_missing <- function(storage_config) {
  message_string <- "DataStorage config: Not available"

  skip_if_not(
    checkmate::test_list(storage_config, min.len = 1, types = "character"),
    message_string
  )

  skip_if_not(
    all(vapply(
      storage_config,
      function(.x) checkmate::test_string(.x, min.chars = 1), logical(1)
    )),
    message_string
  )
}
