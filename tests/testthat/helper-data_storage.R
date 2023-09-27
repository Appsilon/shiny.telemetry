
#' Set of common tests for different data storage providers
#' @param init_fun function to initialize data storage provider
#' @param provider_name string with name of data storage provider
#'
#' @keywords internal
test_that_common_data_storage <- function(init_fun, provider_name) {
  testthat::test_that(
    glue::glue(
      .sep = " ",
      provider_name,
      "Can write to database via DataStorage"
    ),
    {
      data_storage <- init_fun()
      dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

      test_common_data_storage(data_storage, dashboard_name)
    }
  )

  testthat::test_that(
    glue::glue(
      .sep = " ",
      provider_name,
      "Insert and read events without details"
    ),
    {
      data_storage <- init_fun()
      dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

      test_common_empty_details(data_storage, dashboard_name)
    }
  )

  testthat::test_that(
    glue::glue(
      .sep = " ",
      provider_name,
      "Insert and read custom fields with length > 1"
    ),
    {
      data_storage <- init_fun()
      dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

      test_common_len_gt_1(data_storage, dashboard_name)
    }
  )

  testthat::test_that(
    glue::glue(
      .sep = " ",
      provider_name,
      "Insert and read custom fields with length > 1 on a pre-populated file"
    ),
    {
      data_storage <- init_fun()
      dashboard_name <- paste0("dashboard-", rlang::hash(Sys.time()))

      test_common_len_gt_1_alt(data_storage, dashboard_name)
    }
  )
}

test_common_data_storage <- function(data_storage, dashboard_name = "test_dashboard") {
  require(testthat)
  withr::defer(data_storage$close())

  # Necessary constants for the tests

  date_from <- lubridate::today(tzone = "UTC") - 365 * 10
  date_to <- lubridate::today(tzone = "UTC") + 10

  # Empty results should be allowed to run smoothly and without problems

  user_data_empty <- data_storage$read_event_data(
    date_from, date_to, app_name = dashboard_name
  )

  expect_true(checkmate::test_tibble(user_data_empty))
  expect_equal(NROW(user_data_empty), 0)

  #
  # Required fields when reading data (without any rows in data storage)
  #  username, id and value are not stored directly, but within details field
  expect_in(
    c("app_name", "date", "id", "session", "time", "type", "username", "value"),
    colnames(user_data_empty)
  )

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

  user_data <- data_storage$read_event_data(
    date_from, date_to, app_name = dashboard_name
  )

  expect_true(checkmate::test_tibble(user_data))
  expect_equal(NROW(user_data), 4)

  # Empty call (no dates)
  expect_equal(NROW(data_storage$read_event_data(app_name = dashboard_name)), 4)

  #
  # Required fields when reading data
  #  username, id and value are not stored directly, but within details field
  expect_in(
    c("app_name", "date", "id", "session", "time", "type", "username", "value"),
    colnames(data_storage$read_event_data(app_name = dashboard_name))
  )

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
    lubridate::today() + 5,
    app_name = dashboard_name
  ) %>%
    NROW() %>%
    expect_equal(2)

  data_storage$read_event_data(
    lubridate::today() + 2,
    lubridate::today() + 5,
    app_name = dashboard_name
  ) %>%
    NROW() %>%
    expect_equal(1)
}

test_common_empty_details <- function(data_storage, dashboard_name = "test_dashboard") {
  require(testthat)
  withr::defer(data_storage$close())

  data_storage$insert(app_name = dashboard_name, type = "without_session")
  expect_equal(NROW(data_storage$read_event_data(app_name = dashboard_name)), 1)
}

test_common_len_gt_1 <- function(data_storage, dashboard_name = "test_dashboard") {
  require(testthat)
  withr::defer(data_storage$close())

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "vector_selected", value = 1:10, custom = 2),
    session = "some_session_id"
  )

  result <- data_storage$read_event_data(app_name = dashboard_name)

  result %>%
    purrr::pluck("value") %>%
    expect_type("character")

  result %>%
    purrr::pluck("value") %>%
    unname() %>%
    expect_equal(format(paste(1:10, collapse = ", ")))
}

test_common_len_gt_1_alt <- function(data_storage, dashboard_name = "test_dashboard") {
  require(testthat)
  withr::defer(data_storage$close())

  data_storage$insert(
    app_name = dashboard_name,
    type = "without_session"
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id"
  )

  data_storage$insert(
    app_name = dashboard_name,
    type = "click",
    details = list(id = "vector_selected", value = 1:10, custom = 2),
    session = "some_session_id"
  )

  result <- data_storage$read_event_data(app_name = dashboard_name)

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

#' Template message
#' @keywords internal
arg_missing_msg <- function(var_name) {
  glue::glue("argument \"{var_name}\" is missing, with no default")
}
