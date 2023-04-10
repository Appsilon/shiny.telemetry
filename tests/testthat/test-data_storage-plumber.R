logger::log_threshold(logger::FATAL)
logger::log_threshold(logger::FATAL, namespace = "shiny.telemetry")

Sys.setenv(R_CONFIG_ACTIVE = "test")

test_that("DataStoragePlumber should be able to insert and read", {
  db_path <- tempfile(pattern = "events", fileext = ".sqlite")

  old_env <- capture_evironment_variables(
    "PLUMBER_SECRET", "SECRET_TOKENS", "FORCE_SQLITE_AND_PATH"
  ) # helper function

  # Setup environment variables
  restore_evironment_variables(list(
    PLUMBER_SECRET = "12345",
    SECRET_TOKENS = "12345 6789",
    FORCE_SQLITE_AND_PATH = db_path
  )) # helper function

  withr::defer(file.remove(db_path))
  withr::defer(options(box.path = getwd()))
  withr::defer(restore_evironment_variables(old_env)) # helper function
  withr::defer(reset_box_cache()) # helper function

  # Initialize Plumber Data Storage
  data_storage <- DataStoragePlumber$new(
    hostname = "127.0.0.1",
    path = NULL,
    port = 8087,
    protocol = "http",
    secret = Sys.getenv("PLUMBER_SECRET")
  )

  # Setup API box path
  options(box.path = file.path(getwd(), "..", "..", "plumber_rest_api"))

  reset_box_cache() # helper function
  api <- plumber::pr(file = file.path(options()$box.path, "api/main.R"))

  local_mocked_bindings(
    req_perform = function(req, path, ...) {
      url <- httr2::url_parse(req$url)

      endpoint <- gsub("^/", "", url$path)
      request <- list(args = url$query)
      if (grepl("user_log|session_details", url$path)) {
        request <- list(args = req$body$data)
      }
      result <- api$routes[[endpoint]]$exec(request, res = list(status = 2))
      response <- httr2::response(
        status_code = result$status,
        url = req$url,
        headers = c(
          "Content-Type: application/json", "Transfer-Encoding: chunked"
        ),
        body = result
      )
      response
    },
    resp_body_json = function(resp) {
      resp$body
    },
    .package = "httr2"
  )

  test_common(data_storage)
})

test_that("Plumber API works", {
  db_path <- tempfile(pattern = "events", fileext = ".sqlite")

  old_env <- capture_evironment_variables(
    "PLUMBER_SECRET", "SECRET_TOKENS", "FORCE_SQLITE_AND_PATH"
  ) # helper function

  # Setup environment variables

  restore_evironment_variables(list(
    PLUMBER_SECRET = "", SECRET_TOKENS = "", FORCE_SQLITE_AND_PATH = db_path
  )) # helper function

  # Cleanup operations

  withr::defer(file.remove(db_path))
  withr::defer(options(box.path = getwd()))
  withr::defer(restore_evironment_variables(old_env)) # helper function
  withr::defer(reset_box_cache()) # helper function

  # Setup API
  options(box.path = file.path(getwd(), "..", "..", "plumber_rest_api"))

  reset_box_cache()
  api <- plumber::pr(file = file.path(options()$box.path, "api/main.R"))

  # Read user_log information (should be empty)
  req <- mock_request(from = (Sys.Date() - 365), to = (Sys.Date() + 365))
  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)
  result$result %>% jsonlite::unserializeJSON() %>% NROW() %>% expect_equal(0)

  dat_user_log <- list(
    time = as.character(Sys.time()),
    app_name = "Plumber test",
    session = "some_session",
    type = "input",
    details = list(id = "some_id", value = "new_value"),
  ) %>% jsonlite::serializeJSON()

  req_user_log <- mock_request(data = dat_user_log)

  api$routes$user_log$exec(req_user_log, res = list(status = 2)) %>%
    purrr::pluck("status") %>%
    expect_equal(200)

  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)

  result$result %>%
    jsonlite::unserializeJSON() %>%
    NROW() %>%
    expect_equal(1)

  withr::deferred_run()
})


test_that("Plumber API token only accepts valid messages", {
  db_path <- tempfile(pattern = "events", fileext = ".sqlite")

  old_env <- capture_evironment_variables(
    "PLUMBER_SECRET", "SECRET_TOKENS", "FORCE_SQLITE_AND_PATH"
  ) # helper function

  # Setup environment variables

  restore_evironment_variables(list(
    PLUMBER_SECRET = "12345",
    SECRET_TOKENS = "12345 6789",
    FORCE_SQLITE_AND_PATH = db_path
  )) # helper function

  # Cleanup operations

  withr::defer(file.remove(db_path))
  withr::defer(options(box.path = getwd()))
  withr::defer(restore_evironment_variables(old_env)) # helper function
  withr::defer(reset_box_cache()) # helper function

  # Setup API
  options(box.path = file.path(getwd(), "..", "..", "plumber_rest_api"))

  reset_box_cache()
  api <- plumber::pr(
    file = file.path(options()$box.path, "api/main.R"), envir = new.env()
  )

  # Read user_log information (should be empty)
  req <- mock_request(
    from = Sys.Date() - 365,
    to = Sys.Date() + 365,
    .secret = Sys.getenv("PLUMBER_SECRET")
  )

  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)
  result$result %>% jsonlite::unserializeJSON() %>% NROW() %>% expect_equal(0)

  data_user_log <- list(
    time = as.character(Sys.time()),
    app_name = "Plumber test with token",
    session = "some_session",
    action = "input",
    details = list(id = "some_id", value = "new_value")
  )

  req_user_log <- mock_request(
    data = data_user_log,
    .secret = Sys.getenv("PLUMBER_SECRET"),
    .serialize_data = TRUE
  )

  api$routes$user_log$exec(req_user_log, res = list(status = 2)) %>%
    purrr::pluck("status") %>%
    expect_equal(200)

  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)

  result$result %>%
    jsonlite::unserializeJSON() %>%
    NROW() %>%
    expect_equal(1)

  #
  #
  # Wrong token

  req_user_log <- mock_request(
    data = data_user_log,
    .secret = "a_different_token",
    .serialize_data = TRUE
  )

  api$routes$user_log$exec(req_user_log, res = list(status = 2)) %>%
    purrr::pluck("status") %>%
    expect_equal(401)

  withr::deferred_run()
})
