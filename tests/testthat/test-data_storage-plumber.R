logger::log_threshold(logger::FATAL)
logger::log_threshold(logger::FATAL, namespace = "shiny.telemetry")

test_that("Plumber API works", {
  db_path <- tempfile(pattern = "events", fileext = ".sqlite")

  old_env <- list(
    plumber_secret = Sys.getenv("PLUMBER_SECRET"),
    secret_tokens = Sys.getenv("SECRET_TOKENS"),
    force_sql = Sys.getenv("FORCE_SQLITE_AND_PATH")
  )

  #
  #
  # Setup environment variables

  Sys.setenv(PLUMBER_SECRET = "")
  Sys.setenv(SECRET_TOKENS = "")
  Sys.setenv(FORCE_SQLITE_AND_PATH = db_path)

  #
  #
  # Cleanup operations

  withr::defer(file.remove(db_path))
  withr::defer(options(box.path = getwd()))
  withr::defer({
    Sys.setenv(FORCE_SQLITE_AND_PATH = old_env$force_sql)
    Sys.setenv(PLUMBER_SECRET = old_env$plumber_secret)
    Sys.setenv(SECRET_TOKENS = old_env$secret_tokens)
  })
  withr::defer({
    loaded_mods <- loadNamespace("box")$loaded_mods
    rm(list = ls(loaded_mods), envir = loaded_mods)
  })

  # Setup API
  options(box.path = file.path(getwd(), "..", "..", "plumber_rest_api"))

  loaded_mods <- loadNamespace("box")$loaded_mods
  rm(list = ls(loaded_mods), envir = loaded_mods)
  api <- plumber::pr(file = file.path(options()$box.path, "api/main.R"))

  # Read user_log information (should be empty)
  req <- mock_request(from = as.Date("2013-04-13"), to = as.Date("2025-04-13"))
  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)
  result$result %>% jsonlite::unserializeJSON() %>% NROW() %>% expect_equal(0)

  dat_user_log <- list(
    time = as.character(Sys.time()),
    dashboard = "Plumber test",
    version = "v0.0.0",
    session = "some_session",
    username = "some_username",
    action = "input",
    id = "some_id",
    value = "new_value"
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

  old_env <- list(
    plumber_secret = Sys.getenv("PLUMBER_SECRET"),
    secret_tokens = Sys.getenv("SECRET_TOKENS"),
    force_sql = Sys.getenv("FORCE_SQLITE_AND_PATH")
  )

  #
  #
  # Setup environment variables

  Sys.setenv(PLUMBER_SECRET = "12345")
  Sys.setenv(SECRET_TOKENS = "12345")
  Sys.setenv(FORCE_SQLITE_AND_PATH = db_path)

  #
  #
  # Cleanup operations

  withr::defer(file.remove(db_path))
  withr::defer(options(box.path = getwd()))
  withr::defer({
    Sys.setenv(FORCE_SQLITE_AND_PATH = old_env$force_sql)
    Sys.setenv(PLUMBER_SECRET = old_env$plumber_secret)
    Sys.setenv(SECRET_TOKENS = old_env$secret_tokens)
  })

  # Setup API
  options(box.path = file.path(getwd(), "..", "..", "plumber_rest_api"))

  loaded_mods <- loadNamespace("box")$loaded_mods
  rm(list = ls(loaded_mods), envir = loaded_mods)
  api <- plumber::pr(file = file.path(options()$box.path, "api/main.R"), envir = new.env())

  # Read user_log information (should be empty)
  req <- mock_request(
    from = as.Date("2013-04-13"),
    to = as.Date("2025-04-13"),
    .secret = Sys.getenv("PLUMBER_SECRET")
  )

  result <- api$routes$read_user_data$exec(req, res = list(status = 2))

  expect_equal(result$status, 200)
  result$result %>% jsonlite::unserializeJSON() %>% NROW() %>% expect_equal(0)

  data_user_log <- list(
    time = as.character(Sys.time()),
    dashboard = "Plumber test with token",
    version = "v0.0.0",
    session = "some_session",
    username = "some_username",
    action = "input",
    id = "some_id",
    value = "new_value"
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
