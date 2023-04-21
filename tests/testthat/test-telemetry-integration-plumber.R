# Test that performs integration checks on how Telemetry class works with a
#  valid data storage provider.
test_that("[Plumber] Telemetry writes and reads events (integration)", {
  skip_on_cran()

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
      if (grepl("insert", url$path)) {
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

  test_common_telemetry(data_storage)
})
