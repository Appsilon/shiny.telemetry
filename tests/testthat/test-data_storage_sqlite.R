logger::log_threshold("ERROR", namespace = "shiny.telemetry")

test_that("SQL Data storage inserts / reads", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)

  test_common(data_storage)
})
