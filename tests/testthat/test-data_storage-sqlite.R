test_that("[SQLite] DataStorage should be able to insert and read", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)

  test_common_data_storage(data_storage)
})
