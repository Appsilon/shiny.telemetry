test_that("[SQLite] DataStorage should be able to insert and read", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)

  test_common_data_storage(data_storage)
})

test_that("[SQLite] DataStorage should be able to insert and read events without details", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)
  test_common_empty_details(data_storage)
})

test_that("[SQLite] Insert and read custom fields with length > 1", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)

  test_common_len_gt_1(data_storage)
})

test_that("[SQLite] Insert and read custom fields with length > 1 on a pre-populated file", {
  db_path <- tempfile(fileext = ".sqlite")
  withr::defer(file.remove(db_path))

  data_storage <- DataStorageSQLite$new(db_path = db_path)

  test_common_len_gt_1_alt(data_storage)
})
