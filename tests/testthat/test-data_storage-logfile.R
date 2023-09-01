
test_that("[LogFile] DataStorage should be able to insert and read", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)

  test_common_data_storage(data_storage)
})

test_that("[LogFile] DataStorage should be able to insert and read events without details", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)
  withr::defer(data_storage$close())

  data_storage$insert(
    app_name = "app_name",
    type = "without_session"
  ) %>% expect_silent()

  data_storage$read_event_data() %>%
    expect_silent() %>%
    NROW() %>%
    expect_equal(1)
})

test_that("[LogFile] DataStorage should be able to insert and read custom fields with length > 1", {
  log_file_path <- tempfile(fileext = ".txt")
  withr::defer(file.remove(log_file_path))

  data_storage <- DataStorageLogFile$new(log_file_path = log_file_path)
  withr::defer(data_storage$close())


  data_storage$insert(
    app_name = "app_name",
    type = "without_session"
  ) %>% expect_silent()

  data_storage$insert(
    app_name = "app_name",
    type = "click",
    details = list(id = "some_button_id_2"),
    session = "some_session_id"
  ) %>% expect_silent()

  data_storage$insert(
    app_name = "app_name",
    type = "click",
    details = list(id = "vector_selected", value = 1:10, custom = 2),
    session = "some_session_id"
  ) %>% expect_silent()

  result <- data_storage$read_event_data() %>%
    expect_silent()

  result %>%
    dplyr::filter(
      id == "vector_selected"
    ) %>%
    purrr::pluck("value") %>%
    expect_type("character")

  result %>%
    dplyr::filter(
      id == "vector_selected"
    ) %>%
    purrr::pluck("value") %>%
    unname() %>%
    expect_equal(format(paste(1:10, collapse = ", ")))
})
