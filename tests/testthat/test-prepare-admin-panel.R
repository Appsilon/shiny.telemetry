test_that("get_active_users", {
  log_data <- dplyr::tibble(
    time = strptime("2023-03-20 15:00:00", format = "%Y-%m-%d %H:%M:%S") + c(10, 20, 30, 3601),
    username = c("a", "b", "a", "c"),
    date = Sys.Date()
  )

  result <- get_active_users(log_data)

  # 2 different hours
  expect_equal(NROW(result), 2)

  # 2 userts on hour 3pm / 15h
  expect_equal(
    (result %>% dplyr::pull(.data$users))[1],
    2
  )
})

test_that("get_actions_per_day", {
  log_data <- dplyr::tibble(
    type = c("login", "login", "logout", "something else", "other"),
    date = rep(Sys.Date(), 5)
  )

  expect_equal(
    get_actions_per_day(log_data) %>% dplyr::pull(.data$type),
    2
  )
})

test_that("get_per_day_plot_data", {
  date_initial <- Sys.Date()

  base <- data.frame(date = date_initial)

  per_day <- dplyr::tribble(
    ~date,            ~anonymous, ~users, ~sessions, ~time, ~type,
    date_initial + 0,          1,      3,         4,     0,        5,
    date_initial + 1,          1,     31,        41,     8,       99
  )

  per_day_plot_data <- dplyr::tribble(
    ~date,            ~statistic,                     ~value, ~id, ~index,
    date_initial + 0, "logged users (unique)",             3,   1, "users",
    date_initial + 0, "anonymous users (unique)",          1,   1, "anonymous",
    date_initial + 0, "total opened sessions",             4,   1, "sessions",
    date_initial + 0, "avg session time (hours)",          0,   2, "time",
    date_initial + 0, "total navigations and inputs",      5,   3, "type",
    #
    date_initial + 1, "logged users (unique)",            31,   1, "users",
    date_initial + 1, "anonymous users (unique)",          1,   1, "anonymous",
    date_initial + 1, "total opened sessions",            41,   1, "sessions",
    date_initial + 1, "avg session time (hours)",          8,   2, "time",
    date_initial + 1, "total navigations and inputs",     99,   3, "type",
    #
    date_initial + 2, "logged users (unique)",             0,   1, "users",
    date_initial + 2, "anonymous users (unique)",          0,   1, "anonymous",
    date_initial + 2, "total opened sessions",             0,   1, "sessions",
    date_initial + 2, "avg session time (hours)",          0,   2, "time",
    date_initial + 2, "total navigations and inputs",      0,   3, "type",
  )

  expect_equal(
    get_per_day_plot_data(base, per_day) %>%
      dplyr::select(-"color") %>%
      dplyr::arrange(dplyr::across(all_of(c("date", "index")))) %>%
      dplyr::select(sort(colnames(per_day_plot_data))),

    per_day_plot_data %>%
      dplyr::filter(date == date_initial) %>%
      dplyr::arrange(dplyr::across(all_of(c("date", "index")))) %>%
      dplyr::select(sort(colnames(per_day_plot_data)))
  )

  # 2 days
  base2 <- dplyr::bind_rows(base, base + 1, base + 2)

  expect_equal(
    get_per_day_plot_data(base2, per_day) %>%
      dplyr::select(-"color") %>%
      dplyr::arrange(dplyr::across(all_of(c("date", "index")))) %>%
      dplyr::select(sort(colnames(per_day_plot_data))),
    per_day_plot_data %>%
      dplyr::arrange(dplyr::across(all_of(c("date", "index")))) %>%
      dplyr::select(sort(colnames(per_day_plot_data))),
    ignore_attr = TRUE
  )
})
