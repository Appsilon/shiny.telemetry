test_that("get_active_users", {
  log_data <- tibble::tibble(
    time = strptime("2023-03-20 15:00:00", format = "%Y-%m-%d %H:%M:%S") + c(10, 20, 30, 3601),
    username = c("a", "b", "a", "c"),
    date = rep(Sys.Date(), 4)
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
  log_data <- tibble::tibble(
    action = c("login", "login", "logout", "something else", "other"),
    date = rep(Sys.Date(), 5)
  )

  expect_equal(
    get_actions_per_day(log_data) %>% dplyr::pull(.data$action),
    2
  )
})

# Change from tidyr::gather to tidyr::pivot_longer
# for function: get_per_day_plot_data()
test_that("gather pivot", {
  dat <- tibble::tibble(
    statistic = 1:10,
    value = 11:20,
    date = rep(Sys.Date(), 10)
  )

  gather_df <- dat %>%
    tidyr::gather(key = "statistic", value = "value", -"date") %>%
    dplyr::arrange(.data$date, .data$statistic, .data$value)

  pivot_df <- dat %>%
    tidyr::pivot_longer(c(-"date"), names_to = "statistic", values_to = "value") %>%
    dplyr::arrange(.data$date, .data$statistic, .data$value)

  expect_equal(
    pivot_df,
    gather_df
  )
})
