test_that("Build valid SQL query", {
  build_query_sql("table_name") %>% expect_equal("SELECT * FROM table_name")

  days_ago <- lubridate::today() - 10
  days_ago_double <- days_ago %>% lubridate::as_datetime() %>% as.double()

  days_future <- lubridate::today() + 15
  days_future_double <- days_future %>% lubridate::as_datetime() %>% as.double()

  build_query_sql("table_name", days_ago) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name WHERE time >= {days_ago_double}"
    ))

  build_query_sql("table_name", date_to = days_future) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name WHERE time <= {days_future_double}"
    ))

  build_query_sql("table_name", days_ago, days_future) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name",
      " WHERE time >= {days_ago_double}",
      " AND time <= {days_future_double}"
    ))

  build_query_sql(
    "table_name",
    as.Date("2023-04-13"),
    as.Date("2000-01-01")
  ) %>%
    expect_equal(
      glue::glue(
        "SELECT * FROM table_name",
        " WHERE time >= {lubridate::as_datetime('2023-04-13') %>% as.double()}",
        " AND time <= {lubridate::as_datetime('2000-01-01') %>% as.double()}"
      )
    )

})
