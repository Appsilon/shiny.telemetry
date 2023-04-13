test_that("Build valid SQL query", {
  build_query_sql("table_name") %>% expect_equal("SELECT * FROM table_name")

  build_query_sql("table_name", Sys.Date() - 365) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name WHERE date(time) >= '{Sys.Date() - 365}'"
    ))

  build_query_sql("table_name", date_to = Sys.Date() + 365) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name WHERE date(time) <= '{Sys.Date() + 365}'"
    ))

  build_query_sql("table_name", Sys.Date() - 365, Sys.Date() + 365) %>%
    expect_equal(glue::glue(
      "SELECT * FROM table_name",
      " WHERE date(time) >= '{Sys.Date() - 365}'",
      " AND date(time) <= '{Sys.Date() + 365}'"
    ))

  build_query_sql("table_name", as.Date("2023-04-13"), as.Date("2000-01-01")) %>%
    expect_equal(
      glue::glue(
        "SELECT * FROM table_name",
        " WHERE date(time) >= '2023-04-13' AND date(time) <= '2000-01-01'"
      )
    )

})
