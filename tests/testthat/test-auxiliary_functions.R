test_that("Build valid SQL query", {
  con <- odbc::dbConnect(RSQLite::SQLite(), ":memory:")

  build_query_sql("table_name", .con = con) %>%
    as.character() %>%
    expect_equal("SELECT * FROM `table_name`")

  days_ago <- lubridate::today() - 10
  days_ago_double <- days_ago %>% lubridate::as_datetime() %>% as.double()

  days_future <- lubridate::today() + 15
  days_future_double <- (days_future + 1) %>% lubridate::as_datetime() %>% as.double()

  build_query_sql("table_name", days_ago, .con = con) %>%
    as.character() %>%
    expect_equal(glue::glue(
      "SELECT * FROM `table_name` WHERE time >= {days_ago_double}"
    ))

  build_query_sql("table_name", date_to = days_future, .con = con) %>%
    as.character() %>%
    expect_equal(glue::glue(
      "SELECT * FROM `table_name` WHERE time < {days_future_double}"
    ))

  build_query_sql("table_name", days_ago, days_future, .con = con) %>%
    as.character() %>%
    expect_equal(glue::glue(
      "SELECT * FROM `table_name`",
      " WHERE time >= {days_ago_double}",
      " AND time < {days_future_double}"
    ))

  build_query_sql(
    "table_name",
    as.Date("2023-04-13"),
    as.Date("2000-01-01"),
    .con = con
  ) %>%
    as.character() %>%
    expect_equal(
      glue::glue(
        "SELECT * FROM `table_name`",
        " WHERE time >= {lubridate::as_datetime('2023-04-13') %>% as.double()}",
        " AND time < {lubridate::as_datetime('2000-01-02') %>% as.double()}"
      )
    )

})

test_that("build_mongo_connection_string: Build valid string with NULL", {
  expect_equal(
    build_mongo_connection_string(
      host = "localhost",
      port = 27017,
      username = NULL,
      password = NULL,
      authdb = NULL,
      options = NULL
    ),
    "mongodb://localhost:27017/"
  )
})

test_that("build_mongo_connection_string: Build valid string with user and pass", {
  expect_equal(
    build_mongo_connection_string(
      host = "localhost",
      port = 27017,
      username = "a_user",
      password = "a_pass",
      authdb = NULL,
      options = NULL
    ),
    "mongodb://a_user:a_pass@localhost:27017/"
  )
})

test_that("build_mongo_connection_string: Build valid string with `authdb`", {
  expect_equal(
    build_mongo_connection_string(
      host = "localhost",
      port = 27017,
      username = NULL,
      password = NULL,
      authdb = "path_to_authdb",
      options = NULL
    ),
    "mongodb://localhost:27017/path_to_authdb"
  )
})

test_that("build_mongo_connection_string: Build valid string with `options`", {
  expect_equal(
    build_mongo_connection_string(
      host = "localhost",
      port = 27017,
      username = NULL,
      password = NULL,
      authdb = NULL,
      options = list("option1" = "value1", "option2" = "value2")
    ),
    "mongodb://localhost:27017/?option1=value1&option2=value2"
  )
})

test_that("build_mongo_connection_string: Build valid string with all parameters", {
  expect_equal(
    build_mongo_connection_string(
      host = "localhost",
      port = 27017,
      username = "a_user",
      password = "a_pass",
      authdb = "path_to_authdb",
      options = list("option1" = "value1", "option2" = "value2")
    ),
    "mongodb://a_user:a_pass@localhost:27017/path_to_authdb?option1=value1&option2=value2"
  )
})

describe("merge_regex generates valid regular expressions with", {
  it("* or +", {
    expect_true(grepl(merge_regex(list("[b-z]", "aa+")), "aaa"))
    expect_true(grepl(merge_regex(list("[b-z]", "a+")), "a"))
    expect_true(grepl(merge_regex(list("[b-z]", "ba*")), "ba"))
    expect_true(grepl(merge_regex(list("[b-z]", "ba*")), "b"))
  })

  it("count of characters", {
    expect_true(grepl(merge_regex(list("[b-z]", "a{2}", "1234")), "aa"))
    expect_false(grepl(merge_regex(list("[b-z]", "^a{2}$", "1234")), "aaaa"))
  })

  it("range of characters", {
    expect_true(grepl(merge_regex(list("^[b-z]+$", "a{2}", "1234")), "chrome"))
  })

  it("escape characters", {
    expect_false(grepl(merge_regex(list("a", "\\[a-z\\]")), "y"))
  })
})

testthat("merge_regex invalid regular expressions throw error", {
  expect_error(grepl(merge_regex(list("(", "[b-z]", ")")), "y"))
})
