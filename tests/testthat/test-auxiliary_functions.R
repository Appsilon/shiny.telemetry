describe("build_query_sql: builds valid SQL query", {
  con <- odbc::dbConnect(RSQLite::SQLite(), ":memory:")

  days_ago <- lubridate::today() - 10
  days_ago_double <- days_ago %>% lubridate::as_datetime() %>% as.double()

  days_future <- lubridate::today() + 15
  days_future_double <- (days_future + 1) %>% lubridate::as_datetime() %>% as.double()

  it("with no dates", {
    build_query_sql("table_name", .con = con) %>%
      as.character() %>%
      expect_equal("SELECT * FROM `table_name`")
  })

  it("with date 'from'", {
    build_query_sql("table_name", days_ago, .con = con) %>%
      as.character() %>%
      expect_equal(glue::glue(
        "SELECT * FROM `table_name` WHERE time >= {days_ago_double}"
      ))
  })

  it("with date 'to'", {
    build_query_sql("table_name", date_to = days_future, .con = con) %>%
      as.character() %>%
      expect_equal(glue::glue(
        "SELECT * FROM `table_name` WHERE time < {days_future_double}"
      ))
  })

  it("with date 'from' and 'to'", {
    build_query_sql("table_name", days_ago, days_future, .con = con) %>%
      as.character() %>%
      expect_equal(glue::glue(
        "SELECT * FROM `table_name`",
        " WHERE time >= {days_ago_double}",
        " AND time < {days_future_double}"
      ))
  })
})

describe("build_mongo_connection_string: builds valid string", {
  it("with NULL", {
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

  it("with user and pass", {
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

  it("with `authdb`", {
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

  it("with `options`", {
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

  it("with all parameters", {
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
})
