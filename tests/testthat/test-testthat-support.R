test_that("skip_if_storage_config_missing empty config", {
  expect_condition(
    skip_if_storage_config_missing(list()),
    "DataStorage config: Not available",
    class = "skip"
  )
})

test_that("skip_if_storage_config_missing config with wrong data types", {
  expect_condition(
    skip_if_storage_config_missing(list(SOME_CONFIG = 2)),
    "DataStorage config: Not available",
    class = "skip"
  )
})

test_that("skip_if_storage_config_missing config with mixed data types (some wrong)", {
  expect_condition(
    skip_if_storage_config_missing(list(SOME_CONFIG = 2, OTHER = "SOMETHING")),
    "DataStorage config: Not available",
    class = "skip"
  )
})

test_that("skip_if_storage_config_missing config with wrong data types", {
  expect_condition(
    skip_if_storage_config_missing(list(SOME_CONFIG = NULL)),
    "DataStorage config: Not available",
    class = "skip"
  )

#' Skip if storage configuration is not defined
#' @param storage_config list of characters with configuration
#' @keywords internal
skip_if_storage_config_missing <- function(storage_config) {
  message_string <- "DataStorage config: Not available"

  skip_if_not(
    checkmate::test_list(storage_config, min.len = 1, types = "character"),
    message_string
  )

  skip_if_not(
    all(vapply(
      storage_config,
      function(.x) checkmate::test_string(.x, min.chars = 1), logical(1)
    )),
    message_string
  )
}

  expect_condition(
    skip_if_storage_config_missing(list(SOME_CONFIG = "NULL", OTHER = NULL)),
    "DataStorage config: Not available",
    class = "skip"
  )
})

test_that("skip_if_storage_config_missing config with empty definition", {
  expect_condition(
    skip_if_storage_config_missing(list(SOME_CONFIG = "")),
    "DataStorage config: Not available",
    class = "skip"
  )
})

test_that("skip_if_storage_config_missing config with empty argument", {
  expect_error(
    skip_if_storage_config_missing(),
    "is missing"
  )
})

test_that("skip_if_storage_config_missing config with valid configurations", {
  expect_failure(
    expect_condition(
      skip_if_storage_config_missing(list(SOME_CONFIG = "A_VALUE")),
      "DataStorage config: Not available",
      class = "skip"
    )
  )

  storage_config <- list(
    SOME_CONFIG = "A_VALUE",
    SOME_CONFIG2 = "A_VALUE_2",
    SOME_CONFIG3 = "A_VALUE_3",
    SOME_CONFIG4 = "A_VALUE_4",
    SOME_CONFIG5 = "2",
    SOME_CONFIG6 = "....",
    SOME_CONFIG7 = "_sdas_DASads",
    SOME_CONFIG8 = "Lorem ipsum",
    SOME_CONFIG9 = "dolor sit amet, consectetur adipiscing",
    SOME_CONFIG10 = "elit, sed do eiusmod tempor",
    SOME_CONFIG11 = "Duis aute
    irure",
    SOME_CONFIG12 = "122332",
    SOME_CONFIG13 = "2131 asdda s1234312"
  )

  expect_failure(
    expect_condition(
      skip_if_storage_config_missing(storage_config),
      "DataStorage config: Not available",
      class = "skip"
    )
  )
})

