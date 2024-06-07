#' Build query to read from table in DataStorageSQLite provider
#'
#' @param date_from date representing the starting day of results. Can be NULL.
#' @param date_to date representing the last day of results. Can be NULL.
#' @param bucket string with name of table
#' @param timestamp_wrapper string with function to wrap up seconds in database
#' query.
#' This is a [glue::glue_sql()] formatted string using the parameter 'seconds'.
#'
#' @return A string with SQL query.
#'
#' @noRd
#' @examples
#' con <- odbc::dbConnect(RSQLite::SQLite(), ":memory:")
#' build_query_sql("table_name", .con = con)
#' build_query_sql("table_name", Sys.Date() - 365, .con = con)
#' build_query_sql("table_name", date_to = Sys.Date() + 365, .con = con)
#' build_query_sql("table_name", Sys.Date() - 365, Sys.Date() + 365, .con = con)
#' build_query_sql("table_name", as.Date("2023-04-13"), as.Date("2000-01-01"), .con = con)
#' build_query_sql(
#'   "table_name", as.Date("2023-04-13"), as.Date("2000-01-01"),
#'   timestamp_wrapper = "to_timestamp({seconds})",
#'   .con = con
#' )
build_query_sql <- function(
    bucket,
    date_from = NULL,
    date_to = NULL,
    timestamp_wrapper = NULL,
    .con = NULL) {
  checkmate::assert_string(bucket)
  checkmate::assert_date(date_from, null.ok = TRUE)
  checkmate::assert_date(date_to, null.ok = TRUE)
  checkmate::assert_string(timestamp_wrapper, null.ok = TRUE)

  query <- list(
    .sep = " ",
    "SELECT *",
    "FROM {`bucket`}",
    ifelse(!is.null(date_from) || !is.null(date_to), "WHERE", "")
  )

  build_timestamp <- function(value) { # nolint: object_usage.
    seconds <- lubridate::as_datetime(value) %>% as.double()
    if (is.null(timestamp_wrapper)) {
      return(seconds)
    }
    # timestamp_wrapper is parsed here with `seconds` as a parameter
    glue::glue_sql(timestamp_wrapper, .con = .con)
  }

  where <- list(.sep = " AND ")
  if (!is.null(date_from)) {
    where <- c(
      where,
      glue::glue_sql("time >= ", "{build_timestamp(date_from)}", .con = .con)
    )
  }

  if (!is.null(date_to)) {
    date_to_aux <- (lubridate::as_date(date_to) + 1) %>% # nolint: object_usage.
      lubridate::as_datetime()
    where <- c(
      where,
      glue::glue_sql("time < {build_timestamp(date_to_aux)}", .con = .con)
    )
  }

  # Separate call to build WHERE clause as it has a different separator
  query <- c(query, do.call(glue::glue_sql, c(where, .con = .con)))
  trimws(do.call(glue::glue_sql, c(query, .con = .con)))
}

#' Build query to read from collection in DataStorageMongoDB provider
#'
#' @param date_from date representing the starting day of results. Can be NULL.
#' @param date_to date representing the last day of results. Can be NULL.
#'
#' @return A string or a JSON object that can be used as the query argument of
#' the `find()` method of a [mongolite::mongo()] class.
#'
#' @noRd
#' @examples
#' con <- mongolite::mongo()
#' con$find(query = build_query_mongodb())
#' con$find(query = build_query_mongodb(Sys.Date() - 365))
#' con$find(query = build_query_mongodb(date_to = Sys.Date() + 365))
#' con$find(query = build_query_mongodb(
#'   date_from = Sys.Date() - 365, date_to = Sys.Date() + 365)
#' )
#' con$find(query = build_query_mongodb(
#'   date_from = as.Date("2023-04-13"), date_to = as.Date("2000-01-01")
#' ))
build_query_mongodb <- function(date_from, date_to) {
  if (is.null(date_from) && is.null(date_to)) {
    query <- "{}"
    return(query)
  } else {
    query <- list(time = list())
  }

  if (!is.null(date_from)) {
    if (inherits(date_from, "Date")) {
      date_from <- paste0(as.character(date_from), " 00:00:00 UTC")
    }
    query$time["$gte"] <- as.integer(lubridate::as_datetime(date_from)) * 1000
  }

  if (!is.null(date_to)) {
    if (inherits(date_to, "Date")) {
      date_to <- paste0(as.character(date_to), " 23:59:59 UTC")
    }
    query$time["$lte"] <- as.integer(lubridate::as_datetime(date_to)) * 1000
  }

  jsonlite::toJSON(query, auto_unbox = TRUE)
}

#' Create the connection string for mongodb
#'
#' @noRd
#' @keywords internal
#' @examples
#' build_mongo_connection_string(
#'   "localhost",
#'   31,
#'   "user",
#'   "pass",
#'   "authdb",
#'   list("option1" = "value1", "option2" = "value2")
#' )
build_mongo_connection_string <- function(
    hostname, port, username, password, authdb, options
) {
  checkmate::assert_string(hostname)
  checkmate::assert_int(port)
  checkmate::assert_string(username, null.ok = TRUE)
  checkmate::assert_string(password, null.ok = TRUE)
  checkmate::assert_string(authdb, null.ok = TRUE)
  checkmate::assert_list(options, null.ok = TRUE)

  paste0(
    "mongodb://",
    sprintf("%s:%s@", username, password),
    hostname,
    ":",
    port,
    sprintf("/%s", authdb %||% ""),
    ifelse(
      isFALSE(is.null(options)),
      sprintf(
        "?%s",
        paste(names(options), "=", options, collapse = "&", sep = "")
      ),
      ""
    )
  )
}

#' Process a row's detail (from DB) in JSON format to a data.frame
#'
#' @param details_json string containing details a valid JSON, NULL or NA
#'
#' @return A data.frame with 1 row and a column for every property on the JSON.
#'
#' @noRd
#' @keywords internal
process_row_details <- function(details_json) {
  if (is.null(details_json) || is.na(details_json) || details_json == "") {
    return(data.frame(.empty = "true"))
  }

  # fromJSON() cannot be called with vector input, it needs to
  #  iterated one by one. It also does not allow for NULL, NA nor
  #  empty strings.
  tmp_result <- details_json %>%
    jsonlite::fromJSON() %>%
    purrr::compact()

  tmp_result <- tmp_result %>%
    purrr::map(function(.x) {
      if (checkmate::test_atomic_vector(.x, min.len = 2)) {
        return(list(.x))
      }
      if (length(.x) > 1) {
        return(jsonlite::toJSON(.x, auto_unbox = TRUE))
      }
      .x
    })

  tmp_result <- do.call(cbind, tmp_result) %>%
    as.data.frame() %>%
    # All un-nested columns have to be character type.
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      format
    ))

  # Catch for when `details` json is valid, but empty.
  if (NROW(tmp_result) == 0) {
    return(data.frame(.empty = "true"))
  }

  tmp_result
}
