#' Build query to read from table in DataStorageSQLite provider
#'
#' @param date_from date representing the starting day of results. Can be NULL.
#' @param date_to date representing the last day of results. Can be NULL.
#' @param bucket string with name of table
#' @param timestamp_wrapper string with function to wrap up seconds in database
#' query. This is a glue::glue() formatted string using the parameter 'seconds'.
#'
#' @return A string with SQL query.
#'
#' @noRd
#' @examples
#' build_query_sql("table_name")
#' build_query_sql("table_name", Sys.Date() - 365)
#' build_query_sql("table_name", date_to = Sys.Date() + 365)
#' build_query_sql("table_name", Sys.Date() - 365, Sys.Date() + 365)
#' build_query_sql("table_name", as.Date("2023-04-13"), as.Date("2000-01-01"))
#' build_query_sql(
#'   "table_name", as.Date("2023-04-13"), as.Date("2000-01-01"), timestamp_wrapper = "to_timestamp({seconds})"
#' )
build_query_sql <- function(
  bucket, date_from = NULL, date_to = NULL, timestamp_wrapper = NULL
) {
  checkmate::assert_date(date_from, null.ok = TRUE)
  checkmate::assert_date(date_to, null.ok = TRUE)

  query <- list(
    .sep = " ",
    "SELECT *",
    "FROM {bucket}",
    ifelse(!is.null(date_from) || !is.null(date_to), "WHERE", "")
  )

  build_timestamp <- function(value) {  # nolint: object_usage_linter
    seconds <- lubridate::as_datetime(value) %>% as.double()
    if (is.null(timestamp_wrapper)) {
      return(seconds)
    }
    return(glue::glue(timestamp_wrapper))
  }

  where <- list(.sep = " AND ")
  if (!is.null(date_from)) {
    where <- c(
      where,
      glue::glue(
        "time >= ",
        "{build_timestamp(date_from)}"
      )
    )
  }

  if (!is.null(date_to)) {
    date_to_aux <- (lubridate::as_date(date_to) + 1) %>% # nolint: object_usage_linter
      lubridate::as_datetime()
    where <- c(
      where,
      glue::glue("time < {build_timestamp(date_to_aux)}")
    )
  }

  query <- c(query, do.call(glue::glue, where))
  do.call(glue::glue, query) %>% stringr::str_trim()
}
