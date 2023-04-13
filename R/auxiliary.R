#' Build query to read from table in DataStorageSQLite provider
#'
#' @param date_from date representing the starting day of results. Can be NULL.
#' @param date_to date representing the last day of results. Can be NULL.
#' @param bucket string with name of table
#'
#' @return string with SQL query
#'
#' @examples
#'
#' shiny.telemetry:::build_query_sql("table_name")
#' shiny.telemetry:::build_query_sql("table_name", Sys.Date() - 365)
#' shiny.telemetry:::build_query_sql("table_name", date_to = Sys.Date() + 365)
#' shiny.telemetry:::build_query_sql("table_name", Sys.Date() - 365, Sys.Date() + 365)
#' shiny.telemetry:::build_query_sql("table_name", as.Date("2023-04-13"), as.Date("2000-01-01"))
build_query_sql <- function(bucket, date_from = NULL, date_to = NULL) {
  checkmate::assert_date(date_from, null.ok = TRUE)
  checkmate::assert_date(date_to, null.ok = TRUE)

  query <- list(
    .sep = " ",
    "SELECT *",
    "FROM {bucket}",
    ifelse(!is.null(date_from) || !is.null(date_to), "WHERE", "")
  )

  where <- list(.sep = " AND ")
  if (!is.null(date_from)) {
    where <- c(where, "time >= {as.double(lubridate::as_datetime(date_from))}")
  }

  if (!is.null(date_to)) {
    where <- c(where, "time <= {as.double(lubridate::as_datetime(date_to))}")
  }
  query <- c(query, do.call(glue::glue, where))
  do.call(glue::glue, query) %>% stringr::str_trim()
}
