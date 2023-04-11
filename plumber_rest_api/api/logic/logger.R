box::use(
  logger[
    ld = log_debug, li = log_info, lw = log_warn, le = log_error,
    lf = log_fatal,
  ],
)

#' @export
log_debug <- function(...) ld(..., namespace = "shiny.telemetry.plumber")

#' @export
log_info <- function(...) li(..., namespace = "shiny.telemetry.plumber")

#' @export
log_warn <- function(...) lw(..., namespace = "shiny.telemetry.plumber")

#' @export
log_error <- function(...) le(..., namespace = "shiny.telemetry.plumber")

#' @export
log_fatal <- function(...) lf(..., namespace = "shiny.telemetry.plumber")
