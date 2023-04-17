box::use(
  glue[glue],
  shiny.telemetry[build_token, date_from_null, date_to_null],
  jsonlite[serializeJSON, unbox],
  logger[log_info, log_debug, log_error, log_warn],
  dplyr[`%>%`],
)

box::use(
  api/logic/setup[data_storage],
  api/logic/token[validate_token],
)

#' Handler to read data from data storage provider
#'
#' @param from string with starting date that can be converted to a Date class
#' @param to string with ending date that can be converted to a Date class
#' @param token string with signature of the message
#' @param id string with identification of which secret to use
#' @param FUN function to call to read data (read_event_data)
#' @export
handler <- function(from, to, token, id, FUN) {
  from <- as.Date(from)
  to <- as.Date(to)

  is_token_valid <- validate_token(
    list(from = from, to = to), token, id
  )

  if (isFALSE(is_token_valid)) {
    msg <- glue("Invalid token at {Sys.time()}")
    my_token <- build_token(list(from = from, to = to), secret = id)
    log_debug(
      "Invalid token ",
      "{{",
      "id: '{id}', ",
      "token: '{token}', ",
      "build_token: '{my_token}'",
      "}}"
    )
    return(list(status = 401, success = unbox(msg)))
  }

  if (from == date_from_null) {
    from <- NULL
  }
  if (to == date_to_null) {
    to <- NULL
  }


  list(
    status = 200,
    result = FUN(from, to) %>%
      serializeJSON()
  )
}
