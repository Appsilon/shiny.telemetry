box::use(
  glue[glue],
  shiny.telemetry[build_token],
  jsonlite[serializeJSON, unbox],
  logger[log_info, log_debug, log_error, log_warn],
)

box::use(
  app/logic/setup[data_storage],
  app/logic/token[validate_token],
)

#' @export
handler <- function(from, to, token, id, FUN) {
  is_token_valid <- validate_token(
    list(from = as.Date(from), to = as.Date(to)), token, id
  )

  log_debug("Token is valid: {is_token_valid}")

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

  list(
    status = 200,
    result = FUN(as.Date(from), as.Date(to)) |>
      serializeJSON()
  )
}
