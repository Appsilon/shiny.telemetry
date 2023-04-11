box::use(
  purrr[pluck],
  digest[digest],
  checkmate[test_string],
  rlang[abort],
  config,
  shiny.telemetry[build_token],
)

box::use(
  api/logic/setup[session_secrets],
)

#' Get secret from list of session secrets
#'
#' @param id string that identifies a given secret
#'
#' @export
get_secret <- function(id = NULL) {
  if (is.null(id)) {
    return(NULL)
  }

  pluck(session_secrets, id)
}

#' @export
validate_token <- function(values, token, id) {
  if (is.null(session_secrets)) {
    return(TRUE)
  }

  if (is.null(token) || !checkmate::test_string(token)) {
    return(FALSE)
  }

  return(token == build_token(values, secret = get_secret(id)))
}

if (is.null(box::name())) {
  box::use(api/logic/`__tests__`)
}
