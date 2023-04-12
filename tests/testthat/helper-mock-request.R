mock_request <- function(..., .secret = "", .serialize_data = FALSE) {
  args <- list(...)
  req <- list(
    args = args
  )
  token <- ""
  id <- ""

  if (!is.null(.secret) && stringr::str_trim(.secret) != "") {

    args_token_input <- args
    if (.serialize_data && !is.null(req$args$data)) {
      args_token_input <- args$data
    }

    token <- shiny.telemetry::build_token(args_token_input, .secret)
    id <- shiny.telemetry::build_id_from_secret(.secret)
  }

  req$args$token <- token
  req$args$id <- id

  if (.serialize_data && !is.null(req$args$data)) {
    req$args$data <- jsonlite::serializeJSON(req$args$data)
  }
  req
}
