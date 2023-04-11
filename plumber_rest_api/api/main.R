box::use(
  config,
  glue[glue],
  future[plan],
  promises[future_promise],
  logger[
    log_info, log_debug, log_error, log_warn, log_threshold, log_layout,
    layout_glue_colors
  ],
)

box::use(
  api/handlers/insert,
  api/handlers/read_data,
  api/logic/setup[data_storage],
)

# Setup

# logger
log_threshold(config$get("log_level"))
log_layout(layout_glue_colors)

#' Example
#' @get /health_check
#' @serializer json
function(req, res){
  log_debug('@get /health_check triggered')
  msg <- glue::glue("Everything is nice and peachy at {Sys.time()}")
  res$status <- 200
  list(success = jsonlite::unbox(msg))
}

#' Insert user data
#* @param data:string
#* @param token:string
#* @param id:string
#' @post /user_log
#' @serializer json
function(token = NULL, id = NULL, data, res){
  result <- insert$handler(data, token, id, data_storage$action_bucket)

  res$status <- result$status
  result
}

#' Insert session data
#* @param data:string
#* @param token:string
#* @param id:string
#' @post /session_details
#' @serializer json
function(data, token = NULL, id = NULL, res){
  # future_promise({
  result <- insert$handler(data, token, id, data_storage$session_bucket)
  res$status <- result$status
  result
  # })
}

#' Read user data
#* @param from:string
#* @param to:string
#* @param token:string
#* @param id:string
#* @get /read_user_data
#' @serializer json
function(from, to, token = NULL, id = NULL, res) {
  log_debug('@get /read_user_data triggered')
  result <- read_data$handler(
    from, to, token, id, data_storage$read_user_data
  )
  res$status <- result$status
  result
}

#' Read session data
#* @param from:string
#* @param to:string
#* @param token:string
#* @param id:string
#* @get /read_session_data
#' @serializer json
function(from, to, token = NULL, id = NULL, res) {
  result <- read_data$handler(
    from, to, token, id, data_storage$read_session_data
  )
  res$status <- result$status
  result
}
