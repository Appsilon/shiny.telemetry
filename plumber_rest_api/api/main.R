box::use(
  config,
  glue[glue],
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
#' @post /insert
#' @serializer json
function(token, id, data, res){
  result <- insert$handler(data, token, id, data_storage$event_bucket)

  res$status <- result$status
  result
}

#' Read user data
#* @param from:string
#* @param to:string
#* @param token:string
#* @param id:string
#* @get /read_data
#' @serializer json
function(from, to, token, id, res) {
  log_debug('@get /read_data triggered')
  result <- read_data$handler(
    from, to, token, id, data_storage$read_event_data
  )
  res$status <- result$status
  result
}
