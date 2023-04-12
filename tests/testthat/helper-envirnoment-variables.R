#'
#' @examples
#' Sys.setenv(Z_AAA="123", Z_BBB = "456")
#' capture_evironment_variables(c("Z_AAA", "Z_BBB"))
capture_evironment_variables <- function(...) {
  var_names <- list(...)
  var_names %>% purrr::map(
    ~ Sys.getenv(.x)
  ) %>%
    rlang::set_names(var_names)
}

#'
#' @examples
#' Sys.setenv(Z_AAA="ccc", Z_BBB = "EEE")
#' old_vars <- capture_evironment_variables(c("Z_AAA", "Z_BBB"))
#' Sys.setenv(Z_AAA="123", Z_BBB = "456")
#' glue::glue("Z_AAA = {Sys.getenv(\"Z_AAA\")} Z_BBB = {Sys.getenv(\"Z_BBB\")}")
#' restore_evironment_variables(old_vars)
#' glue::glue("Z_AAA = {Sys.getenv(\"Z_AAA\")} Z_BBB = {Sys.getenv(\"Z_BBB\")}")
restore_evironment_variables <- function(captured_vars = list()) {
  do.call(
    Sys.setenv,
    captured_vars
  )
}

reset_box_cache <- function() {
  loaded_mods <- loadNamespace("box")$loaded_mods
  rm(list = ls(loaded_mods), envir = loaded_mods)
}
