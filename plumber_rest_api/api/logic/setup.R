box::use(
  config,
  shiny.telemetry[
    DataStorageSQLite, DataStorageLogFile, build_id_from_secret,
  ],
  rlang[abort],
  digest[digest],
  checkmate[test_string],
  purrr[pluck, set_names],
  stringr[str_trim, str_sub],
  logger[log_warn, log_debug, log_info],
  dplyr[`%>%`],
)

STORAGE_METHODS <- list(
  sqlite = DataStorageSQLite,
  logfile = DataStorageLogFile,
  .default = DataStorageSQLite
)

#' Get storage
#' @param driver string with method of storage see shiny.telemtry::DataStorage
#'
#' @examples
#' get_storage("sqlite")
get_storage <- function(driver) {
  # This is used exclusively for testing purposes
  if (is.null(driver)) {
    return(STORAGE_METHODS$.default)
  }

  if (is.null(STORAGE_METHODS[[driver]])) {
    log_warn("Method '{driver}' is not supported, using default")
    return(STORAGE_METHODS$.default)
  }

  STORAGE_METHODS[[driver]]
}

#
setup_storage <- function() {

  storage_config <- config$get("storage_method")

  data_storage <- do.call(
    get_storage(storage_config$driver)$new,
    storage_config[[storage_config$driver]]$params
  )

  data_storage
}

#
setup_secrets <- function(tokens_raw) {
  if (!test_string(tokens_raw, null.ok = TRUE)) {
   rlang::abort("SECRET_TOKENS environmental variable must be a string")
  }

  if (is.null(tokens_raw) || str_trim(tokens_raw) == "") {

    if(!config$get("allow_empty_tokens")) {
      rlang::abort("SECRET_TOKENS environmental variable must be defined.")
    }

    return(NULL)
  }

  str_trim(tokens_raw) %>%
    strsplit(" +") %>%
    pluck(1) %>%
    set_names(~sapply(., shiny.telemetry::build_id_from_secret))
}

#' @export
session_secrets <- setup_secrets(config$get("tokens"))

#' @export
data_storage <- setup_storage()
