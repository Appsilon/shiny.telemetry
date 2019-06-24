basic_view_role <- "basic_view"
admin_view_role <- "admin_view"

users_roles <- list(
  list(name = "basic", actions = c(basic_view_role)),
  list(name = "admin", actions = c(admin_view_role))
)

target_apps_config_path <- "target_configs/"

stats_db_config_variables <- c("DB_NAME", "DB_HOST", "DB_PORT", "DB_USER", "DB_PASSWORD", "DRV")

target_env_variables <- c("value", "label", stats_db_config_variables)

users_credentials_db_env_variables <- c(
  "USERS_DB_NAME", "USERS_DB_HOST", "USERS_DB_PORT",
  "USERS_DB_USER", "USERS_DB_PASSWORD", "USERS_DRV"
)

#' Possible drivers allowed for log storage
#' @description These values can be used as DRV parameter inside config_file.
#' @export
AVAILABLE_DB_DRIVERS <- c("postgresql", "sqlite")

#' Maximum of connections
#' @description Set maximum of connections to postgres from one session
#' @export
MAX_POSTGRES_CONNECTIONS <- 50
