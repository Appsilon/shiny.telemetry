library(shiny)

# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

library(dplyr)

logger::log_threshold("DEBUG", namespace = "shiny.telemetry")

# Default storage backend using LogFile
data_storage <- DataStorageLogFile$new(
  username = "test_user",
  log_file_path = file.path(getwd(), "user_stats.txt"),
  session_file_path = file.path(getwd(), "session_details.txt")
)

# This sample application includes a configuration for RSConnect deployments,
# that uses parameters in `config.yml` file to define Data Storage backend
if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  data_storage <- do.call(
    config::get("data_storage")$class$new,
    config::get("data_storage")$params %>%
      purrr::assign_in("username", "test_user")
  )
}

# Used for package deployment of test application on connect
if (Sys.getenv("FORCE_PLUMBER_CONFIG") == "1") source("force_plumber_ds.R")

analytics_app(data_storage = data_storage)
