library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinyjs)
library(tidyr)
library(dplyr)
library(purrr)
library(plotly)
library(timevis)
library(ggplot2)
library(mgcv)
library(config)
library(DT)

# Please install shiny.telemetry with all dependencies
# remotes::install_github("Appsilon/shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

# Default storage backend using LogFile
data_storage <- DataStorageLogFile$new(
  log_file_path = file.path(getwd(), "user_stats.txt")
)

# This sample application includes a configuration for RSConnect deployments,
# that uses parameters in `config.yml` file to define Data Storage backend
if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  data_storage <- do.call(
    config::get("data_storage")$class$new,
    config::get("data_storage")$params
  )
}

# check if there is data in the data_storage
if (nrow(data_storage$read_event_data()) > 0) {
  # run the analytics app if there is data
  analytics_app(data_storage = data_storage)
} else {
  # the empty app if there is no data
  empty_app()
}
