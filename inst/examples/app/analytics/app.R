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
log_file_path <- file.path("..", "instrumentation", "user_stats.txt")
if (!dir.exists(dirname(log_file_path))) log_file_path <- "user_stats.txt"

data_storage <- DataStorageLogFile$new(
  log_file_path = log_file_path
)

# This sample application includes a configuration for RSConnect deployments,
# that uses parameters in `config.yml` file to define Data Storage backend
if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  data_storage <- do.call(
    config::get("data_storage")$class$new,
    config::get("data_storage")$params
  )
}

analytics_app(data_storage = data_storage)

# shiny::shinyApp(system.file("examples", "app", "analytics", package = "shiny.telemetry")) # nolint: commented_code, line_length.
