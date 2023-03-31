library(shiny)
# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

library(shiny.semantic)
library(semantic.dashboard)
library(shinyjs)
library(tidyr)
library(dplyr)
library(purrr)
library(plotly)
library(timevis)
library(DT)

data_storage <- DataStorageLogFile$new(
  username = "test_user",
  log_file_path = file.path(getwd(), "user_stats.txt"),
  session_file_path = file.path(getwd(), "session_details.txt")
)

# Used for package deployment of test application on connect
if (Sys.getenv("FORCE_PLUMBER_CONFIG") == "1") source("force_plumber_ds.R")

analytics_app(data_storage = data_storage)
