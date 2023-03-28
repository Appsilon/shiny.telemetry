library(shiny)
library(dplyr) # necessary to import %>%
library(RSQLite)
# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

data_storage <- DataStorageRSQLite$new(
  username = "viewer", db_path = "user_stats.sqlite"
)

run_analytics_dashboard(data_storage)
