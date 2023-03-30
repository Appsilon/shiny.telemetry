library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinyjs)
library(tidyr)
library(dplyr) # necessary to import %>%
library(purrr)
library(plotly)
library(timevis)
library(DT)
# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

# Connecting to a SQLite data storage backend
data_storage <- DataStoragePlumber$new(
  username = "test_user",
  hostname = "connect.appsilon.com",
  path = "shiny_telemetry_plumber",
  port = 443,
  protocol = "https",
  authorization = Sys.getenv("CONNECT_AUTHORIZATION_KEY"),
  secret = Sys.getenv("PLUMBER_SECRET")
)

run_analytics_dashboard(data_storage)
