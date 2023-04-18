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
library(shiny.telemetry)

# Default storage backend using LogFile
data_storage <- DataStoragePostgreSQL$new(
  user = "postgres", password = "mysecretpassword"
)

analytics_app(data_storage = data_storage)
