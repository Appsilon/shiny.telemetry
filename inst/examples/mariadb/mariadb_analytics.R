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
data_storage = DataStorageMariaDB$new(
  user = "mariadb", password = "mysecretpassword"
)

analytics_app(data_storage = data_storage)
