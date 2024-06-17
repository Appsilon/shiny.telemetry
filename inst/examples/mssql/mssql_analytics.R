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

# Default storage backend using MariaDB
data_storage <- DataStorageMSSQLServer$new(
  user = "sa",
  password = "my-Secr3t_Password",
  hostname = "localhost",
  port = 1433,
  dbname = "my_db",
  driver = "ODBC Driver 18 for SQL Server",
  trust_server_certificate = "YES"
)

analytics_app(data_storage = data_storage)
