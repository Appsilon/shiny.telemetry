library(shiny)
library(dplyr) # necessary to import %>%
library(RSQLite)
# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

# define function hot to get username
get_user <- function(session) {
  username <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search)$username)
  if (is.null(username)) username <- "unknownUser"
  shiny::req(username)
  return(username)
}

data_storage <- DataStorageRSQLite$new(
  username = get_user(session = NULL), db_path = "user_stats.sqlite"
)

run_analytics_dashboard(data_storage)
