library(shiny)
library(RSQLite)
# please install shiny.stats with all dependencies
# install.packages("shiny.stats", dependencies = TRUE)
library(shiny.stats)

# prepare credentials list to access logs:
db_credentials <- list(
  DB_NAME = "user_stats.sqlite",
  DB_DRIVER = "SQLite"
)

# define function hot to get username
get_user <- function(session) {
  username <- isolate(parseQueryString(session$clientData$url_search)$username)
  req(username)
  return(username)
}

# define ui and server
ui <- shiny_stats_ui()
server <- shiny_stats_server(get_user, db_credentials = db_credentials)

shinyApp(ui = ui, server = server, options = list(port = 8887, launch.browser = TRUE))
