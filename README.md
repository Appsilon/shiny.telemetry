shiny.telemetry
============

Easy way for logging users activity and adding statistics panel to your Shiny app.

## Prerequisites

Remote DB. Can be PostgreSQL DB or simple sqlite file.

## How to register user's action?

1. Prepare DB to store results (here we use SQLite, but PostgreSQL is possible as well).

```
# run only once
connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = "user_stats.sqlite")
DBI::dbDisconnect(connection)
```

2. Depending on your authentication approach, define function to extract username.
In this example we just extract username from url `username` parameter.
```
get_user <- function(session) {
  parseQueryString(isolate(session$clientData$url_search))$username
}
```
3. Structure your main app to register user's actions.

```
library(shiny)
library(shiny.telemetry)
library(RSQLite)

ui <- fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      actionButton("apply_slider", "Apply")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_stats.sqlite")

  # creating user connection list and making sure required tables exist in DB
  user_connection <- initialize_connection(connection, username = get_user(session))

  # registering login
  log_login(user_connection)

  # selecting registered actions to watch
  log_click(user_connection, id = "apply_slider")
  log_input(user_connection, input, input_id = "bins")

  # server code
  output$distPlot <- renderPlot({
    input$apply_slider
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  # registering logout (this also disconnects connection object, if not used take care of it on your own)
  log_logout(user_connection)
}
```

4. Run and play with your app at `http://localhost:8888/?username=<username>`.
```
shinyApp(ui = ui, server = server, options = list(port = 8888, launch.browser = FALSE))
```

## How to display users' stats?

1. Create the following app.
```
library(shiny)
library(RSQLite)
# please install shiny.telemetry with all dependencies
# install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

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
```

2. Run the app and see usage statistics at `http://localhost:8887/?username=<username>`.
```
shinyApp(ui = ui, server = server, options = list(port = 8887, launch.browser = FALSE))
```

## Appsilon

<img src="https://avatars0.githubusercontent.com/u/6096772" align="right" alt="" width="6%" />

Appsilon is a **Posit (formerly RStudio) Full Service Certified Partner**.<br/>
Learn more
at [appsilon.com](https://appsilon.com).

Get in touch [opensource@appsilon.com](mailto:opensource@appsilon.com)

Check the [Rhinoverse](https://rhinoverse.dev).

<a href = "https://appsilon.com/careers/" target="_blank"><img src="http://d2v95fjda94ghc.cloudfront.net/hiring.png" alt="We are hiring!"/></a>
