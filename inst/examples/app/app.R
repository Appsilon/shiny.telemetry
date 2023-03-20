library(shiny)
library(shiny.telemetry)
library(RSQLite)

get_user <- function(session) {
  parseQueryString(isolate(session$clientData$url_search))$username
}

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

shinyApp(ui = ui, server = server, options = list(port = 8888, launch.browser = FALSE))
