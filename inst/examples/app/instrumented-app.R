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
  # Connecting to a SQLite data storage backend
  data_storage <- DataStorageRSQLite$new(
    username = get_user(session), db_path = "user_stats.sqlite"
  )

  # registering login
  log_login(data_storage)

  log_click(data_storage, id = "random_click_with_init")

  # selecting registered actions to watch
  log_button(data_storage, input, button_id = "apply_slider")
  log_input(data_storage, input, input_id = "bins")

  # server code
  output$distPlot <- renderPlot({
    input$apply_slider
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })

  # registering logout (this also disconnects connection object, if not used
  #  take care of it on your own)
  log_logout(data_storage)
}

shinyApp(ui = ui, server = server, options = list(port = 8888, launch.browser = FALSE))
