library(shiny)
library(shiny.telemetry)
library(dplyr)
library(config)

get_user <- function(session) {
  username <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search)$username)
  if (is.null(username)) username <- "unknownUser"
  shiny::req(username)
  username
}

ui <- fluidPage(
  use_telemetry(),
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "bins", "Number of bins:", min = 1, max = 50, value = 30
      ),
      actionButton("apply_slider", "Apply")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  tags$hr(),
  tags$div(
    tags$h3("Sample application instrumented by Shiny.telemetry"),
    tags$p(glue::glue("Note: using {config::get('data_storage')$class_name} as data backend.")),
    tags$p("Information logged:"),
    tags$ul(
      tags$li("Start of session"),
      tags$li("Every time slider changes"),
      tags$li("Click of 'Apply' button")
    )
  )
)

# Default Telemetry with data storage backend using LogFile
telemetry <- Telemetry$new(
  name = "demo",
  version = "v0.0.9007",
  data_storage = DataStorageLogFile$new(
    username = get_user(session),
    log_file_path = file.path(getwd(), "user_stats.txt"),
    session_file_path = file.path(getwd(), "session_details.txt")
  )
)

# This sample application includes a configuration for RSConnect deployments,
# that uses parameters in `config.yml` file to define Data Storage backend
if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  telemetry <- Telemetry$new(
    name = "demo",
    version = "v0.0.9007",
    data_storage = do.call(
      config::get("data_storage")$class$new,
      config::get("data_storage")$params %>%
        purrr::assign_in("username", get_user(session))
    )
  )
}

server <- function(input, output, session) {
  t2 <- telemetry$start_session(input)

  # server code
  output$distPlot <- renderPlot({
    input$apply_slider
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })

  telemetry$log_click("another click")

  # or

  session$userData$telemetry$log_click("some_id_click")

  # or

  t2$log_click("another click")
}

shinyApp(ui = ui, server = server)
