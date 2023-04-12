library(shiny)
library(shiny.telemetry)
library(dplyr)
library(config)

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
    log_file_path = file.path(getwd(), "user_stats.txt"),
    session_file_path = file.path(getwd(), "session_details.txt")
  )
)

# This sample application includes a configuration for RSConnect deployments,
# that uses parameters in `config.yml` file to define Data Storage backend
if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
  print(config::get("data_storage"))
  telemetry <- Telemetry$new(
    name = "demo",
    version = "v0.0.9007",
    data_storage = do.call(
      config::get("data_storage")$class_name$new,
      config::get("data_storage")$params
    )
  )
}

server <- function(input, output, session) {
  telemetry$start_session()

  # server code
  output$distPlot <- renderPlot({
    input$apply_slider
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = isolate(input$bins) + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })

  telemetry$log_click("another click")
}

shinyApp(ui = ui, server = server)
