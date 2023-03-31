# shiny.telemetry

> Easy way for logging users activity and adding statistics panel to your Shiny app

## Install

Shiny.telemetry can be installed from github by using the remotes package:

```
remotes::install_github("Appsilon/shiny.telemetry")
```

## How to use in  a Shiny Dashboard?

This package can be used with a minimal setup that keeps track of. 

* Global `Telemetry` object that is across the different sessions
* In the UI a `use_telemetry()` call is required to add package-related javascript
* The server can be initialized with the method `start_session` of the `Telemetry object 
    * for example: `telemetry$start_session(input)`

The default `shinyApp` can be modified to add telemetry as it's shown below.

There isn't anything different from the user's perspective, but the data that is being tracked is available to read by:

```
# After running the app
Telemetry$new()$telemetry$read_events("2020-01-01", "2050-01-01")
```

Sample Shiny app:

```
library(shiny)
library(shiny.telemetry)

telemetry <- Telemetry$new() # Object to manage telemetry, by default it 
                             # will use a SQLite in the local filesystem

shinyApp(
  ui = fluidPage(
    numericInput("n", "n", 1),
    plotOutput("plot"),
    use_telemetry() # shiny.telemetry UI element
  ),
  server = function(input, output) {
    output$plot <- renderPlot( plot(head(cars, input$n)) )
  }
)

shinyAppDir(system.file("examples/01_hello", package="shiny"))


# The object can be passed to runApp()
app <- shinyApp(
  ui = fluidPage(
    numericInput("n", "n", 1),
    plotOutput("plot")
  ),
  server = function(input, output) {
    telemetry$start_session(input)
    output$plot <- renderPlot( plot(head(cars, input$n)) )
  }
)

runApp(app)
```

## How to display users' stats?

The 

```
library(shiny)
# please install shiny.telemetry with all dependencies
# > install.packages("shiny.telemetry", dependencies = TRUE)
library(shiny.telemetry)

telemetry <- Telemetry$new()

# define ui and server
ui <- shiny_stats_ui()

server <- shiny_stats_server(data_storage = telemetry$data_storage)
shinyApp(ui = ui, server = server, options = list(port = 8887, launch.browser = TRUE))
```

## Data providers

There are 3 different types of data providers that can range from local filesystem storage to a remote plumber REST API instance.

* SQLite using `DataStorageRSQLite` class
* Logfile using `DataStorageLogFile` class
* Plumber REST API using one of the providers above as backend using `DataStoragePlumber` class

The setup for plumber requires a valid Plumber instance running on the network and the communication can be protected. See Plumber deployment documentation for more information.

## Appsilon

<img src="https://avatars0.githubusercontent.com/u/6096772" align="right" alt="" width="6%" />

Appsilon is a **Posit (formerly RStudio) Full Service Certified Partner**.<br/>
Learn more
at [appsilon.com](https://appsilon.com).

Get in touch [opensource@appsilon.com](mailto:opensource@appsilon.com)

Check the [Rhinoverse](https://rhinoverse.dev).

<a href = "https://appsilon.com/careers/" target="_blank"><img src="http://d2v95fjda94ghc.cloudfront.net/hiring.png" alt="We are hiring!"/></a>
