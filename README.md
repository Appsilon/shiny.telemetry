# shiny.telemetry

> Easy logging of users activity and session events of your Shiny App 

This R package allows to track events happening on a session and storing to a database, either locally at the deployment or remotely.

It allows the developers to have access to insights about the dashboard usage and adoption, so that they can understand the users better and tailor the dashboards and their features.

## Install

The `shiny.telemetry` package can be installed from GitHub by using the remotes package:

```
remotes::install_github("Appsilon/shiny.telemetry")
```

## How to use in a Shiny Dashboard?

`shiny.telemetry` allows for a minimal setup with only 3 commands that can track some information about the session:

* When session starts and ends
* The browser version used by the client
* Changes in the inputs _(doesn't track values by default)_

The code below runs a minimal example of a shiny application that uses `shiny.telemetry`.
In this example, this package will keep the session information and all changes to the `numericInput`.

ℹ️ _note_: From the user's perspective of using the dashboard nothing happens as all operations run in the background.

```R
telemetry <- shiny.telemetry::Telemetry$new() # 1. Initialize telemetry with default options
shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny.telemetry::use_telemetry(), # 2. Add necessary Javascript to Shiny
    shiny::numericInput("n", "n", 1)
  ),
  server = function(input, output) {
    telemetry$start_session() # 3. Minimal setup to track events
  }
)
```

When inspecting the code above, we can breakdown the 3 lines of code by:

1. Global `Telemetry` object that is used across the different sessions
2. Add necessary Javascript to the UI by calling `use_telemetry()`. It is used to track browser version. 
3. Initialize the session-specific tracking by  calling method `start_session()` of the `Telemetry` object 

## How to access the data?

The developers and administrators of the dashboard can access the data that is gathered by `shiny.telemetry` via a Telemetry object or directly from `DataStorage` via the appropriate provider.

```
# After running the app
shiny.telemetry::Telemetry$new()$read_events("2020-01-01", "2050-01-01")
shiny.telemetry::DataStorageRSQLite$new(db_path = "telemetry.sqlite") # default provider and path for Telemetry$new()
```

The package has an analytics sample dashboard to help access the data. It is located at `inst/examples/app/analytics` and it should be modified so that it references the correct `DataStorage` provider and configuration.

## Data providers

There are 3 different types of data providers that can range from local filesystem storage to a remote plumber REST API instance.

* SQLite using `DataStorageRSQLite` class
* Logfile using `DataStorageLogFile` class
* Plumber REST API using one of the providers above as backend using `DataStoragePlumber` class

The setup for plumber requires a valid Plumber instance running on the network and the communication can be protected. See Plumber deployment documentation for more information.

## Debugging the Telemetry calls

The package uses the `logger` package internally with the `shiny.telemetry` namespace. To debug the `shiny.telemetry` calls in the dashboard, change the threshold of this namespace to `DEBUG`.

```R
logger::log_threshold("DEBUG", namespace = "shiny.telemetry")
```

ℹ️ _note_: This command can be run before the Shiny call or by adding it to the `.Rprofile`.

## Appsilon

<img src="https://avatars0.githubusercontent.com/u/6096772" align="right" alt="" width="6%" />

Appsilon is a **Posit (formerly RStudio) Full Service Certified Partner**.<br/>
Learn more at [appsilon.com](https://appsilon.com).

Get in touch [opensource@appsilon.com](mailto:opensource@appsilon.com)

Check the [Rhinoverse](https://rhinoverse.dev).

<a href = "https://appsilon.com/careers/" target="_blank"><img src="http://d2v95fjda94ghc.cloudfront.net/hiring.png" alt="We are hiring!"/></a>
