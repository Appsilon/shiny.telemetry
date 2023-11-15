## Example application for {shiny.telemetry}

> Simple Shiny dashboard with inputs and tabular navigation to test run `shiny.telemetry`

These 2 folders contain an example application that tracks all inputs using `shiny.telemtry` and the accompanying analytics dashboard.

#### Run the example

To run the application, install the necessary dependencies:

```R
install.packages(
  c(
    "shiny.telemetry", "dplyr", "config"
  ),
  dependencies = c("Depends", "Imports", "Suggests")
)
```

And start the R/Shiny app by: `shiny::runApp("instrumentation")`

Afterwards, the analytics application will have access to the data and can be started by: `shiny::runApp("instrumentation")`

For more information, visit the [documentation](https://appsilon.github.io/shiny.telemetry/) of `shiny.telemetry`
