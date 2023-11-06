## Example application for {shiny.telemetry}

These 2 folders contain an example application that tracks all inputs using `shiny.telemtry` and the accompanying analytics dashboard.

To run the application, install the necessary dependencies:

```R
install.packages(
  c(
    "shiny", "semantic.dashboard", "shiny.semantic", "shiny.telemetry",
    "dplyr", "config", "shiny.telemtry"
  ),
  dependencies = c("Depends", "Imports", "Suggests")
)
```

And start the R/Shiny app by: `shiny::runApp("instrumentation")`

Afterwards, the analytics application will have access to the data and can be started by: `shiny::runApp("instrumentation")`

For more information, visit the [documentation](https://appsilon.github.io/shiny.telemetry/) of `shiny.telemetry`
