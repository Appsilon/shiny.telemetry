# Plumber REST API as a DataStorage provider for shiny.telemetry

> REST API that receives write and read requests for telemetry data in `shiny.telemetry`

This [plumber](https://www.rplumber.io/) REST API serves as a middleware between an application that is being monitored by `shiny.telemetry` and a DataStorage backend that write the logging information to persistent storage. 

![Architecture](../inst/images/data_storage_plumber.svg)

## Setup

To get a minimal Plumber instance running locally it requires an `R` environment _(at least `≥v4.1`)_ and the `renv` R package.

Start by downloading or cloning the source files in the repository and open an R session on the `/plumber_rest_api` directory.

Install the required packages by calling `renv::restore()` inside the R session.

```R
> renv::restore()
```

Start the Plumber API by sourcing `plumber.R` file or by calling `plumber::plumb("plumber.R")`.

```R
> # source("plumber.R")
> plumber::plumb("plumber.R")
```

The plumber instance is now running and the API documentation can be accessed via [127.0.0.1:8087/__docs__/](http://127.0.0.1:8087/__docs__/). The `/health_check` endpoint can be used without any parameters.

In order to test as a `shiny.telemetry` data storage provider, you can run the code below in an R session.

The example will:

1. Create an instance of the `DataStoragePlumber` R6 class configured to use the local Plumber instance
1. Manually log several pieces of information:
    * Login
    * 2 Clicks
    * Session detail
1. Read the action and session logs
    * note the timestamps

```R
library(shiny.telemetry)
data_storage <- DataStoragePlumber$new(
  username = "test_user",
  hostname = "127.0.0.1",
  port = 8087,
  protocol = "http"
)

log_login(data_storage)
log_click(data_storage, "an_id")
log_click(data_storage, "a_different_id")

log_session_detail(data_storage, detail = "some detail")

data_storage$read_user_data("2020-01-01", "2035-01-01")
data_storage$read_session_data("2020-01-01", "2035-01-01")
```

## Communication signature & validation

To make sure th

