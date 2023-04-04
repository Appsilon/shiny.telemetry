
#' Telemetry class to manage analytics gathering at a global level
#'
#' @description
#' An instance of this class will define metadata and data storage provider
#' for gathering telemetry analytics of a Shiny dashboard.
#'
#'
#' The `name` and `version` parameters will describe the dashboard name and
#' version to track using analytics, allowing to store the analytics data from
#' multiple dashboards in the same data storage provider. As well as
#' discriminate different versions of the dashboard.
#'
#'
#' The default data storage provider uses a local SQLite database, but this
#' can be customizable when instantiating the class, by using another one of
#' the supported providers (see [DataStorage]).
#'
#' @seealso [shiny.telemetry::DataStorage] which this function wraps.
#' @export
#' @examples
#' telemetry <- Telemetry$new(
#'   data_storage = DataStorageRSQLite$new(
#'     db_path = tempfile(pattern = "telemetry", fileext = ".sqlite")
#'   )
#' )
#'
#' telemetry$start_session(input = shiny::reactiveValues(), logout = FALSE)
#'
#' telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |> tail()
#'
#' telemetry <- Telemetry$new(
#'   data_storage = DataStorageLogFile$new(
#'     log_file_path = tempfile(pattern = "user_stats", fileext = ".txt"),
#'     session_file_path = tempfile(pattern = "session_details", fileext = ".txt")
#'   )
#' )
#'
#' telemetry$start_session(shiny::reactiveValues(), logout = FALSE)
#'
#' telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |> tail()
Telemetry <- R6::R6Class( # nolint object_name_linter
  classname = "Telemetry",
  public = list(

    #' @description
    #' Constructor that initialize Telemetry instance with parameters.
    #'
    #' @param name (optional) string that identifies the name of the dashboard.
    #' By default it will store data with `(dashboard)`.
    #' @param version (optional) string that identifies the version of the
    #' dashboard. By default it will use `v0.0.0`.
    #' @param data_storage (optional) DataStorage instance where telemetry
    #' data is being stored.
    #' It can take any of data storage providers by this package,
    #' By default it will store in a sqlite local database on the current
    #' working directory with filename `telemetry.sqlite`

    initialize = function(
      name = "(dashboard)",
      version = "v0.0.0",
      data_storage = DataStorageRSQLite$new(
        db_path = file.path("telemetry.sqlite")
      )
    ) {
      checkmate::assert_string(name)
      checkmate::assert_string(version)

      private$.data_storage <- data_storage

      private$.name <- name
      private$.version <- version
    },

    #' @description
    #' Setup basic telemetry
    #'
    #' @param track_inputs flag that indicates if the basic telemetry should
    #' track the inputs that change version. `TRUE` by default
    #' @param track_values flag that indicates if the basic telemetry should
    #' track the values of the inputs that are changing. `FALSE` by default.
    #' This parameter is ignored if `track_inputs` is `FALSE`
    #' @param login flag that indicates if the basic telemetry should
    #' track when a session starts. `TRUE` by default.
    #' @param logout flag that indicates if the basic telemetry should
    #' track when the session ends. `TRUE` by default.
    #' @param browser_version flag that indicates if the basic telemetry should
    #' track the browser version. `TRUE` by default.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    start_session = function(
      track_inputs = TRUE,
      track_values = FALSE,
      login = TRUE,
      logout = TRUE,
      browser_version = TRUE,
      session = shiny::getDefaultReactiveDomain()
    ) {

      username <- shiny::isolate(
        shiny::parseQueryString(session$clientData$url_search)$username
      )
      if (is.null(username)) username <- "unknown_user"

      input <- shiny::reactiveValues()
      if (checkmate::test_r6(session, "ShinySession")) {
        input <- session$input
      }

      if (isTRUE(track_inputs) && isFALSE(private$track_all_inputs_flag)) {
        self$log_all_inputs(input, track_values)
        private$track_all_inputs_flag <- TRUE
      }

      if (isTRUE(login)) {
        self$log_login(session = session, username)
      }

      if (isTRUE(logout)) {
        self$log_logout(session = session, username)
      }

      if (isTRUE(browser_version)) {
        self$log_browser_version(input, session = session)
      }

      invisible(self)
    },

    #' @description read events data
    #'
    #' @param date_from date representing the starting day of results
    #' @param date_to date representing the last day of results

    read_events = function(date_from, date_to) {
      private$.data_storage$read_user_data(date_from, date_to)
    },

    # ###############################################################
    #
    #   _                __                  _   _
    #  | |              / _|                | | (_)
    #  | | ___   __ _  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___
    #  | |/ _ \ / _` | |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
    #  | | (_) | (_| | | | | |_| | | | | (__| |_| | (_) | | | \__ \
    #  |_|\___/ \__, | |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
    #            __/ |
    #           |___/
    #
    #  log functions
    # ##############################################################


    #' @description
    #' Log when session starts
    #'
    #' @param username string with username from current session
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_login = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      logger::log_debug("login", namespace = "shiny.telemetry")

      private$.log_event(
        event = "login user",
        value = username,
        session = session
      )
    },

    #' @description
    #' Log when session ends
    #'
    #' @param username string with username from current session
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_logout = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      shiny::onSessionEnded(function() {
        logger::log_debug("logout", namespace = "shiny.telemetry")

        private$.log_event(
          event = "logout user",
          value = username,
          session = session
        )
      }, session)
    },

    #' @description
    #' Log an action click
    #'
    #' @param id string that identifies a manual click to the dashboard.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_click = function(id, session = shiny::getDefaultReactiveDomain()) {
      checkmate::assert_string(id)

      logger::log_debug("click: {id}", namespace = "shiny.telemetry")

      private$.log_event(
        event = "click",
        id = id,
        session = session
      )
    },

    #' @description
    #' Log the browser version
    #'
    #' @param input [shiny::reactiveValues] object containing the inputs for
    #' a Shiny dashboard.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_browser_version = function(
      input, session = shiny::getDefaultReactiveDomain()
    ) {
      shiny::observeEvent(input$browser_version, {
        browser <- input$browser_version
        shiny::validate(
          shiny::need(browser, "'browser_info_js' should be set in app head")
        )
        logger::log_debug(
          "browser_version: {browser}", namespace = "shiny.telemetry"
        )

        private$.log_event(
          event = "browser",
          value = browser,
          session = session
        )
      })
    },

    #' @description
    #' Log session details
    #'
    #' @param detail string with details about the session.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_session = function(
      detail, session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(detail)

      logger::log_debug("session_details: {detail}", namespace = "shiny.telemetry")

      private$.log_session(
        detail = detail,
        session = session
      )
    },

    #' @description
    #' Track a button and track changes to this input (without storing the
    #' values)
    #'
    #' @param input [shiny::reactiveValues] object containing the inputs for
    #' a Shiny dashboard.
    #' @param input_id string that identifies the button in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_button = function(
      input, input_id, session = shiny::getDefaultReactiveDomain()
    ) {
      self$log_input(input, input_id, session = session)
    },

    #' @description
    #' A short description...
    #'
    #' @param input [shiny::reactiveValues] object containing the inputs for
    #' a Shiny dashboard.
    #' @param track_values flag that indicates if the basic telemetry should
    #' track the values of the inputs that are changing. `FALSE` by default.
    #' This parameter is ignored if `track_inputs` is `FALSE`.
    #' @param excluded_inputs vector of input_ids that should not be tracked.
    #' By default it doesn't track browser version, which is added by this
    #' package.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_all_inputs = function(
      input,
      track_values = FALSE,
      excluded_inputs = c("browser_version"),
      session = shiny::getDefaultReactiveDomain()
    ) {

      input_values <- shiny::isolate(shiny::reactiveValuesToList(input))

      if (checkmate::test_r6(session, "ShinySession")) {
        session$userData$shiny_input_values <- input_values
      }

      logger::log_debug(logger::skip_formatter(
        paste(
          "Default Shiny inputs initialized:",
          as.character(jsonlite::toJSON(input_values, auto_unbox = TRUE))
        )),
        namespace = "shiny.telemetry"
      )

      shiny::observe({
        if (checkmate::test_r6(session, "ShinySession")) {
          old_input_values <- session$userData$shiny_input_values
        } else {
          old_input_values <- input_values
        }
        new_input_values <- shiny::reactiveValuesToList(input)

        if (NROW(new_input_values) != 0) {
          names <- unique(c(names(old_input_values), names(new_input_values)))
          names <- setdiff(names, excluded_inputs)
          for (name in names) {
            old <- old_input_values[name]
            new <- new_input_values[name]
            if (!identical(old, new)) {
              if (isTRUE(track_values)) {
                logger::log_debug(
                  "Shiny input change detected on {name} ",
                  "{old[[name]]} -> {new[[name]]}",
                  namespace = "shiny.telemetry"
                )
              } else {
                logger::log_debug(
                  "Shiny input change detected on {name} (no value tracking) ",
                  "{old[[name]]} -> {new[[name]]}",
                  namespace = "shiny.telemetry"
                )
              }

              if (isTRUE(track_values)) {
                private$.log_event(
                  event = "input",
                  id = name,
                  value = new[[name]],
                  session = session
                )
                next
              }
              private$.log_event(
                event = "input",
                id = name,
                session = session
              )
            }
          }
        }
        if (checkmate::test_r6(session, "ShinySession")) {
          session$userData$shiny_input_values <- new_input_values
        } else {
          input_values <- new_input_values
        }
      })
    },

    #' @description
    #' A short description...
    #' @param input [shiny::reactiveValues] object containing the inputs for
    #' a Shiny dashboard.
    #' @param input_id string that identifies the generic input in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param track_value flag that indicates if the basic telemetry should
    #' track the value of the input that are changing. `FALSE` by default.
    #' @param matching_values An object specified possible values to register.
    #' @param input_type 'text' to registered bare input value, 'json' to parse
    #' value from JSON format.
    #' @param session ShinySession object or NULL to identify the current
    #' Shiny session.

    log_input = function(
      input,
      input_id,
      track_value = FALSE,
      matching_values = NULL,
      input_type = "text",
      session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(input_id)
      checkmate::assert_flag(track_value)
      checkmate::assert(
        .combine = "or",
        checkmate::check_atomic_vector(matching_values),
        checkmate::check_null(matching_values)
      )

      checkmate::assert_string(input_type)
      checkmate::assert_choice(input_type, c("text", "json"))

      shiny::observeEvent(
        input[[input_id]],
        {
          input_value <- input[[input_id]]

          if (isTRUE(track_value)) {
            return(private$log_input_value(
              input_id = input_id,
              input_value = input_value,
              matching_values = matching_values,
              input_type = input_type,
              session = session
            ))
          }

          private$.log_event(
            event = "input", id = input_id, session = session
          )

        },
        # Options to observeEvent call
        priority = -1, ignoreInit = TRUE
      )
    }

  ),
  active = list(

    #' @field data_storage instance of a class that inherits from
    #' [DataStorage]. See the documentation on that class for more information.

    data_storage = function() private$.data_storage,

    #' @field dashboard string with name of dashboard

    dashboard = function() private$.name,

    #' @field version string with version of the dashboard

    version = function() private$.version
  ),
  private = list(

    track_all_inputs_flag = FALSE,
    .name = NULL,
    .version = NULL,
    .data_storage = NULL,

    # Methods

    log_generic = function(
      event = NULL,
      id = NULL,
      value = NULL,
      session = NULL,
      add_username = TRUE,
      use_detail = FALSE,
      bucket = NULL
    ) {
      checkmate::assert_string(event, null.ok = TRUE)
      checkmate::assert_string(id, null.ok = TRUE)

      checkmate::assert(
        .combine = "or",
        checkmate::check_list(value, null.ok = TRUE),
        checkmate::check_atomic(value)
      )
      checkmate::assert_r6(session, classes = "ShinySession", null.ok = TRUE)

      payload <- list(
        dashboard = self$dashboard,
        version = self$version
      )

      session_token <- NULL
      if (!is.null(session)) {
        payload$session <- session$token
      }

      if (checkmate::test_list(value)) {
        value <- jsonlite::toJSON(value)
      }

      if (!is.null(event)) {
        payload$action <- event
      }

      if (!is.null(id)) {
        payload$id <- id
      }

      if (isTRUE(add_username)) {
        if (!is.null(session)) {
          username <- shiny::isolate(
            shiny::parseQueryString(session$clientData$url_search)$username
          )
          if (is.null(username)) username <- "anonymous"
          shiny::req(username)
          payload$username <- username
        }
      }

      if (!is.null(value)) {
        if (isTRUE(use_detail)) {
          payload$detail <- value
        } else {
          payload$value <- value
        }
      }

      self$data_storage$insert(
        values = payload,
        bucket = bucket
      )
    },

    .log_event = function(
      event = NULL, id = NULL, value = NULL, session = NULL
    ) {
      private$log_generic(
        event = event,
        id = id,
        value = value,
        session = session,
        add_username = TRUE,
        bucket = self$data_storage$action_bucket
      )
    },

    .log_session = function(detail = NULL, session = NULL) {
      private$log_generic(
        value = detail,
        session = session,
        add_username = FALSE,
        use_detail = TRUE,
        bucket = self$data_storage$session_bucket
      )
    },

    log_input_value = function(
      input_id, input_value, matching_values, input_type, session
    ) {
      if (is.null(input_value)) {
        return(NULL)
      }

      if (is.logical(input_value)) {
        input_value <- as.character(input_value)
      }

      if (!is.null(matching_values) && input_type == "json") {
        # Used to be parse_val() function
        input_value <- jsonlite::fromJSON(
          ifelse(is.null(input_value), "\"\"", input_value)
        )
      }

      if (
        is.null(matching_values) |
        (!is.null(matching_values) && input_value %in% matching_values)
      ) {
        # save each value separately (if more than 1)
        n_values <- length(input_value)
        input_ids <- input_id
        if (n_values > 1) {
          input_ids <- glue::glue("{input_id}_{seq(1, n_values)}")
        }

        purrr::walk2(
          input_value,
          input_ids,
          ~ private$.log_event(
            event = "input",
            id = .y,
            value = .x,
            session = session
          )
        )
      }
    }
  )
)
