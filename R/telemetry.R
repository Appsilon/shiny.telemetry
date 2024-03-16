
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
#' the supported providers (see [`DataStorage`]).
#'
#' @seealso [`DataStorage`] which this function wraps.
#' @export
#' @examples
#' log_file_path <- tempfile(fileext = ".txt")
#' telemetry <- Telemetry$new(
#'   data_storage = DataStorageLogFile$new(log_file_path = log_file_path)
#' )
#'
#' #
#' # Create dummy session (only for example purposes)
#' session <- shiny::MockShinySession$new()
#' class(session) <- c(class(session), "ShinySession")
#'
#' telemetry$start_session(session = session)
#'
#' telemetry$log_click("a_button", session = session)
#'
#' telemetry$log_custom_event("a_button", list(value = 2023), session = session)
#' telemetry$log_custom_event("a_button", list(custom_field = 23), session = session)
#'
#' # Manual call loging with custom username
#' telemetry$log_login("ben", session = session)
#'
#' telemetry$data_storage$read_event_data("2020-01-01", "2025-01-01")
#'
#' file.remove(log_file_path)
#'
#' #
#' # Using SQLite
#'
#' db_path <- tempfile(fileext = ".sqlite")
#' telemetry <- Telemetry$new(
#'   data_storage = DataStorageSQLite$new(db_path = db_path)
#' )
#'
#' telemetry$log_custom_event("a_button", list(value = 2023), session = session)
#' telemetry$log_custom_event("a_button", list(custom_field = 23), session = session)
#'
#' telemetry$data_storage$read_event_data("2020-01-01", "2025-01-01")
#'
#' file.remove(db_path)
Telemetry <- R6::R6Class( # nolint object_name_linter
  classname = "Telemetry",
  public = list(

    #' @description
    #' Constructor that initializes Telemetry instance with parameters.
    #'
    #' @param app_name (optional) string that identifies the name of the dashboard.
    #' By default it will store data with `(dashboard)`.
    #' @param version (optional) string that identifies the version of the
    #' dashboard. By default it will use `v0.0.0`.
    #' @param data_storage (optional) `DataStorage` instance where telemetry
    #' data is being stored.
    #' It can take any of data storage providers by this package,
    #' By default it will store in a SQLite local database in the current
    #' working directory with filename `telemetry.sqlite`

    initialize = function(
      app_name = "(dashboard)",
      data_storage = DataStorageSQLite$new(
        db_path = file.path("telemetry.sqlite")
      )
    ) {
      checkmate::assert_string(app_name)

      private$.data_storage <- data_storage

      private$.name <- app_name
    },

    #' @description
    #' Setup basic telemetry
    #'
    #' @param track_inputs flag that indicates if the basic telemetry should
    #' track the inputs that change value. `TRUE` by default
    #' @param track_values flag that indicates if the basic telemetry should
    #' track the values of the inputs that are changing. `FALSE` by default.
    #' This parameter is ignored if `track_inputs` is `FALSE`
    #' @param login flag that indicates if the basic telemetry should
    #' track when a session starts. `TRUE` by default.
    #' @param logout flag that indicates if the basic telemetry should
    #' track when the session ends. `TRUE` by default.
    #' @param browser_version flag that indicates that the browser version
    #' should be tracked.`TRUE` by default.
    #' @param navigation_input_id string or vector of strings that represent
    #' input ids and which value should be tracked as navigation events. i.e.
    #' a change in the value represent a navigation to a page or tab.
    #' By default, no navigation is tracked.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #' @param username Character with username. If set, it will overwrite username
    #' from session object.
    #' @param track_anonymous_user `TRUE` or `FALSE.`If `TRUE` and username is `NULL`,
    #' it will set a cookie to track anonymous user.`TRUE` by default
    #'
    #' @return Nothing. This method is called for side effects.

    start_session = function(
      track_inputs = TRUE,
      track_values = FALSE,
      login = TRUE,
      logout = TRUE,
      browser_version = TRUE,
      navigation_input_id = NULL,
      session = shiny::getDefaultReactiveDomain(),
      username = NULL,
      track_anonymous_user = TRUE
    ) {

      checkmate::assert_flag(track_inputs)
      checkmate::assert_flag(track_values)
      checkmate::assert_flag(login)
      checkmate::assert_flag(logout)
      checkmate::assert_flag(browser_version)
      checkmate::assert_flag(track_anonymous_user)

      checkmate::assert_character(navigation_input_id, null.ok = TRUE)

      username <- private$get_user(session, username, track_anonymous_user)

      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession"),
        checkmate::check_class(session, "session_proxy")
      )

      input <- session$input

      if (isTRUE(track_inputs)) {
        private$.log_all_inputs(
          track_values,
          excluded_inputs = c(
            "browser_version"
          ),
          navigation_inputs = navigation_input_id,
          session = session
        )
      } else {
        sapply(navigation_input_id, self$log_navigation)
      }

      if (isTRUE(login)) {
        self$log_login(session = session, username)
      }

      if (isTRUE(logout)) {
        self$log_logout(session = session, username)
      }

      if (isTRUE(browser_version)) {
        self$log_browser_version(session = session)
      }

      NULL
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
    #' Log an input change as a navigation event
    #'
    #' @param input_id string that identifies the generic input in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_navigation = function(
      input_id, session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(input_id)
      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession"),
        checkmate::check_class(session, "session_proxy")
      )

      private$.log_input(
        input_id = input_id,
        track_value = TRUE,
        event_type = "navigation",
        session = session
      )
    },

    #' @description
    #' Log a navigation event manually by indicating the id (as input id)
    #'
    #' @param navigation_id string that identifies navigation event.
    #' @param value string that indicates a value for the navigation
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_navigation_manual = function(
      navigation_id, value, session = shiny::getDefaultReactiveDomain()
    ) {
      logger::log_debug(
        "Writing 'navigation' event with ",
        "id: '{navigation_id}' and value: '{value}'",
        namespace = "shiny.telemetry"
      )

      private$.log_event(
        type = "navigation",
        details = list(id = navigation_id, value = value),
        session = session
      )
    },

    #' @description
    #' Log when session starts
    #'
    #' @param username string with username from current session
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_login = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      logger::log_debug("event: login", namespace = "shiny.telemetry")

      private$log_generic(
        type = "login",
        details = list(username = username),
        session = session
      )
    },

    #' @description
    #' Log when session ends
    #'
    #' @param username string with username from current session
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_logout = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      shiny::onSessionEnded(function() {
        logger::log_debug("event: logout", namespace = "shiny.telemetry")

        private$log_generic(
          type = "logout",
          details = list(username = username),
          session = session
        )
      }, session)
    },

    #' @description
    #' Log an action click
    #'
    #' @param id string that identifies a manual click to the dashboard.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_click = function(id, session = shiny::getDefaultReactiveDomain()) {
      checkmate::assert_string(id)

      logger::log_debug("event: click: {id}", namespace = "shiny.telemetry")

      private$log_generic(
        type = "click",
        details = list(id = id),
        session = session
      )
    },

    #' @description
    #' Log the browser version
    #'
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_browser_version = function(
      session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession"),
        checkmate::check_class(session, "session_proxy")
      )

      input <- session$input

      shiny::observeEvent(input$browser_version, {
        browser <- input$browser_version

        shiny::validate(
          shiny::need(browser, "'browser_info_js' should be set in app head")
        )
        logger::log_debug(
          "event: browser_version: {browser}", namespace = "shiny.telemetry"
        )

        private$log_generic(
          type = "browser",
          details = list(value = browser),
          session = session
        )
      })
    },

    #' @description
    #' Track a button and track changes to this input (without storing the
    #' values)
    #'
    #' @param input_id string that identifies the button in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param track_value flag that indicates if the basic telemetry should
    #' track the value of the input that are changing. `FALSE` by default.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_button = function(
      input_id,
      track_value = FALSE,
      session = shiny::getDefaultReactiveDomain()
    ) {
      self$log_input(input_id, track_value = track_value, session = session)
    },

    #' @description
    #' A short description...
    #'
    #' @param track_values flag that indicates if the basic telemetry should
    #' track the values of the inputs that are changing. `FALSE` by default.
    #' This parameter is ignored if `track_inputs` is `FALSE`.
    #' @param excluded_inputs vector of input_ids that should not be tracked.
    #' By default it doesn't track browser version, which is added by this
    #' package.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_all_inputs = function(
      track_values = FALSE,
      excluded_inputs = c("browser_version"),
      session = shiny::getDefaultReactiveDomain()
    ) {
      private$.log_all_inputs(
        track_values = track_values,
        excluded_inputs = excluded_inputs,
        navigation_inputs = c(),
        session = session
      )
    },

    #' @description
    #' A short description...
    #' @param input_id string that identifies the generic input in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param track_value flag that indicates if the basic telemetry should
    #' track the value of the input that are changing. `FALSE` by default.
    #' @param matching_values An object specified possible values to register.
    #' @param input_type `"text"` to registered bare input value, `"json"` to parse
    #' value from `JSON` format.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_input = function(
      input_id,
      track_value = FALSE,
      matching_values = NULL,
      input_type = "text",
      session = shiny::getDefaultReactiveDomain()
    ) {
      private$.log_input(
        input_id = input_id,
        track_value = track_value,
        matching_values = matching_values,
        input_type = input_type,
        event_type = "input",
        session = session
      )
    },

    #' @description
    #' Log a manual input value.
    #'
    #'
    #' This can be called in telemetry and is also used as a layer between
    #' log_input family of functions and actual log event.
    #' It creates the correct payload to log the event internally.
    #'
    #' @param input_id string that identifies the generic input in the Shiny
    #' application so that the function can track and log changes to it.
    #' @param value (optional) scalar value or list with the value to register.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_input_manual = function(
      input_id, value = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(input_id)

      logger::log_debug(
        "event: input '{input_id}' change: ",
        "{dplyr::coalesce(as.character(value), \"'NULL' (note: it might not be tracked)\")}",
        namespace = "shiny.telemetry"
      )

      private$.log_event(
        type = "input",
        details = list(id = input_id, value = value),
        session = session
      )
    },

    #' @description
    #' Log a manual event
    #'
    #' @param event_type string that identifies the event type
    #' @param details (optional) scalar value or list with the value to register.
    #' @param session `ShinySession` object or NULL to identify the current
    #' Shiny session.
    #'
    #' @return Nothing. This method is called for side effects.

    log_custom_event = function(
      event_type, details = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      private$.log_event(
        type = event_type, details = details, session = session
      )
    }

  ),
  active = list(

    #' @field data_storage instance of a class that inherits from
    #' [`DataStorage`]. See the documentation on that class for more information.

    data_storage = function() private$.data_storage,

    #' @field app_name string with name of dashboard

    app_name = function() private$.name

  ),
  private = list(

    .name = NULL,
    .version = NULL,
    .data_storage = NULL,

    # Methods

    log_generic = function(
      type = NULL,
      details = NULL,
      session = NULL
    ) {
      checkmate::assert_string(type, null.ok = TRUE)

      checkmate::assert(
        .combine = "or",
        checkmate::check_list(details, null.ok = TRUE),
        checkmate::check_scalar(details)
      )
      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession", null.ok = TRUE),
        checkmate::check_class(session, "session_proxy")
      )

      self$data_storage$insert(
        app_name = self$app_name,
        type = type,
        session = session$token,
        details = details
      )
    },

    .log_all_inputs = function(
      track_values,
      excluded_inputs,
      navigation_inputs,
      session
    ) {
      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession"),
        checkmate::check_class(session, "session_proxy")
      )

      checkmate::assert_flag(track_values)
      checkmate::assert_character(excluded_inputs, null.ok = TRUE)
      checkmate::assert_character(navigation_inputs, null.ok = TRUE)

      if (is.null(navigation_inputs)) {
        navigation_inputs <- c()
      }

      input_values <- shiny::isolate(
        shiny::reactiveValuesToList(session$input)
      )
      session$userData$shiny_input_values <- input_values

      logger::log_debug(logger::skip_formatter(
                                               paste(
                                                 "shiny inputs initialized:",
                                                 paste(names(input_values), collapse = ", ")
                                               )),
      namespace = "shiny.telemetry"
      )

      # Log initial value for navigation
      input_values[names(input_values) %in% navigation_inputs] %>%
        names() %>%
        purrr::walk(
          ~ self$log_navigation_manual(
            navigation_id = .x,
            value = input_values[[.x]],
            session = session
          )
        )

      shiny::observe({
        old_input_values <- session$userData$shiny_input_values
        new_input_values <- shiny::reactiveValuesToList(session$input)

        if (NROW(new_input_values) != 0) {
          names <- unique(c(names(old_input_values), names(new_input_values)))
          names <- setdiff(names, excluded_inputs)
          for (name in names) {
            old <- old_input_values[name]
            new <- new_input_values[name]
            if (!identical(old, new)) {

              if (name %in% navigation_inputs) {
                self$log_navigation_manual(name, new[[name]], session)
                next
              }

              log_value <- NULL
              if (isTRUE(track_values)) {
                log_value <- new[[name]]
              }

              self$log_input_manual(name, log_value, session)
            }
          }
        }

        session$userData$shiny_input_values <- new_input_values
        input_values <- new_input_values
      })
    },

    .log_input = function(
      input_id,
      track_value = FALSE,
      matching_values = NULL,
      input_type = "text",
      event_type = "input",
      session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(input_id)
      checkmate::assert_flag(track_value)

      checkmate::assert(
        .combine = "or",
        checkmate::check_atomic_vector(matching_values),
        checkmate::check_null(matching_values)
      )

      checkmate::assert_choice(input_type, c("text", "json"))
      checkmate::assert_choice(event_type, c("input", "navigation"))

      checkmate::assert(
        .combine = "or",
        checkmate::check_r6(session, "ShinySession"),
        checkmate::check_class(session, "session_proxy")
      )

      input <- session$input

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
              event_type = event_type,
              session = session
            ))
          }

          if (
            is.null(matching_values) ||
              (!is.null(matching_values) && input_value %in% matching_values)
          ) {

            logger::log_debug(
              "Writing '{event_type}' event with id: '{input_id}'",
              namespace = "shiny.telemetry"
            )

            private$.log_event(
              type = event_type,
              details = list(id = input_id),
              session = session
            )
          }

        },
        # Options to observeEvent call
        priority = -1, ignoreInit = TRUE
      )
    },

    .log_event = function(
      type = NULL, details = NULL, session = NULL
    ) {
      private$log_generic(
        type = type,
        details = details,
        session = session
      )
    },

    log_input_value = function(
      input_id, input_value, matching_values, input_type, session, event_type
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
        is.null(matching_values) ||
          (!is.null(matching_values) && input_value %in% matching_values)
      ) {
        # save each value separately (if more than 1)
        n_values <- length(input_value)
        input_ids <- input_id
        if (n_values > 1) {
          input_ids <- glue::glue("{input_id}_{seq(1, n_values)}")
        }

        logger::log_debug(
          "Writing '{event_type}' event with ",
          "id: '{input_id}' and value: ",
          "'{jsonlite::toJSON(input_value, auto_unbox = TRUE)}'",
          namespace = "shiny.telemetry"
        )

        purrr::walk2(
          input_ids,
          input_value,
          ~ self$log_custom_event(
            event_type, list(id = .x, value = .y), session
          )
        )
      }
    },
    extract_cookie = function(cookie_string, cookie_name = "shiny_user_cookie") {
      hash <- NULL
      if (!is.null(cookie_string)) {
        cookies <- strsplit(cookie_string, ";")[[1]]
        cookies <- trimws(cookies)
        for (cookie in cookies) {
          parts <- strsplit(cookie, "=")[[1]]
          if (length(parts) == 2 && parts[1] == cookie_name) {
            # Check if the value looks like a SHA256 hash
            cookie_value <- parts[2]
            if (grepl("^[a-f0-9]{64}$", cookie_value)) {
              hash <- cookie_value
              break
            }
          }
        }
      }
      return(hash)
    },
    get_user = function(
      session = shiny::getDefaultReactiveDomain(),
      force_username = NULL,
      track_anonymous_user = TRUE
    ) {
      if (!is.null(force_username)) return(force_username)
      if (isFALSE(is.null(session)) && isFALSE(is.null(session$user))) {
        return(session$user) # POSIT Connect
      } else if (nzchar(Sys.getenv("SHINYPROXY_USERNAME"))) {
        return(Sys.getenv("SHINYPROXY_USERNAME"))
      } else if (track_anonymous_user) {
        cookie_value <- private$extract_cookie(cookie_string = session$request$HTTP_COOKIE)
        # cookie_value will be NULL if either not found or not generated using SHA256 algorithm.
        if (is.null(cookie_value)) {
          cookie_string <- paste0(session$request$HTTP_USER_AGENT,
                                  session$request$REMOTE_ADDR, Sys.time())
          cookie_value <- digest::digest(cookie_string, algo = "sha256")
          session$sendCustomMessage("setUserCookie", list(
            cookieName = "shiny_user_cookie",
            cookieValue = cookie_value,
            expiryInDays = 365
          ))
        }
        return(cookie_value)
      } else {
        return(NULL)
      }
    }
  )
)
