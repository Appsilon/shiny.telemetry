# REMOVE ME!!
logger::log_threshold("DEBUG", namespace = "shiny.telemetry")

#' Telemetry class to manage telemetry at a global level (outside a session)
#'
#' @description
#'
#'
#' @export
#' @examples
#' telemetry <- Telemetry$new()
#'
#' mock_session <- list(clientData = list(url_search = ""))
#' ts <- telemetry$start_session(mock_session, track_inputs = FALSE, logout = FALSE)
#'
#' telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |> tail()
Telemetry <- R6::R6Class( # nolint object_name_linter
  classname = "Telemetry",
  public = list(

    data_storage = NULL,

    #' @description
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
      self$data_storage <- data_storage
      private$.name <- name
      private$.version <- version
    },

    #' @description
    #' Setup basic telemetry
    #'
    #' @param session

    start_session = function(
      input,
      track_inputs = TRUE,
      track_values = FALSE,
      login = TRUE,
      logout = TRUE,
      browser_version = TRUE,
      save_in_session = TRUE,
      session = shiny::getDefaultReactiveDomain()
    ) {

      username <- shiny::isolate(
        shiny::parseQueryString(session$clientData$url_search)$username
      )
      if (is.null(username)) username <- "unknown_user"

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

      if (isTRUE(save_in_session)) {
        session$userData$telemetry <- self
      }

      invisible(self)
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

    log_login = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      logger::log_debug("login", namespace = "shiny.telemetry")

      private$log_action(
        action = "login user",
        value = username,
        session = session
      )
    },

    #' @description
    #' Log when session ends
    #'

    log_logout = function(
      username = NULL, session = shiny::getDefaultReactiveDomain()
    ) {
      shiny::onSessionEnded(function() {
        logger::log_debug("logout", namespace = "shiny.telemetry")

        private$log_action(
          action = "logout user",
          value = username,
          session = session
        )

        # TODO: remove me

        print(
          telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |>
            tail()
        )

        # private$.telemetry$data_storage$close()
      }, session)
    },

    #' @description
    #' Log an action click
    #'

    log_click = function(id, session = shiny::getDefaultReactiveDomain()) {
      checkmate::assert_string(id)

      logger::log_debug("click: {id}", namespace = "shiny.telemetry")

      private$log_action(
        action = "click",
        id = id,
        session = session
      )
    },

    #' @description
    #' A short description...
    #'

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

        private$log_action(
          action = "browser",
          value = browser,
          session = session
        )
      })
    },

    #' @description
    #' A short description...
    #'

    log_session_details = function(
      detail, session = shiny::getDefaultReactiveDomain()
    ) {
      checkmate::assert_string(detail)

      logger::log_debug("session_details: {detail}", namespace = "shiny.telemetry")

      private$log_session(
        value = detail,
        session = session
      )
    },

    #' @description
    #' A short description...
    #'

    log_button = function(
      input, input_id, session = shiny::getDefaultReactiveDomain()
    ) {
      self$log_input(input, input_id, session = session)
    },

    #' @description
    #' A short description...
    #'

    log_all_inputs = function(
      input,
      track_values = FALSE,
      excluded_inputs = c("browser_version"),
      session = shiny::getDefaultReactiveDomain()
    ) {
      input_values <- shiny::isolate(shiny::reactiveValuesToList(input))

      logger::log_debug(logger::skip_formatter(
        paste(
          "Default Shiny inputs initialized:",
          as.character(jsonlite::toJSON(input_values, auto_unbox = TRUE))
        )),
        namespace = "shiny.telemetry"
      )

      shiny::observe({
        old_input_values <- input_values
        new_input_values <- shiny::reactiveValuesToList(input)
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
              private$log_action(
                action = "input",
                id = name,
                value = new[[name]],
                session = session
              )
              next
            }
            private$log_action(
              action = "input",
              id = name,
              session = session
            )
          }
        }
        input_values <- new_input_values
      })
    },

    #' @description
    #' A short description...
    #'

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
      checkmate::assert_character(matching_values)
      checkmate::assert_string(input_type)
      checkmate::assert_choice(input_type, c("text", "json"))


      shiny::observeEvent(
        input[[input_id]],
        {
          input_value <- input[[input_id]]

          if (isTRUE(track_value)) {
            return(private$log_input_value(
              input_id, input_value, matching_values, input_type
            ))
          }

          private$log_action(
            action = "input", id = input_id, session = session
          )

        },
        # Options to observeEvent call
        priority = -1, ignoreInit = TRUE
      )
    }

  ),
  active = list(
    dashboard = function() private$.name,
    version = function() private$.version
  ),
  private = list(
    track_all_inputs_flag = FALSE,
    .name = NULL,
    .version = NULL,

    # Methods

    log_generic = function(
      action = NULL,
      id = NULL,
      value = NULL,
      session = NULL,
      add_username = TRUE,
      use_detail = FALSE,
      bucket = NULL
    ) {
      checkmate::assert_string(action)
      checkmate::assert_string(id, null.ok = TRUE)
      checkmate::assert(
        .combine = "or",
        checkmate::check_string(value, null.ok = TRUE),
        checkmate::check_list(value, null.ok = TRUE)
      )
      checkmate::assert_r6(session, classes = "ShinySession", null.ok = TRUE)

      session_token <- NULL
      if (!is.null(session)) {
        session_token <- session$token
      }

      if (checkmate::test_list(value)) {
        value <- jsonlite::toJSON(value)
      }

      values <- list(
        session = session_token,
        dashboard = self$dashboard,
        version = self$version,
        action = action,
        id = id
      )

      if (isTRUE(add_username)) {
        if (!is.null(session)) {
          username <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search)$username)
          if (is.null(username)) username <- "unknownUser"
          shiny::req(username)
          values$username <- username
        }
      }

      if (isTRUE(use_detail)) {
        values$detail <- value
      } else {
        values$value <- value
      }

      self$data_storage$insert(
        values = values,
        bucket = bucket
      )
    },

    log_action = function(
      action = NULL, id = NULL, value = NULL, session = NULL
    ) {
      private$log_generic(
        action = action,
        id = id,
        value = value,
        session = session,
        add_username = TRUE,
        bucket = self$data_storage$action_bucket
      )
    },

    log_session = function(
      action = NULL, id = NULL, value = NULL, session = NULL
    ) {
      private$log_generic(
        action = action,
        id = id,
        value = value,
        session = session,
        add_username = FALSE,
        bucket = self$data_storage$session_bucket
      )
    },

    log_input_value = function(
      input_id, input_value, matching_values, input_type
    ) {
      if (is.null(input_value)) {
        return(NULL)
      }

      if (is.logical(input_value)) {
        input_value <- as.character(input_value)
      }

      if (!is.null(matching_values) && input_type == "json") {
        input_value <- parse_val(input_value)
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
          ~ private$log_action(
            values = list(action = "input", id = .y, value = .x)
          )
        )
      }
    }
  )
)
