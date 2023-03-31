
#' TelemetrySession class to manage telemetry within a Shiny session
#'
#' @description
#'
#'
#' @export
#' @examples
#' telemetry <- Telemetry$new()
TelemetryProxy <- R6::R6Class( # nolint object_name_linter
  classname = "TelemetryProxy",

  #
  # Public

  public = list(

    #' @description
    #'
    #'

    initialize = function(telemetry, username = NULL, session_id = NULL) {
      checkmate::assert_r6(telemetry, "Telemetry")
      checkmate::assert_string(username, null.ok = TRUE)
      checkmate::assert_string(session_id, null.ok = TRUE)

      if (is.null(session_id)) {
        session_id <- private$generate_random_id()
      }

      if (is.null(username)) {
        username <- "anonymous"
      }
      private$.telemetry <- telemetry
      private$.username <- username
      private$.session_id <- session_id
    },

    #' @description
    #' Log when session starts
    #'

    log_login = function() {
      logger::log_debug("login", namespace = "shiny.telemetry")

      private$log_action(values = list("action" = "login"))
    },

    #' @description
    #' Log when session ends
    #'

    log_logout = function(session = shiny::getDefaultReactiveDomain()) {
      shiny::onSessionEnded(function() {
        logger::log_debug("logout", namespace = "shiny.telemetry")
        private$log_action(values = list("action" = "logout"))

        # TODO: remove me
        print(telemetry$data_storage$read_user_data("2020-01-01", "2025-01-01") |> tail())

        # private$.telemetry$data_storage$close()
      }, session)
    },

    #' @description
    #' Log an action click
    #'

    log_click = function(id) {
      checkmate::assert_string(id)

      logger::log_debug("click: {id}", namespace = "shiny.telemetry")

      private$log_action(values = list("action" = "click", "id" = id))
    },

    #' @description
    #' A short description...
    #'

    log_browser_version = function(input) {
      shiny::observeEvent(input$browser_version, {
        browser <- input$browser_version
        shiny::validate(
          shiny::need(browser, "'browser_info_js' should be set in app head")
        )
        logger::log_debug(
          "browser_version: {browser}", namespace = "shiny.telemetry"
        )

        private$log_action(
          values = list("action" = "browser", "value" = browser)
        )
      })
    },

    #' @description
    #' A short description...
    #'

    log_session_details = function(detail) {
      checkmate::assert_string(detail)

      logger::log_debug("session_details: {detail}", namespace = "shiny.telemetry")

      private$log_session(list(detail = detail))
    },

    #' @description
    #' A short description...
    #'

    log_button = function(input, input_id) {
      self$log_input(input, input_id)
    },

    #' @description
    #' A short description...
    #'

    log_all_inputs = function(
      input, track_values = FALSE, excluded_inputs = c("browser_version")
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
                values = list(action = "input", id = name, value = new[[name]])
              )
              next
            }
            private$log_action(
              values = list(action = "input", id = name)
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
      input_type = "text"
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

          private$log_action(values = list(action = "input", id = input_id))

        },
        # Options to observeEvent call
        priority = -1, ignoreInit = TRUE
      )
    }

  ),

  #
  # Active

  active = list(

  ),

  #
  # Private

  private = list(

    # Fields

    .session_id = NULL,

    .username = NULL,

    .telemetry = NULL,

    # Methods

    log_action = function(values) {
      checkmate::assert_list(values)

      values$username <- private$.username
      values$session <- private$.session_id
      values$dashboard <- private$.telemetry$dashboard
      values$version <- private$.telemetry$version

      private$.telemetry$data_storage$insert(
        values = values,
        bucket = private$.telemetry$data_storage$action_bucket
      )
    },

    log_session = function(values) {
      checkmate::assert_list(values)

      values$username <- NULL
      values$session <- private$.session_id
      values$dashboard <- private$.telemetry$dashboard
      values$version <- private$.telemetry$version

      private$.telemetry$data_storage$insert(
        values = values,
        bucket = private$.telemetry$data_storage$session_bucket
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
    },

    generate_random_id = function() {
      paste(
        c(
          sample(c(letters, LETTERS, 0:9), 10),
          format(Sys.time(), "%Y%d%m%H%M%S")
        ),
        collapse = ""
      )
    }
  )
)
