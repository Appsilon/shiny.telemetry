#' Log user activity
#'
#' @description These functions connects to database and log specific user
#' activity. See more in \strong{Details} section.
#'
#' @details \code{log_input} and \code{log_button} observe selected input value
#' and registers its insertion or change inside specified database.
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#' @param input input object inherited from server function.
#' @param input_id id of registered input control.
#' @param matching_values An object specified possible values to register.
#' @param input_type 'text' to registered bare input value, 'json' to parse
#' value from JSON format.
#'
#' @export
log_input <- function(
  data_storage, input, input_id, matching_values = NULL, input_type = "text"
) {
  shiny::observeEvent(input[[input_id]], {
    input_value <- input[[input_id]]
    if (is.logical(input_value)) {
      input_value <- as.character(input_value)
    }

    if (is.null(input_value)) {
      return(NULL)
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
        ~ data_storage$insert(
           values = list(action = "input", id = .y, value = .x),
           bucket = data_storage$action_bucket
        )
      )
    }
  },
    # Options to observeEvent call
    priority = -1, ignoreInit = TRUE
  )
}

#' @rdname log_input
#' @param button_id id of registered button input control.
#' @export
log_button <- function(data_storage, input, button_id) {
  shiny::observeEvent(
    input[[button_id]],
    {
      data_storage$insert(
        list(action = "click", id = button_id),
        data_storage$action_bucket
      )
    },
    # Options to observeEvent call
    priority = -1, ignoreInit = TRUE
  )
}

#' @details Each function (except \code{log_custom_action}) store logs inside
#' 'user_log' table. It is required to build admin panel
#' (See \link{prepare_admin_panel_components}).
#'
#' @param values Named list. Names of the list specify column names of
#' \code{bucket} and list elements corresponding values that should be
#' inserted. Columns 'time' and 'session' are filled automatically so
#' you cannot pass it on you own.
#'
#' @rdname log_input
#'
#' @export
log_custom_action <- function(data_storage, values) {
  data_storage$insert(values, bucket = data_storage$action_bucket)
}

#' @param values Named list. Names of the list specify column names of
#' \code{bucket} and list elements corresponding values that should be
#' inserted. Columns 'time' and 'session' are filled automatically so
#' you cannot pass it on you own.
#'
#' @rdname log_input
#'
#' @export
log_custom_session <- function(data_storage, values) {
  data_storage$insert(
    values, bucket = data_storage$session_bucket, add_username = FALSE
  )
}

#' @rdname log_input
#' @param action Specified action value that should be added to 'user_log'
#' bucket.
#' @export
log_action <- function(data_storage, action) {
  data_storage$insert(
    values = list("action" = action),
    bucket = data_storage$action_bucket
  )
}

#' @rdname log_input
#'
#' @param id Id of clicked button.
#' @export
log_click <- function(data_storage, id) {
  data_storage$insert(
    values = list(
      "action" = "click", "id" = id),
      bucket = data_storage$action_bucket
  )
}

#' @rdname log_input
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#' @export
log_login <- function(data_storage) {
  data_storage$insert(
    values = list("action" = "login"),
    bucket = data_storage$action_bucket
  )
}

#' @details \code{log_logout} should be used inside \code{observe} function.
#' It is based on \code{shiny::onStop}.
#' @rdname log_input
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @export
log_logout <- function(data_storage) {
  shiny::onStop(function() {
    data_storage$insert(
      values = list("action" = "logout"), bucket = data_storage$action_bucket
    )
    data_storage$close()
  })
}

#' @rdname log_input
#'
#' @param detail Information that should describe session.
#' @export
log_session_detail <- function(data_storage, detail) {
  data_storage$insert(
    values = list("detail" = detail),
    bucket = data_storage$session_bucket,
    add_username = FALSE
  )
}

#' Browser info
#'
#' @description It sends info about user's browser to server.
#' Place it inside head tag of your Shiny app. You can get this value on server
#' from \code{input[["browser_version"]]}.
#' You can also use log_browser_version function to log browser version into
#' sqlite file.
#'
#' @param id string with namespace for use within moduleServer
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' library(shiny)
#' library(shiny.semantic)
#' library(shiny.telemetry)
#'
#' ui <- function() {
#'   shinyUI(
#'     semanticPage(
#'       tags$head(shiny.telemetry::browser_info_js),
#'       title = "Browser info example",
#'       textOutput("browser")
#'     )
#'   )
#' }
#'
#' server <- shinyServer(function(input, output) {
#'   output$browser <- renderText(input[["browser_version"]])
#' })
#'
#' shinyApp(ui = ui(), server = server)
#' }
#' @export
browser_info_js <- function(id = "") {
  checkmate::assert_string(id, null.ok = TRUE)
  shiny_namespace <- ""
  if (id != "" && !is.null(id)) {
    shiny_namespace <- glue::glue("{id}-")
  }

  shiny::tags$script(type = "text/javascript",
    shiny::HTML(paste0(
    "
  $(document).on('shiny:sessioninitialized', function(event) {
    var br_ver = (function(){
      var ua= navigator.userAgent, tem, M;
      M = ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
      if (/trident/i.test(M[1])) {
        tem = /\\brv[ :]+(\\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
      }
      if (M[1]=== 'Chrome') {
        tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
      }
      M = M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
      if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
      return M.join(' ');
    })();
    ",
    glue::glue("Shiny.setInputValue(\"{shiny_namespace}browser_version\", br_ver);"),
    "
  });
    "
    ))
  )
}


#' @rdname browser_info_js
#'
#' @param data_storage data_storage instance that will handle all backend read
#' @param input input object inherited from server function.
#' and writes.
#'
#' @export
log_browser_version <- function(data_storage, input) {
  shiny::observeEvent(input$browser_version, {
    browser <- input$browser_version
    shiny::validate(
      shiny::need(browser, "'browser_info_js' should be set in app head")
    )
    data_storage$insert(
      values = list("action" = "browser", "value" = browser),
      bucket = data_storage$action_bucket
    )
  })
}
