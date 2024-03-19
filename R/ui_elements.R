#' Function that adds telemetry HTML elements to UI
#'
#' @param id (optional) string with id representing the namespace
#' @return A `shiny.tag` object to be included in the UI of a Shiny app.
#'
#' @export
use_telemetry <- function(id = "") {
  checkmate::assert_string(id, null.ok = TRUE)
  shiny_namespace <- ""
  if (!is.null(id) && !identical(trimws(id), "")) {
    shiny_namespace <- shiny::NS(trimws(id), "")
  }

  shiny::addResourcePath(
    "js",
    system.file("www", "js", package = "shiny.telemetry")
  )
  shiny::addResourcePath(
    "js-cookie",
    system.file("www", "external", "js-cookie", package = "shiny.telemetry")
  )

  shiny::singleton(
    shiny::tags$head(
      # Custom script that is embedded with namespace
      shiny::tags$script(
        type = "text/javascript",
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
      ),
      # Additional scripts
      shiny::tags$script(
        src = file.path("js-cookie", "js.cookie.min.js"),
        type = "text/javascript"
      ),
      shiny::tags$script(
        src = file.path("js", "cookie_handler.js"),
        type = "text/javascript"
      )
    )
  )
}
