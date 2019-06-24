#' @export
shiny_stats_ui <- function(custom_css_path = NULL) {
  shinyUI(
    semanticPage(
      shinyjs::useShinyjs(),
      suppressDependencies("bootstrap"),
      suppressDependencies("plotlyjs"),
      suppressDependencies("semantic-ui"),
      style = "min-height: 100%;",
      tags$head(
        tags$link(rel = "stylesheet", href = custom_css_path),
        tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css"),
        tags$script(src = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js"),
        tags$script(src = "https://cdn.plot.ly/plotly-1.20.2.min.js"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.4/numeral.min.js")
      ),
      uiOutput("admin_page")
    )
  )
}

#' @export
shiny_stats_server <- function(get_user, allow_admin_rule, db_credentials) {
  shinyServer(function(input, output, session) {
    session$user <- get_user(session)
    output$admin_page <- renderUI({
      user_is_authorized <- !is.null(session$user)
      ui <- ""
      if (user_is_authorized) {
        allow_admin_view <- allow_admin_rule(session)
        ui <- div(
          class = "ui middle aligned center aligned grid",
          div(
            class = "ui row",
            uiicon("remove circle"), style = "margin-top: 1em;",
            "Access only for admin!"
          ),
          div(
            class = "ui row",
            tags$a(
              class = "ui labeled icon basic button", href = "../../logout",
              semantic.dashboard::icon("power off"), "Logout"
            )
          )
        )
      }
      if (allow_admin_view) {
        prepare_admin_panel_components(input, output, session, db_credentials)
        ui <- adminUI()
      }
      ui
    })
  })
}
