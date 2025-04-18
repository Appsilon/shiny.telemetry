analytics_ui <- function() {
  # Dashboard header carrying the title of the dashboard
  header <- semantic.dashboard::dashboardHeader(
    shinyjs::useShinyjs(),
    shiny::suppressDependencies("bootstrap"),
    shiny::suppressDependencies("plotlyjs"),
    style = "min-height: 100%;",
    shiny::includeCSS(
      system.file(
        "examples", "app", "analytics", "www", "styles.css", package = "shiny.telemetry"
      )
    ),
    shiny::tags$head(
      shiny::tags$script(src = "https://cdn.plot.ly/plotly-1.20.2.min.js"),
      shiny::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.4/numeral.min.js"
      )
    ),
    shiny::tags$div(
      class = "right menu",
      shiny::tags$a(
        class = "icon item", href = "../../logout",
        semantic.dashboard::icon("power off")
      )
    )
  )

  # Sidebar content of the dashboard
  sidebar <- semantic.dashboard::dashboardSidebar(
    size = "",
    semantic.dashboard::sidebarMenu(
      semantic.dashboard::menuItem(
        text = "General stats",
        tabName = "general",
        icon = semantic.dashboard::icon("bar chart")
      ),
      semantic.dashboard::menuItem(
        text = "Activity stats",
        tabName = "input_part",
        icon = semantic.dashboard::icon("laptop")
      ),
      semantic.dashboard::menuItem(
        text = "User specific stats",
        tabName = "user",
        icon = semantic.dashboard::icon("user outline")
      ),
      semantic.dashboard::menuItem(
        text = "Sessions",
        tabName = "sessions",
        icon = semantic.dashboard::icon("history")
      ),
      semantic.dashboard::menuItem(
        text = shiny::HTML(paste("Tools", semantic.dashboard::icon("wrench"))),
        semantic.dashboard::menuSubItem(
          text = shiny::HTML(paste(
            "Date window", semantic.dashboard::icon("calendar")
          )),
          shiny::uiOutput("filters", style = "padding-left: 2em;")
        ),
        semantic.dashboard::menuSubItem(
          text = shiny::HTML(paste(
            "Data download", semantic.dashboard::icon("download")
          )),
          render_download_button(
            "download_data", "Download", style = "margin-left: 2em;"
          )
        )
      ),
      semantic.dashboard::menuItem(
        shiny.semantic::selectInput(
          inputId = "app_name",
          label = shiny::tags$label("Application name", shiny.semantic::icon("laptop")),
          choices = character()
        )
      )
    )
  )

  final_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "General app stats",
    shiny::div(
      class = "ui grid",
      shiny::div(class = "row", shiny::uiOutput("date_header")),
      shiny::div(
        class = "five column row",
        semantic.dashboard::valueBoxOutput("total_users", 4),
        semantic.dashboard::valueBoxOutput("total_sessions", 4),
        semantic.dashboard::valueBoxOutput("total_time", 4),
        semantic.dashboard::valueBoxOutput("total_days", 4)
      )
    )
  )

  stats_daily <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "Statistics daily",
    plotly::plotlyOutput("daily_stats", height = "600px")
  )

  global_user_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "General users stats",
    plotly::plotlyOutput("users_general"),
    plotly::plotlyOutput("users_per_hour")
  )

  user_specific_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "User specific stats",
    shiny::div(
      class = "ui segment",
      shiny::tags$h3("Select desired user: "),
      shiny::uiOutput("selected_user")
    ),
    shiny::uiOutput("selected_user_stats")
  )

  activity_global_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "Global activity stats",
    shiny::div(
      class = "ui grid",
      shiny::div(
        style = "margin-top: 2em;", class = "ui row",
        semantic.dashboard::valueBoxOutput("total_inputs", width = 8),
        semantic.dashboard::valueBoxOutput("total_navigations", width = 8)
      ),
      shiny::div(
        class = "ui row",
        plotly::plotlyOutput("global_action_plot", height = "200px")
      )
    )
  )

  stats_per_action <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "Stats per action",
    shiny::tags$h3("Select desired action: "),
    shiny::uiOutput("select_action"),
    shiny::uiOutput("action_stats", style = "margin-top: 2em;")
  )

  stats_per_id <- shiny::uiOutput(
    "action_id_stats", style = "width: 100%; padding: 0;"
  )

  global_session_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "Sessions timeline",
    shiny::tags$h3("Sessions duration over time", style = "margin-top:0;"),
    timevis::timevisOutput("sessions_general")
  )

  session_specific_stats <- shiny.semantic::segment(
    class = "centered-and-margined full-width",
    title = "Sessions details",
    shiny::tags$h3("Summary per session", style = "margin-top:0;"),
    shiny::tags$h4(
      "Select row to display more details",
      style = "margin-top:0; color: grey; font-style: italic;"
    ),
    DT::dataTableOutput("sessions_table"),
    timevis::timevisOutput("session_actions")
  )

  # combine the two fluid rows to make the body
  body <- semantic.dashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(paste0(
          ".shiny-output-error-validation {color: white;}",
          " .ui.statistic > .value {text-transform: none !important}"
        ))
      )
    ),
    semantic.dashboard::tabItems(
      semantic.dashboard::tabItem(
        tabName = "general", final_stats, stats_daily
      ),
      semantic.dashboard::tabItem(
        tabName = "input_part",
        activity_global_stats,
        stats_per_action,
        stats_per_id
      ),
      semantic.dashboard::tabItem(
        tabName = "user", global_user_stats, user_specific_stats
      ),
      semantic.dashboard::tabItem(
        tabName = "sessions", global_session_stats, session_specific_stats
      )
    )
  )

  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header,
    sidebar,
    body
  )
}

analytics_server <- function(data_storage) {
  shiny::shinyServer(function(input, output, session) {
    prepare_admin_panel_components(
      input,
      output,
      session,
      data_storage = data_storage
    )
  })
}

#' Run example telemetry analytics dashboard
#'
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @return An object that represents the analytics app. Printing the object or
#' passing it to `shiny::runApp()` will run it.
#'
#' @export
analytics_app <- function(data_storage) {
  shiny::shinyApp(
    ui = analytics_ui(),
    server = analytics_server(data_storage = data_storage)
  )
}
