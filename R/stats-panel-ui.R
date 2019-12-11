segment <- function(title, ..., color = "blue") {
  div(
    class = "ui raised segment", style = "margin-bottom: 0.5em; width: 100%;",
    tags$div(style = "margin-bottom: 0.5em;", class = paste("ui demo right ribbon label", color), title),
    ...
  )
}

convert_hour <- function(time) {
  hour <- as.POSIXlt(time)$hour
  ifelse(hour == 0, "12am", ifelse(hour == 12, "12pm", ifelse(hour < 12, paste0(hour, "am"), paste0(hour - 12, "pm"))))
}

convert_timediff_to_HM <- function(timediff_in_seconds) {
  days <- (timediff_in_seconds / (60 * 60 * 24)) %>% floor()
  seconds_in_posixct <- .POSIXct(timediff_in_seconds, tz = "GMT")
  S <- as.numeric(format(seconds_in_posixct, "%S"))
  M <- as.numeric(format(seconds_in_posixct, "%M"))
  H <- as.numeric(format(seconds_in_posixct, "%H")) + 24 * days
  if (H > 0) {
    sprintf("%sh %sm", H, M)
  } else {
    sprintf("%sm %ss", M, S)
  }
}

format_time_diff <- function(time_end, time_start) {
  convert_timediff_to_HM(difftime(time_end, time_start, units = "secs"))
}

convert_char_HM_timediff_to_timediff <- function(time_char) {
  as.difftime(time_char, format = "%Hh:%Mm", units = "secs")
}

prepare_color_scale <- function(values, palette) {
  vals <- scales::rescale(min(values):max(values))
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric(palette, domain = NULL)(vals)
  setNames(data.frame(vals[o], cols[o]), NULL)
}

prepare_date_axis_ticks <- function(date_sequence, quantile = 0.1) {
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  date_range <- sort(date_sequence)
  n <- length(date_range)
  tickvals <- date_range[unique(c(seq(1, n, by = max(1, floor(quantile * n))), n))]
  ticktext <- c(format(tickvals[1], "%b %d<br />%Y"), format(tickvals[-1], "%b %d"))
  Sys.setlocale("LC_TIME", lct)
  list(tickvals = tickvals, ticktext = ticktext)
}

render_download_button <- function(output_id, label, style = NULL) {
  a(
    id = output_id,
    class = "ui grey tiny basic button shiny-download-link",
    style = style,
    href = "",
    target = "_blank",
    download = NA,
    label
  )
}

adminUI <- function() {

  # Dashboard header carrying the title of the dashboard
  header <- semantic.dashboard::dashboardHeader(
    tags$div(
      class = "right menu",
      tags$a(class = "icon item", href = "../../logout", semantic.dashboard::icon("power off"))
    )
  )
  # Sidebar content of the dashboard
  sidebar <- semantic.dashboard::dashboardSidebar(
    size = "",
    semantic.dashboard::sidebarMenu(
      semantic.dashboard::menuItem(text = "General stats", tabName = "general", icon = semantic.dashboard::icon("bar chart")),
      semantic.dashboard::menuItem(text = "Activity stats", tabName = "input_part", icon = semantic.dashboard::icon("laptop")),
      semantic.dashboard::menuItem(text = "User specific stats", tabName = "user", icon = semantic.dashboard::icon("user outline")),
      semantic.dashboard::menuItem(text = "Sessions", tabName = "sessions", icon = semantic.dashboard::icon("history")),
      semantic.dashboard::menuItem(
        text = HTML(paste("Tools", semantic.dashboard::icon("wrench"))),
        semantic.dashboard::menuSubItem(
          text = HTML(paste("Date window", semantic.dashboard::icon("calendar"))),
          uiOutput("filters", style = "padding-left: 2em;")
        ),
        semantic.dashboard::menuSubItem(
          text = HTML(paste("Data download", semantic.dashboard::icon("download"))),
          render_download_button("download_data", "Download", style = "margin-left: 2em;")
        )
      )
    )
  )

  final_stats <- segment(
    title = "General app stats",
    div(
      class = "ui grid",
      div(class = "row", uiOutput("date_header")),
      div(
        class = "four column row",
        semantic.dashboard::valueBoxOutput("total_users", 4),
        semantic.dashboard::valueBoxOutput("total_sessions", 4),
        semantic.dashboard::valueBoxOutput("total_time", 4),
        semantic.dashboard::valueBoxOutput("total_days", 4)
      )
    )
  )

  stats_daily <- segment(
    title = "Statistics daily",
    plotly::plotlyOutput("daily_stats", height = "600px")
  )

  global_user_stats <- segment(
    title = "General users stats",
    plotly::plotlyOutput("users_general"),
    plotly::plotlyOutput("users_per_hour")
  )

  user_specific_stats <- segment(
    title = "User specific stats",
    div(class = "ui segment", tags$h3("Select desired user: "), uiOutput("selected_user")),
    uiOutput("selected_user_stats")
  )

  activity_global_stats <- segment(
    title = "Global activity stats",
    div(
      class = "ui grid",
      div(
        style = "margin-top: 2em;", class = "ui row",
        semantic.dashboard::valueBoxOutput("total_inputs", width = 8),
        semantic.dashboard::valueBoxOutput("total_clicks", width = 8)
      ),
      div(class = "ui row", plotly::plotlyOutput("global_action_plot", height = "200px"))
    )
  )

  stats_per_action <- segment(
    title = "Stats per action",
    tags$h3("Select desired action: "),
    uiOutput("select_action"),
    uiOutput("action_stats", style = "margin-top: 2em;")
  )

  stats_per_id <- uiOutput("action_id_stats", style = "width: 100%; padding: 0;")

  global_session_stats <- segment(
    title = "Sessions timeline",
    tags$h3("Sessions duration over time", style = "margin-top:0;"),
    timevis::timevisOutput("sessions_general")
  )

  session_specific_stats <- segment(
    title = "Sessions details",
    tags$h3("Summary per session", style = "margin-top:0;"),
    tags$h4("Select row to display more details", style = "margin-top:0; color: grey; font-style: italic;"),
    DT::dataTableOutput("sessions_table"),
    timevis::timevisOutput("session_actions")
  )

  # combine the two fluid rows to make the body
  body <- semantic.dashboard::dashboardBody(
    tags$head(
      tags$style(
        HTML(".shiny-output-error-validation {color: white;} .ui.statistic > .value {text-transform: none !important}")
      )
    ),
    semantic.dashboard::tabItems(
      semantic.dashboard::tabItem(tabName = "general", final_stats, stats_daily),
      semantic.dashboard::tabItem(tabName = "input_part", activity_global_stats, stats_per_action, stats_per_id),
      semantic.dashboard::tabItem(tabName = "user", global_user_stats, user_specific_stats),
      semantic.dashboard::tabItem(tabName = "sessions", global_session_stats, session_specific_stats)
    )
  )

  semantic.dashboard::dashboardPage(
    title = "App usage statistics",
    header, sidebar, body
  )
}

