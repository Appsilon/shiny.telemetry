# define function hot to get username
get_user <- function(session) {
  username <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search)$username)
  if (is.null(username)) username <- "unknownUser"
  shiny::req(username)
  return(username)
}

get_users_per_day <- function(log_data) {
  log_data %>%
    dplyr::select("date", "username") %>%
    dplyr::distinct() %>%
    dplyr::select("date") %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(users = dplyr::n())
}

get_sessions_per_day <- function(log_data) {
  log_data %>%
    dplyr::select("date", "session") %>%
    dplyr::distinct() %>%
    dplyr::select("date") %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(sessions = dplyr::n())
}

get_time_per_day <- function(log_data) {
  log_data %>%
    dplyr::mutate(time = as.POSIXct(.data$time)) %>%
    dplyr::group_by(.data$date, .data$session) %>%
    dplyr::summarise(
      time = round(
        as.numeric(max(.data$time) - min(.data$time), units = "hours"),
        2
      )
    ) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(time = mean(.data$time))
}

get_actions_per_day <- function(log_data) {
  log_data %>%
    dplyr::filter(!(.data$action %in% c("login", "logout"))) %>%
    dplyr::select("date", "action") %>%
    dplyr::select("date") %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(action = dplyr::n())
}

get_per_day_data <- function(
    users_per_day_data, sessions_per_day, time_per_day, actions_per_day
) {
  users_per_day_data %>%
    dplyr::full_join(sessions_per_day, by = "date") %>%
    dplyr::full_join(time_per_day, by = "date") %>%
    dplyr::full_join(actions_per_day, by = "date")
}

get_time_daily <- function(log_data) {
  log_data %>%
    dplyr::mutate(time = as.POSIXct(.data$time)) %>%
    dplyr::group_by(.data$session, .data$date) %>%
    dplyr::summarise(
      time_spent = difftime(max(.data$time), min(.data$time), units = "secs")
    )
}

get_active_users <- function(log_data) {
  log_data %>%
    dplyr::select("time", "username", "date") %>%
    dplyr::mutate(day_hour = convert_hour(.data$time)) %>%
    dplyr::group_by(.data$date, .data$day_hour) %>%
    dplyr::summarise(users = length(unique(.data$username))) %>%
    dplyr::arrange(.data$date)
}

get_per_day_plot_data <- function(base, per_day) {
  dplyr::left_join(base, per_day, by = "date") %>%
    tidyr::pivot_longer(
      c(-"date"), names_to = "statistic", values_to = "value"
    ) %>%
    dplyr::arrange(.data$date, .data$statistic, .data$value) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::mutate(id = dplyr::case_when(
      statistic == "users" ~ 3L,
      statistic == "actions" ~ 1L,
      statistic == "sessions" ~ 1L,
      statistic == "time" ~ 2L
    )) %>%
    dplyr::mutate(statistic = dplyr::case_when(
      statistic == "users" ~ "logged users (unique)",
      statistic == "actions" ~ "total clicks and inputs",
      statistic == "sessions" ~ "total opened sessions",
      statistic == "time" ~ "avg session time (hours)"
    ))
}
