date_filters <- function() {
  shiny::tagList(
    shiny::tags$div(
      shiny::tags$div(shiny::HTML("From")),
      shiny.semantic::date_input(
        "date_from",
        value = lubridate::today(tzone = "UTC") - 30,
        icon = NULL,
        style = "width: 135px;"
      )
    ),
    shiny::tags$div(
      shiny::tags$div(shiny::HTML("To")),
      shiny.semantic::date_input(
        "date_to", value = lubridate::today(tzone = "UTC"), icon = NULL, style = "width: 135px;"
      )
    )
  )
}

get_users_per_day <- function(log_data) {
  log_data %>%
    dplyr::select("date", "username", "session") %>%
    dplyr::distinct() %>%
    # Create anonymous or users type
    dplyr::mutate(
      user_type = dplyr::if_else(is.na(.data$username), "anonymous", "users")
    ) %>%
    dplyr::group_by(.data$date, .data$user_type) %>%
    dplyr::summarise(users = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "user_type", values_from = "users") %>%
    # Make sure every day has values for anonymous and users columns
    dplyr::bind_rows(
      dplyr::tibble(anonymous = integer(0), users = integer(0))
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.integer), ~tidyr::replace_na(.x, 0))
    )
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
      time = as.numeric(max(.data$time) - min(.data$time), units = "hours")
    ) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(time = mean(.data$time))
}

get_actions_per_day <- function(log_data) {
  log_data %>%
    dplyr::filter(!(.data$type %in% c("login", "logout"))) %>%
    dplyr::select("date", "type") %>%
    dplyr::select("date") %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(type = dplyr::n())
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
  metadata <- dplyr::tribble(
    ~index,      ~color,    ~id, ~statistic,
    "users",     "#fbbd08",  1L, "logged users (unique)",
    "anonymous", "#b21e1e",  1L, "anonymous users (unique)",
    "type",      "#00827c",  3L, "total navigations and inputs",
    "sessions",  "#1a69a4",  1L, "total opened sessions",
    "time",      "#fa23f3",  2L, "avg session time (hours)"
  )

  dplyr::left_join(base, per_day, by = "date") %>%
    tidyr::pivot_longer(
      c(-"date"), names_to = "index", values_to = "value"
    ) %>%
    dplyr::arrange(.data$date, .data$index, .data$value) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::left_join(metadata, by = "index")
}

#' prepare_admin_panel_components
#'
#' @param input input object inherited from server function.
#' @param output output object inherited from server function.
#' @param session session object inherited from server function.
#' @param data_storage data_storage instance that will handle all backend read
#' and writes.
#'
#' @keywords internal
prepare_admin_panel_components <- function(
  input, output, session, data_storage
) {
  hour_levels <- c("12am", paste0(1:11, "am"), "12pm", paste0(1:11, "pm"))

  log_data <- shiny::reactive({
    data_storage$read_event_data(
      as.Date(input$date_from), as.Date(input$date_to)
    )
  })

  output$filters <- shiny::renderUI(date_filters())

  selected_log_data <- shiny::reactive({
    shiny::validate(
      shiny::need(input$date_from, label = "date_from"),
      shiny::need(input$date_to, label = "date_to")
    )
    log_data()
  })

  output$download_data <- shiny::downloadHandler(
    filename = function() {
      sprintf("data_%s_%s.csv", input$date_from, input$date_to)
    },
    content = function(file) {
      utils::write.csv(selected_log_data(), file)
    }
  )

  output$date_header <- shiny::renderUI({
    shiny::tags$h4(
      class = "ui horizontal divider header",
      sprintf("From %s to %s", input$date_from, input$date_to)
    )
  })
  ## general tab reactives

  date_base <- shiny::reactive({
    shiny::validate(
      shiny::need(input$date_from, label = "date_from"),
      shiny::need(input$date_to, label = "date_to")
    )
    data.frame(date = seq(as.Date(input$date_from), as.Date(input$date_to), by = "day"))
  })

  date_base_with_hours <- shiny::reactive({
    expand.grid(
      date = date_base()$date,
      day_hour = c(paste0(1:12, "am"), paste0(1:12, "pm")),
      stringsAsFactors = FALSE
    )
  })

  plot_daily_stats <- function(plot_data) {
    n_plots <- length(unique(plot_data$id))
    x_axis_ticks <- prepare_date_axis_ticks(unique(plot_data$date))
    annote <- function(y, text) {
      list(
        y = y,
        text = text,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 16)
      )
    }

    plot_data <- plot_data %>%
      dplyr::mutate(
        text = dplyr::if_else(
          .data$index == "time",
          .data$value %>%
            round(digits = 5) %>%
            lubridate::duration(unit = "hours") %>%
            as.character(),
          ""
        ),
        text = dplyr::if_else(
          .data$index == "time",
          glue::glue("<br /><i>(around {.data$text})</i>"),
          ""
        )
      )

    plot_arguments <- plot_data %>%
      dplyr::group_by(.data$id) %>%
      dplyr::group_map(function(x, ...) {
        x %>%
          plotly::plot_ly(
            x = ~date, y = ~value, color = ~statistic, colors = ~color,
            text = ~text,
            hovertemplate = paste(
              "%{label}",
              "<br />Value: %{y}",
              "%{text}"
            )
          ) %>%
          plotly::add_bars()
      })

    plot_arguments$nrows <- n_plots
    plot_arguments$shareX <- TRUE

    do.call(plotly::subplot, plot_arguments) %>%
      plotly::layout(
        legend = list(orientation = "h"),
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        ),
        margin = list(r = 20),
        annotations = list(
          annote(y = 1.03, text = "Unique users / opened sessions"),
          annote(y = 0.65, text = "Average session time [hours]"),
          annote(y = 0.29, text = "Total navigations and inputs")
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  }

  users_per_day <- shiny::reactive({
    get_users_per_day(selected_log_data())
  })

  sessions_per_day <- shiny::reactive({
    get_sessions_per_day(selected_log_data())
  })

  time_per_day <- shiny::reactive({
    get_time_per_day(selected_log_data())
  })

  actions_per_day <- shiny::reactive({
    get_actions_per_day(selected_log_data())
  })

  per_day_data <- shiny::reactive({
    get_per_day_data(
                     users_per_day(),
                     sessions_per_day(),
                     time_per_day(),
                     actions_per_day())
  })

  per_day_plot_data <- shiny::reactive({
    get_per_day_plot_data(date_base(), per_day_data())
  })

  time_daily <- shiny::reactive({
    get_time_daily(selected_log_data())
  })

  output$daily_stats <- plotly::renderPlotly({
    plot_daily_stats(per_day_plot_data())
  })

  shiny::observe({
    if (length(time_daily()) > 0) {
      output$total_time <- semantic.dashboard::renderValueBox({
        time_hours <- time_daily() %>%
          dplyr::pull(.data$time_spent) %>%
          mean() %>%
          convert_timediff_to_hm()

        semantic.dashboard::valueBox(
          value = time_hours,
          subtitle = "Average time spent daily",
          icon = semantic.dashboard::icon("User Circle"),
          color = "yellow",
          width = 16
        )
      })
    } else {
      NULL
    }
  })

  shiny::observe({
    if (nrow(selected_log_data()) > 0) {
      output$total_users <- semantic.dashboard::renderValueBox({
        semantic.dashboard::valueBox(
          value = length(unique(
            selected_log_data() %>%
              dplyr::filter(.data$type == "login") %>%
              dplyr::filter(.data$username != "") %>%
              dplyr::pull(.data$username)
          )),
          subtitle = "Unique users (with logins)",
          icon = semantic.dashboard::icon("User Circle"),
          color = "red",
          width = 16
        )
      })

      output$total_anon <- semantic.dashboard::renderValueBox({
        semantic.dashboard::valueBox(
          value = length(unique(
            selected_log_data() %>%
              dplyr::filter(.data$type == "login") %>%
              dplyr::filter(is.na(.data$username)) %>%
              dplyr::pull(.data$session)
          )),
          subtitle = "Anonymous users",
          icon = semantic.dashboard::icon("User Circle"),
          color = "red",
          width = 16
        )
      })

      output$total_sessions <- semantic.dashboard::renderValueBox({
        semantic.dashboard::valueBox(
          value = length(unique(selected_log_data() %>% dplyr::pull(session))),
          subtitle = "Sessions opened",
          icon = semantic.dashboard::icon("User Circle"),
          color = "blue",
          width = 16
        )
      })

      output$total_days <- semantic.dashboard::renderValueBox({
        semantic.dashboard::valueBox(
          value = length(unique(as.Date(selected_log_data()$time))),
          subtitle = "Days active",
          icon = semantic.dashboard::icon("Calendar"),
          color = "teal",
          width = 16
        )
      })
    } else {
      NULL
    }
  })

  ## users tab reactives

  users_plot_data <- shiny::reactive({
    total_users_per_day <- selected_log_data() %>%
      dplyr::arrange(.data$date) %>%
      dplyr::filter(.data$type == "login") %>%
      dplyr::select("date", "username") %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(users = dplyr::n())

    nested_users_data <- selected_log_data() %>%
      dplyr::filter(.data$type == "login") %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(.data$date) %>%
      dplyr::arrange(.data$date)

    nested_users_data$new_users <- nested_users_data$username %>%
      purrr::accumulate(union) %>%
      purrr::map(length) %>%
      unlist() %>%
      diff() %>%
      purrr::prepend(1)

    nested_users_data %>%
      dplyr::distinct(.data$username, .data$date, .data$new_users) %>%
      dplyr::full_join(total_users_per_day, by = "date") %>%
      dplyr::full_join(date_base(), by = "date") %>%
      tidyr::replace_na(list(users = 0, new_users = 0)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        new_users = sum(.data$new_users),
        users = max(.data$users)
      ) %>%
      dplyr::mutate(previous_users = .data$users - .data$new_users)
  })

  active_users <- shiny::reactive({
    get_active_users(selected_log_data())
  })

  heatmap_data <- shiny::reactive({
    heatmap_temp_data <- dplyr::left_join(
      date_base_with_hours(),
      active_users(),
      by = c("date", "day_hour")
    ) %>%
      tidyr::replace_na(list(users = 0))
    heatmap_temp_data$day_hour <- factor(
      heatmap_temp_data$day_hour,
      levels = hour_levels
    )
    heatmap_temp_data
  })

  output$users_general <- plotly::renderPlotly({
    x_axis_ticks <- prepare_date_axis_ticks(unique(users_plot_data()$date))
    plotly::plot_ly(
      dplyr::arrange(users_plot_data(), .data$date),
      x = ~date, y = ~new_users, color = I("#ff7f0e"),
      name = "New users logged", type = "bar",
      hoverinfo = "text", text = ~paste("New users:", new_users)
    ) %>%
      plotly::add_trace(
        y = ~previous_users, name = "Returning users logged", color = I("#1f77b4"),
        hoverinfo = "text", text = ~paste("Returning users:", previous_users)
      ) %>%
      plotly::layout(
        yaxis = list(title = ""),
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        ),
        title = "Users logged each day", barmode = "stack"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$users_per_hour <- plotly::renderPlotly({
    colz <- prepare_color_scale(heatmap_data()$users, "Blues")
    x_axis_ticks <- prepare_date_axis_ticks(unique(heatmap_data()$date))
    plotly::plot_ly(
      heatmap_data(),
      x = ~date,
      y = ~day_hour,
      z = ~users,
      type = "heatmap",
      colorscale = colz,
      showscale = FALSE,
      hoverinfo = "text",
      text = ~paste(
        "<br />Date:", date,
        "<br />Hour:", day_hour,
        "<br />Users: ", users
      )
    ) %>%
      plotly::layout(
        title = "Total users logged each hour", yaxis = list(title = ""),
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$selected_user <- shiny::renderUI({
    shiny.semantic::search_selection_choices(
      "selected_users",
      sort(unique(selected_log_data()$username)),
      multiple = FALSE, default_text = "Select user"
    )
  })

  selected_user_data <- shiny::reactive({
    shiny::validate(
      shiny::need(input$selected_users, "At least one user must be selected")
    )
    selected_log_data() %>%
      dplyr::filter(.data$username %in% input$selected_users)
  })

  actions_per_users_data <- shiny::reactive({
    temp_user_actions_data <- selected_user_data() %>%
      dplyr::mutate(day_hour = convert_hour(.data$time)) %>%
      dplyr::group_by(.data$date, .data$day_hour) %>%
      dplyr::summarise(actions = dplyr::n())

    temp_user_actions_data <- dplyr::left_join(
      date_base_with_hours(),
      temp_user_actions_data,
      by = c("date", "day_hour")
    ) %>%
      tidyr::replace_na(list(actions = 0))

    temp_user_actions_data$day_hour <- factor(
      temp_user_actions_data$day_hour,
      levels = hour_levels
    )
    temp_user_actions_data
  })

  output$user_actions <- plotly::renderPlotly({
    colz <- prepare_color_scale(actions_per_users_data()$actions, "Blues")
    x_axis_ticks <- prepare_date_axis_ticks(unique(actions_per_users_data()$date))
    plotly::plot_ly(
      actions_per_users_data(),
      x = ~date,
      y = ~day_hour,
      z = ~actions,
      type = "heatmap",
      colorscale = colz,
      showscale = FALSE,
      hoverinfo = "text",
      text = ~paste(
        "<br />Date:", date,
        "<br />Hour:", day_hour,
        "<br />Actions: ", actions
      )
    ) %>%
      plotly::layout(
        yaxis = list(title = ""), title = "Operations performed by user each hour",
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        ),
        margin = list(r = 25, b = 50)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$user_total_time <- semantic.dashboard::renderValueBox({
    total_hours <- selected_user_data() %>%
      dplyr::mutate(time = as.POSIXct(.data$time)) %>%
      dplyr::group_by(.data$session) %>%
      dplyr::summarise(
        time_spent = difftime(max(.data$time), min(.data$time), units = "secs")
      ) %>%
      dplyr::pull(.data$time_spent) %>%
      sum() %>%
      convert_timediff_to_hm()

    semantic.dashboard::valueBox(
      value = total_hours,
      subtitle = "Hours the user spent on app",
      icon = semantic.dashboard::icon("Calendar"),
      color = "blue",
      width = 16
    )
  })

  output$user_active_days <- semantic.dashboard::renderValueBox({
    semantic.dashboard::valueBox(
      value = length(unique(selected_user_data()$date)),
      subtitle = "Days the user logged",
      icon = semantic.dashboard::icon("Calendar"),
      color = "blue",
      width = 16
    )
  })

  output$user_actions_mean <- semantic.dashboard::renderValueBox({
    semantic.dashboard::valueBox(
      value = nrow(selected_user_data()),
      subtitle = "Actions the user executed",
      icon = semantic.dashboard::icon("Calendar"),
      color = "blue",
      width = 16
    )
  })

  output$user_since <- semantic.dashboard::renderInfoBox({
    semantic.dashboard::valueBox(
      value = min(selected_user_data()$date),
      subtitle = "User first login",
      icon = semantic.dashboard::icon("Calendar"),
      color = "blue",
      width = 16
    )
  })

  output$selected_user_stats <- shiny::renderUI({
    shiny::validate(shiny::need(input$selected_users, "selected_users"))
    if (is.null(input$selected_users)) {
      ""
    } else {
      shiny::div(
        class = "ui horizontal segments",
        shiny::div(
          class = "ui segment",
          shiny::div(
            class = "ui grid",
            shiny::div(
              class = "column eleven wide",
              plotly::plotlyOutput("user_actions")
            ),
            shiny::div(
              style = "margin-top: 1.8em;", class = "column five wide",
              shiny::div(
                semantic.dashboard::valueBoxOutput("user_total_time"),
                style = "margin-bottom: 0.5em;"
              ),
              shiny::div(
                semantic.dashboard::valueBoxOutput("user_active_days"),
                style = "margin-bottom: 0.5em;"
              ),
              shiny::div(
                semantic.dashboard::valueBoxOutput("user_actions_mean"),
                style = "margin-bottom: 0.5em;"
              ),
              shiny::div(
                semantic.dashboard::valueBoxOutput("user_since"),
                style = "margin-bottom: 0.5em;"
              )
            )
          )
        )
      )
    }
  })

  # input stats

  global_action_data <- shiny::reactive({
    action_date_base <- expand.grid(
      date = date_base()$date,
      type = c("input", "navigation"),
      stringsAsFactors = FALSE
    )

    selected_log_data() %>%
      dplyr::filter(.data$type %in% c("input", "navigation")) %>%
      dplyr::group_by(.data$type, .data$date) %>%
      dplyr::summarise(times = dplyr::n()) %>%
      (function(.dot) {
        dplyr::left_join(action_date_base, .dot, by = c("type", "date"))
      })() %>%
      tidyr::replace_na(list(times = 0))
  })

  output$global_action_plot <- plotly::renderPlotly({
    colz <- prepare_color_scale(global_action_data()$times, "Blues")
    x_axis_ticks <- prepare_date_axis_ticks(unique(global_action_data()$date))
    plotly::plot_ly(
      global_action_data(),
      x = ~date,
      y = ~type,
      z = ~times,
      type = "heatmap",
      colorscale = colz,
      showscale = FALSE,
      hoverinfo = "text",
      text = ~paste(
        "<br />Date:", date,
        "<br />Event:", type,
        "<br />Amount: ", times
      )
    ) %>%
      plotly::layout(
        title = "Total actions performed each day", yaxis = list(title = ""),
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$total_inputs <- semantic.dashboard::renderValueBox({
    total_inputs_value <- global_action_data() %>%
      dplyr::filter(.data$type == "input") %>%
      dplyr::pull(.data$times) %>%
      sum()

    semantic.dashboard::valueBox(
      value = total_inputs_value,
      subtitle = "Total inputs performed",
      icon = semantic.dashboard::icon("bar chart"),
      color = "purple",
      width = 5
    )
  })

  output$total_navigations <- semantic.dashboard::renderValueBox({
    total_navigations_value <- global_action_data() %>%
      dplyr::filter(.data$type == "navigation") %>%
      dplyr::pull(.data$times) %>%
      sum()

    semantic.dashboard::valueBox(
      value = total_navigations_value,
      subtitle = "Total navigations performed",
      icon = semantic.dashboard::icon("bar chart"),
      color = "purple",
      width = 5
    )
  })

  output$select_action <- shiny::renderUI({
    actions <- selected_log_data() %>%
      dplyr::filter(.data$type %in% c("navigation", "input")) %>%
      dplyr::pull(.data$type) %>%
      unique() %>%
      sort()
    shiny.semantic::search_selection_choices(
      "selected_action",
      actions,
      multiple = FALSE,
      default_text = "..."
    )
  })

  selected_action_data <- shiny::reactive({
    selected_log_data() %>%
      dplyr::filter(.data$type == input$selected_action)
  })

  # s_* = selected_*
  s_action_aggregated_data <- shiny::reactive({
    selected_action_data() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(times_total = dplyr::n())
  })

  output$selected_action_plot <- plotly::renderPlotly({
    id_date_base <- expand.grid(
      date = date_base()$date, id = unique(selected_action_data()$id),
      stringsAsFactors = FALSE
    )
    x_axis_ticks <- prepare_date_axis_ticks(unique(id_date_base$date))

    id_data <- selected_action_data() %>%
      dplyr::group_by(.data$date, .data$id) %>%
      dplyr::summarise(times = dplyr::n()) %>%
      (function(.dot) {
        dplyr::left_join(id_date_base, .dot, by = c("id", "date"))
      })() %>%
      dplyr::left_join(s_action_aggregated_data(), by = "id") %>%
      tidyr::replace_na(list(times = 0)) %>%
      dplyr::mutate(
        input_label = sprintf("%s (total %s)", .data$id, .data$times_total)
      )

    colz <- prepare_color_scale(heatmap_data()$users, "Blues")

    plotly::plot_ly(
      id_data,
      x = ~date,
      y = ~input_label,
      z = ~times,
      type = "heatmap",
      colorscale = colz,
      showscale = FALSE,
      hoverinfo = "text",
      text = ~paste(
        "<br />Date:", date,
        "<br />Input ID:", id,
        "<br />Amount: ", times
      )
    ) %>%
      plotly::layout(
        title = "Actions executed each day", yaxis = list(title = ""),
        xaxis = list(
          title = "", hoverformat = "%b %d",
          tickvals = x_axis_ticks$tickvals, ticktext = x_axis_ticks$ticktext
        ),
        margin = list(l = 150)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$select_action_id <- shiny::renderUI({
    shiny::validate(shiny::need(input$selected_action, "selected_action"))
    if (input$selected_action %in% c("input", "navigation")) {
      shiny.semantic::search_selection_choices(
        "selected_action_id",
        sort(unique(selected_action_data()$id)),
        multiple = FALSE,
        default_text = "Select action id"
      )
    } else {
      ""
    }
  })

  selected_action_id_data <- shiny::reactive({
    shiny::validate(shiny::need(input$selected_action_id, "selected action id"))
    selected_action_data() %>%
      dplyr::filter(.data$id == input$selected_action_id)
  })

  output$action_stats <- shiny::renderUI({
    shiny::validate(shiny::need(input$selected_action, "selected_action"))
    if (is.null(input$selected_action) | input$selected_action == "") {
      ""
    } else {
      shiny::div(
        class = "sixteen wide column",
        plotly::plotlyOutput("selected_action_plot", height = "200px")
      )
    }
  })

  output$input_id_table <- DT::renderDataTable({
    shiny::validate(shiny::need(input$selected_action_id, "selected_action_id"))
    selected_action_id_data() %>%
      dplyr::group_by(.data$value) %>%
      dplyr::summarise(times = dplyr::n()) %>%
      dplyr::rename("Value of selected input" = "value", "Total Amount" = "times")
  },
  rownames = FALSE,
  options = list(
    lengthChange = FALSE,
    searching = TRUE
  )
  )

  output$action_id_stats <- shiny::renderUI({
    shiny::validate(shiny::need(input$selected_action, "selected_action"))
    if (!input$selected_action %in% c("login user", "logout user", "browser")) {
      shiny.semantic::segment(
        title = "Stats per input id",
        shiny::tags$h3("Select input ID:"),
        shiny::uiOutput("select_action_id", style = "margin-bottom: 0.5em;"),
        DT::dataTableOutput("input_id_table")
      )
    } else {
      ""
    }
  })

  # sessions stats

  sessions_data <- shiny::reactive({
    selected_log_data() %>%
      dplyr::select("time", "session", "type") %>%
      dplyr::filter(.data$type %in% c("login", "logout", "input", "navigation")) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$session) %>%
      dplyr::summarise(
        start = as.character(min(.data$time)), end = as.character(max(.data$time)),
        style = "font-size: 0.1em;"
      )
  })

  output$sessions_general <- timevis::renderTimevis({
    timevis::timevis(sessions_data(), options = list(
      start = min(lubridate::as_date(sessions_data()$start)) - 1,
      end = max(lubridate::as_date(sessions_data()$end)) + 1,
      margin = list(item = 0.5)
    ))
  })

  sessions_summary <- shiny::reactive({
    selected_log_data() %>%
      dplyr::group_by(.data$session) %>%
      tidyr::fill("username") %>%
      dplyr::summarise(
        username = unique(.data$username),
        session_start_date = min(.data$time),
        session_duration = round(difftime(max(.data$time), min(.data$time), units = "secs")),
        session_actions = dplyr::n()
      )
  })

  output$sessions_table <- DT::renderDataTable({
    DT::datatable(
      sessions_summary(),
      selection = "single",
      colnames = c("Session ID", "User name", "Init time", "Duration (s)", "Performed actions")
    )
  })

  selected_session <- shiny::reactive({
    shiny::validate(shiny::need(input$sessions_table_rows_selected, label = "select_row"))
    sessions_summary()[input$sessions_table_rows_selected, ]$session
  })

  selected_session_data <- shiny::reactive({
    shiny::validate(shiny::need(selected_session(), label = "selected_session"))
    selected_log_data() %>%
      dplyr::filter(
        .data$type %in% c("login", "logout", "input", "navigation"),
        session == selected_session()
      ) %>%
      dplyr::mutate(
        start = as.character(.data$time),
        content = dplyr::case_when(
          type %in% c("login", "logout") ~ type,
          type == "input" ~ sprintf("Input: %s <br /> Value: %s", id, value),
          type == "navigation" ~ sprintf("Navigated: %s", id)
        ),
        style = "text-align: left;",
        end = NA
      ) %>%
      dplyr::select("start", "content", "time")
  })

  output$session_actions <- timevis::renderTimevis({
    shiny::validate(shiny::need(selected_session_data(), label = "selected_session"))
    timevis::timevis(selected_session_data(), options = list(
      start = min(lubridate::as_date(selected_session_data()$time)) - 1,
      end = max(lubridate::as_date(selected_session_data()$time)) + 1
    ))
  })
}
