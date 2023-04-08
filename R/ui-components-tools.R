segment <- function(title, ..., color = "blue") {
  shiny::div(
    class = "ui raised segment", style = "margin-bottom: 0.5em; width: 100%;",
    shiny::tags$div(
      style = "margin-bottom: 0.5em;",
      class = paste("ui demo right ribbon label", color),
      title
    ),
    ...
  )
}

convert_hour <- function(time) {
  hour <- as.POSIXlt(time)$hour
  ifelse(
    hour == 0, "12am",
    ifelse(
      hour == 12, "12pm",
      ifelse(hour < 12, paste0(hour, "am"), paste0(hour - 12, "pm"))
    )
  )
}

convert_timediff_to_hm <- function(timediff_in_seconds) {
  days <- (timediff_in_seconds / (60 * 60 * 24)) |> floor()
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
  convert_timediff_to_hm(difftime(time_end, time_start, units = "secs"))
}

convert_char_hm_to_timediff <- function(time_char) {
  as.difftime(time_char, format = "%Hh:%Mm", units = "secs")
}

prepare_color_scale <- function(values, palette) {
  vals <- scales::rescale(min(values):max(values))
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric(palette, domain = NULL)(vals)
  stats::setNames(data.frame(vals[o], cols[o]), NULL)
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
  shiny::a(
    id = output_id,
    class = "ui grey tiny basic button shiny-download-link",
    style = style,
    href = "",
    target = "_blank",
    download = NA,
    label
  )
}
