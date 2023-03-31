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

date_filters <- function() {
  shiny::tagList(
    shiny::tags$div(
      shiny::tags$div(shiny::HTML("From")),
      shiny.semantic::date_input(
        "date_from",
        value = Sys.Date() - 30,
        icon_name = NULL,
        style = "width: 135px;"
      )
    ),
    shiny::tags$div(
      shiny::tags$div(shiny::HTML("To")),
      shiny.semantic::date_input(
        "date_to",
        value = Sys.Date(),
        icon_name = NULL,
        style = "width: 135px;"
      )
    )
  )
}
