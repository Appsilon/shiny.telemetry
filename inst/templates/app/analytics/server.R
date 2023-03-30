# Obtain data_storage object from global environment
shiny::getShinyOption("data_storage")

function(input, output, session) {
  session$user <- get_user(session)

  prepare_admin_panel_components(input, output, session, data_storage)
}
