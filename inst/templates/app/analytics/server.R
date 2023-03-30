function(input, output, session) {
  session$user <- get_user(session)

  getOption("data_storage")

  prepare_admin_panel_components(input, output, session, data_storage)
}
