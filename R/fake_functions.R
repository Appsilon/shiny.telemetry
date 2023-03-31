# Fake function to allow R CMD CHECK to remove NOTE due to unused imports
#
# They are being used inside the R6 class
fake_function <- function() {
  R6::R6Class
  checkmate::assert_string
  glue::glue
  logger::log_debug
  RSQLite::SQLite
  digest::digest
  httr2::request
  odbc::dbConnect
}
