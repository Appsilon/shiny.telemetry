USERS_TABLE_NAME <- "users"

get_role_actions <- function(rolename, roles) {
  role <- purrr::detect(roles, ~ .$name == rolename)
  if (is.null(role)) {
    stop("Asked for role undefined in roles.")
  }
  role$actions
}

get_user_authorized_actions <- function(user, roles) {
  user_roles <- unlist(user$roles)
  unlist(purrr::map(user_roles, ~ get_role_actions(., roles)))
}

username_is_not_unique <- function(users, username) {
  if (is.null(username)) {
    return(FALSE)
  }
  existing_usernames <- purrr::map_chr(users, "username")
  username %in% existing_usernames[duplicated(existing_usernames)]
}

extract_user_credentials <- function(users, username) {
  user <- purrr::detect(users, ~ .$username == username)
  if (is.null(user)) {
    user <- list(username = NA, password = NA, roles = list(NULL))
  }
  user
}

user_is_authorized <- function(username, action, users, roles) {
  user <- extract_user_credentials(users, username)
  if (username_is_not_unique(users, username)) {
    stop("Users should have unique names. ",
         "Please check usernames in your database.")
  }
  if ("roles" %in% names(user)) {
    authorized_for <- get_user_authorized_actions(user, roles)
    (action %in% authorized_for)
  } else {
    stop("Users do not have roles defined, but is_authorized was called. ",
         "Please define roles in users dataframe ",
         "when calling initialize_accounts.")
  }
}

#' Registers a new user in the database.
#'
#' @param connection Database connection.
#' @param username A string representing unique user name.
#' @param password A plain text password that will be encrypted and saved.
#' @param roles A list of roles. Each role is a string representing role name.
#' @param info A named list containing additional information about the user.
#'
#' @export
register_user <- function(connection, username, password, roles = list(), info = list()) {
  encrypted_password <- ifelse(is.na(password), NA, bcrypt::hashpw(password))

  record <- list(
    username = username,
    password = encrypted_password,
    roles = rjson::toJSON(roles),
    info = rjson::toJSON(info)
  )

  odbc::dbWriteTable(connection, USERS_TABLE_NAME, as.data.frame(record, stringsAsFactors = FALSE),
                     overwrite = FALSE, append = TRUE, row.names = FALSE)
}

deserialize_user <- function(user) {
  user <- as.list(user)
  user$roles <- rjson::fromJSON(user$roles)
  user$info <- rjson::fromJSON(user$info)

  return(user)
}

#' Retrieves all users stored in the database.
#'
#' @param connection Database connection.
#'
#' @export
retrieve_users <- function(connection) {
  query <- paste0("SELECT * FROM ", USERS_TABLE_NAME)
  users <- odbc::dbGetQuery(connection, query)

  lapply(seq(along = rownames(users)),
         function(i) deserialize_user(users[i, ]))
}
