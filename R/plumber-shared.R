
#' Builds hash for a call
#'
#' Function that takes creates a signature for the `values` using a secret.
#'
#'
#' This is used in shiny.telemetry, but also externally with the Plumber
#' endpoint.
#'
#' @param values R object that is going to be signed
#' @param secret string that contains the shared secret to sign the
#' communication. It can be NULL on both telemetry and in plumber API to
#' disable this communication feature
#'
#' @return a string that contains an hash to uniquely identify the parameters
#' @export
#'
#' @examples
#' build_token(values = list(list(1, 2, 3), 2, 2, 3, "bb"))
#' build_token(values = list(list(1, 2, 3), 1, 2, 3, "bb"))
#' build_token(values = list(list(1, 2, 3), 1, 2, 3, "bb"), secret = "abc")
#' build_token(values = list(list(1, 2, 3), 1, 2, 3, "bb"), secret = "abd")
build_token <- function(values, secret = NULL) {
  checkmate::assert_string(secret, null.ok = TRUE)

  digest::digest(list(values, secret), algo = "sha256")
}

#' Builds id from a secret that can be used in open communication
#'
#' This is used in shiny.telemetry, but also externally with the Plumber
#' endpoint.
#'
#' @param secret string that contains information that should not be publicly
#' available
#'
#' @return a string with an hash of the secret
#' @export
#'
#' @examples
#' build_id_from_secret("some_random_secret_generated_with_uuid::UUIDgenerate")
build_id_from_secret <- function(secret) {
  digest::digest(secret, algo = "sha256") %>%
    substr(start = 1, stop = 8)
}
