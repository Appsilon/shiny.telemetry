box::use(
  uuid[UUIDgenerate],
  jsonlite[serializeJSON, unserializeJSON],
  mockery[stub],
  purrr[set_names],
  stringr[str_replace_all],
  withr[with_envvar],
  shiny.telemetry[build_token],
)

tokens_raw <- UUIDgenerate() |> str_replace_all("-", "")

# need with_envvar to control hash token
with_envvar(
  new = c("SECRET_TOKENS" = tokens_raw),
  box::use(
    app/logic/setup[session_secrets],
    app/logic/token[...],
  )
)

test_that("Tokens work", {
  id <- names(session_secrets)
  values <- list(
    session = UUIDgenerate(), username = "test_user", id = "something",
    value = 12, action = "input"
  )

  token <- build_token(values, secret = id)

  values_2 <- serializeJSON(values) |> unserializeJSON()
  validate_token(values_2, token, id) |>
    expect_true()
})
