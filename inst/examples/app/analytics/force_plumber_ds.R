# Connecting to a plumber API data storage backend
data_storage <- DataStoragePlumber$new(
  username = "test_user",
  hostname = Sys.getenv("PLUMBER_HOSTNAME"),
  path = Sys.getenv("PLUMBER_PATH"),
  port = Sys.getenv("PLUMBER_PORT") %>% as.numeric(),
  protocol = Sys.getenv("PLUMBER_PROTOCOL"),
  authorization = Sys.getenv("CONNECT_AUTHORIZATION_KEY"),
  secret = Sys.getenv("PLUMBER_SECRET")
)
