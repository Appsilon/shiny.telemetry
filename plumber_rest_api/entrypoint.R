logger::log_info("path here: {fs::path_abs(here::here())}")
logger::log_info("path getwd: {fs::path_abs(getwd())}")
logger::log_info("files here: {paste(list.files(getwd()), collapse = \", \")}")

plumber::plumb(
  file = "app/main.R"
)
