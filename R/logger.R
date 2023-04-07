
# Setup logger namespace
logger::layout_glue_generator(
  format = paste(
    "{crayon::bold(colorize_by_log_level(level, levelr))}",
    "from {namespace}",
    "[{crayon::italic(format(time, \"%Y-%m-%d %H:%M:%S\"))}]",
    "{grayscale_by_log_level(msg, levelr)}"
  )
) %>%
  logger::log_layout(namespace = "shiny.telemetry")
