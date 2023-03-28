loaded_mods <- loadNamespace("box")$loaded_mods
rm(list = ls(loaded_mods), envir = loaded_mods)

plumber::pr(
  file = "app/main.R"
) |>
  plumber::pr_run(port = 8087)
