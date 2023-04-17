loaded_mods <- loadNamespace("box")$loaded_mods
rm(list = ls(loaded_mods), envir = loaded_mods)

plumber::pr_run(plumber::pr(file = "api/main.R"), port = 8087)
