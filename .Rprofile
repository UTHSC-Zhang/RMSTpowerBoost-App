if (!isTRUE(getOption("rmstpb.profile.initialized", FALSE))) {
  options(rmstpb.profile.initialized = TRUE)

  if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
  }

  if (dir.exists(".git") && !isTRUE(getOption("rmstpb.gitpull.ran", FALSE))) {
    options(rmstpb.gitpull.ran = TRUE)
    try({
      message("Running git pull...")
      result <- if (.Platform$OS.type == "windows") {
        shell("git pull", intern = TRUE)
      } else {
        system("git pull", intern = TRUE)
      }
      cat(result, sep = "\n")
    }, silent = TRUE)
  }
}
