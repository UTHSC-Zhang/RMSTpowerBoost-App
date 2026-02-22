#!/usr/bin/env Rscript

find_repo_root <- function(start_dir) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    has_git <- dir.exists(file.path(current, ".git"))
    has_desc <- file.exists(file.path(current, "DESCRIPTION"))
    if (has_git || has_desc) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find repository root from script location.", call. = FALSE)
    }
    current <- parent
  }
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
repo_root <- find_repo_root(dirname(script_path))
setwd(repo_root)

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required. Install with install.packages('covr').", call. = FALSE)
}

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required. Install with install.packages('xml2').", call. = FALSE)
}

dir.create("coverage", showWarnings = FALSE, recursive = TRUE)
message("Writing coverage artifacts in: ", file.path(getwd(), "coverage"))

cov <- covr::package_coverage(quiet = FALSE)
covr::to_cobertura(cov, filename = "coverage/cobertura.xml")
saveRDS(cov, file = "coverage/coverage.rds")
writeLines(sprintf("percent_coverage=%.2f", covr::percent_coverage(cov)), "coverage/summary.txt")

covr::codecov(coverage = cov, quiet = FALSE)
