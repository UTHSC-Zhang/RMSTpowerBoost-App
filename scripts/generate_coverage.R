#!/usr/bin/env Rscript

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required. Install it with install.packages('covr').")
}

cov <- covr::package_coverage(
  type = "tests",
  line_exclusions = list("R/generate_app_method_datasets.R" = 1:100000)
)

covr::to_cobertura(cov, filename = "coverage.xml")
message("Wrote coverage.xml")
