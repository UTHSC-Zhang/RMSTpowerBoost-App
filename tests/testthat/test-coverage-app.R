options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(survival)
  library(dplyr)
})

make_pilot <- function(n_per_stratum = 20, event_rate = 0.7) {
  n_per_stratum <- as.integer(n_per_stratum)
  if (n_per_stratum %% 2 != 0) n_per_stratum <- n_per_stratum + 1L
  n <- n_per_stratum * 2L
  strata <- rep(c("A", "B"), each = n_per_stratum)
  arm <- rep(rep(c(0, 1), each = n_per_stratum / 2L), 2L)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  time <- rexp(n, rate = 0.1) + 0.1
  status <- rbinom(n, 1, event_rate)
  for (s in unique(strata)) {
    idx <- which(strata == s)
    if (sum(status[idx]) < 2) status[idx][1:2] <- 1
  }
  data.frame(time = time, status = status, arm = arm, strata = strata, x1 = x1, x2 = x2)
}

test_that("app coverage harness runs without error", {
  set.seed(123)
  pilot <- make_pilot()

  covs <- list(list(name = "x1", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)))
  rec <- recipe_quick_aft(
    n = 40, model = "aft_lognormal",
    baseline = list(mu = 2.2, sigma = 0.5),
    treat_effect = -0.2,
    covariates = covs,
    target_censoring = 0.2,
    allocation = "1:1"
  )

  tmp_dir <- tempfile("sets_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  expect_error({
    suppressWarnings({
    generate_recipe_sets(
      rec,
      vary = list(n = c(20, 30)),
      out_dir = tmp_dir,
      formats = c("csv", "rds"),
      n_reps = 1,
      seed_base = 1,
      filename_template = "sc{scenario_id}_r{rep}_n{n}"
    )

    load_recipe_sets(file.path(tmp_dir, "manifest.rds"))

    rebuild_manifest(
      rec,
      vary = list(n = c(20, 30)),
      out_dir = tmp_dir,
      filename_template = "sc{scenario_id}_r{rep}_n{n}"
    )

    linear.power.analytical.app(pilot, "time", "status", "arm", sample_sizes = c(10, 12), linear_terms = "x1", L = 2)
    linear.ss.analytical.app(pilot, "time", "status", "arm", target_power = 0.5, linear_terms = "x1", L = 2,
                             n_start = 8, n_step = 2, max_n_per_arm = 12)

    additive.power.analytical.app(pilot, "time", "status", "arm", "strata", sample_sizes = c(8, 10),
                                  linear_terms = "x1", L = 2)
    additive.ss.analytical.app(pilot, "time", "status", "arm", "strata", target_power = 0.5,
                               linear_terms = "x1", L = 2, n_start = 6, n_step = 2, max_n_per_arm = 10)

    MS.power.analytical.app(pilot, "time", "status", "arm", "strata", sample_sizes = c(8, 10),
                            linear_terms = "x1", L = 2)
    MS.ss.analytical.app(pilot, "time", "status", "arm", "strata", target_power = 0.5,
                         linear_terms = "x1", L = 2, n_start = 6, n_step = 2, max_n_per_arm = 10)

    DC.power.analytical.app(pilot, "time", "status", "arm", "status", sample_sizes = c(10, 12),
                            linear_terms = "x1", L = 2)
    DC.ss.analytical.app(pilot, "time", "status", "arm", "status", target_power = 0.5,
                         linear_terms = "x1", L = 2, n_start = 8, n_step = 2, max_n_per_arm = 12)
    })
  }, NA)
})
