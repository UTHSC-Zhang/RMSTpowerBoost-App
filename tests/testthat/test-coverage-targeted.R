options(stringsAsFactors = FALSE)

mk_pilot <- function(n = 60) {
  set.seed(123)
  data.frame(
    time = rexp(n, rate = 0.12) + 0.1,
    status = rbinom(n, 1, 0.7),
    arm = rbinom(n, 1, 0.5),
    strata = factor(sample(c("S1", "S2"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
}

mk_recipe <- function() {
  recipe_quick_aft(
    n = 40,
    model = "aft_lognormal",
    baseline = list(mu = 2.2, sigma = 0.5),
    treat_effect = -0.2,
    covariates = list(
      list(name = "x1", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)),
      list(name = "grp", type = "categorical", dist = "categorical",
           params = list(prob = c(0.5, 0.5), labels = c("A", "B")))
    ),
    target_censoring = 0.2
  )
}

test_that("generate_recipe_sets and recipe_grid hit validation/edge branches", {
  rec <- mk_recipe()
  out_dir <- tempfile("gen_sets_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expect_error(generate_recipe_sets(rec, out_dir = out_dir, vary = "x"), "vary must be a named list")
  expect_error(generate_recipe_sets(rec, out_dir = out_dir, formats = "bad"), "formats must be subset")

  man <- generate_recipe_sets(
    base_recipe = rec,
    vary = list(),
    out_dir = out_dir,
    formats = character(0),
    n_reps = 1L,
    seed_base = 10
  )
  expect_true(is.data.frame(man))
  expect_true(file.exists(file.path(out_dir, "manifest.rds")))

  g0 <- recipe_grid(rec, list())
  expect_equal(length(g0), 1)
})

test_that("load_recipe_sets and rebuild_manifest cover path-selection and failures", {
  rec <- mk_recipe()
  out_dir <- tempfile("load_sets_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  dat <- simulate_from_recipe(rec, seed = 99)
  f_csv <- file.path(out_dir, "d1.csv")
  f_txt <- file.path(out_dir, "d1.txt")
  f_rds <- file.path(out_dir, "d1.rds")
  f_rdata <- file.path(out_dir, "d1.RData")
  utils::write.csv(dat, f_csv, row.names = FALSE)
  utils::write.table(dat, f_txt, sep = "\t", row.names = FALSE, quote = FALSE)
  saveRDS(dat, f_rds)
  dat_obj <- dat
  save(dat_obj, file = f_rdata)

  # 4 rows to force each loader branch (rds, rdata, csv, txt)
  man <- data.frame(
    scenario_id = 1:4,
    rep = 1L,
    seed = c(1, 2, 3, 4),
    achieved_censoring = c(NA, NA, NA, NA),
    file_rds = c(f_rds, NA, NA, NA),
    file_rdata = c(NA, f_rdata, NA, NA),
    file_csv = c(NA, NA, f_csv, NA),
    file_txt = c(NA, NA, NA, f_txt),
    stringsAsFactors = FALSE
  )
  mpath <- file.path(out_dir, "manifest.rds")
  saveRDS(man, mpath)
  sets <- load_recipe_sets(mpath)
  expect_equal(length(sets), 4)
  expect_true(all(vapply(sets, function(x) is.data.frame(x$data), logical(1))))

  # Empty manifest error branch
  saveRDS(man[0, ], mpath)
  expect_error(load_recipe_sets(mpath), "manifest has 0 rows")

  # No file path in row branch
  bad <- data.frame(
    scenario_id = 1, rep = 1, seed = 1,
    file_rds = NA_character_, file_rdata = NA_character_,
    file_csv = NA_character_, file_txt = NA_character_,
    stringsAsFactors = FALSE
  )
  saveRDS(bad, mpath)
  expect_error(load_recipe_sets(mpath), "No data file path in manifest row")

  expect_error(rebuild_manifest(rec, list(), out_dir = file.path(out_dir, "missing")), "out_dir does not exist")

  empty_dir <- tempfile("rebuild_empty_")
  dir.create(empty_dir, recursive = TRUE, showWarnings = FALSE)
  expect_error(rebuild_manifest(rec, list(), out_dir = empty_dir), "no files matched")
})

test_that("survival diagnostics covers one-arm and stratified paths", {
  dat <- mk_pilot(40)
  dat_one_arm <- dat
  dat_one_arm$arm <- 1L
  d1 <- .run_survival_diagnostics(dat_one_arm, "time", "status", "arm", alpha = 0.05)
  expect_true(is.list(d1))
  expect_true("logrank_summary" %in% names(d1))

  d2 <- .run_survival_diagnostics(dat, "time", "status", "arm", strata_var = "strata", alpha = 0.1)
  expect_true(is.list(d2))
  expect_s3_class(d2$km_plot, "ggplot")
})

test_that("analytical stratified models cover error/warning branches", {
  dat <- mk_pilot(80)

  dat_bad <- dat
  dat_bad$status <- 0L
  dat_bad$status[dat_bad$strata == "S1"][1] <- 1L
  dat_bad$status[dat_bad$strata == "S2"][1] <- 1L
  expect_error(
    additive.power.analytical.app(dat_bad, "time", "status", "arm", "strata",
                                  sample_sizes = c(8, 10), linear_terms = "x1", L = 2),
    "Insufficient events"
  )
  expect_error(
    MS.power.analytical.app(dat_bad, "time", "status", "arm", "strata",
                            sample_sizes = c(8, 10), linear_terms = "x1", L = 2),
    "Insufficient events"
  )

  dat_chr <- dat
  dat_chr$x1 <- as.character(round(dat_chr$x1, 2))
  expect_error(
    additive.power.analytical.app(dat_chr, "time", "status", "arm", "strata",
                                  sample_sizes = c(8, 10), linear_terms = "x1", L = 2),
    "must be numeric"
  )

  expect_warning(
    additive.ss.analytical.app(dat, "time", "status", "arm", "strata",
                               target_power = 0.9999, linear_terms = "x1", L = 2,
                               n_start = 4, n_step = 2, max_n_per_arm = 6),
    "not achieved by max N"
  )
  expect_warning(
    MS.ss.analytical.app(dat, "time", "status", "arm", "strata",
                         target_power = 0.9999, linear_terms = "x1", L = 2,
                         n_start = 4, n_step = 2, max_n_per_arm = 6),
    "not achieved by max N"
  )
})

test_that("boot model validations and failure paths are covered", {
  dat <- mk_pilot(50)

  expect_error(
    linear.power.boot.app(dat, "time", "status", "arm", sample_sizes = NULL, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "sample_sizes"
  )
  expect_error(
    linear.ss.boot.app(dat, "time", "status", "arm", target_power = NULL, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "target_power"
  )

  expect_error(
    additive.power.boot.app(dat, "time", "status", "arm", sample_sizes = NULL, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "sample_sizes"
  )
  expect_error(
    additive.ss.boot.app(dat, "time", "status", "arm", target_power = NULL, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "target_power"
  )

  expect_error(
    MS.power.boot.app(dat, "time", "status", "arm", "strata", sample_sizes = NULL, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "sample_sizes"
  )
  expect_error(
    MS.ss.boot.app(dat, "time", "status", "arm", "strata", target_power = c(0.8, 0.9), linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1),
    "single numeric value"
  )
})

test_that("recipe_sim internal branches move closer to full coverage", {
  # validate_recipe branches
  rec <- mk_recipe()
  rec$treatment <- NULL
  v <- validate_recipe(rec)
  expect_true(is.null(v$treatment))

  rec <- mk_recipe()
  rec$event_time$frailty <- list(type = "gamma", var = -1, group = "grp")
  expect_error(validate_recipe(rec), "nonnegative")

  # normalize alias path
  nm <- .normalize_model_name("ph_piecewise_exponential", list(rates = c(0.1), cuts = numeric(0)))
  expect_equal(nm$model, "cox_pwexp")

  # .rdraw branches
  expect_length(.rdraw(list(dist = "lognormal", params = list(meanlog = 0, sdlog = 1)), 5), 5)
  expect_length(.rdraw(list(dist = "gamma", params = list(shape = 2, scale = 1)), 5), 5)
  expect_length(.rdraw(list(dist = "weibull", params = list(shape = 1.3, scale = 2)), 5), 5)
  expect_length(.rdraw(list(dist = "beta", params = list(shape1 = 2, shape2 = 5)), 5), 5)
  expect_length(.rdraw(list(dist = "t", params = list(df = 4)), 5), 5)
  expect_length(.rdraw(list(dist = "bernoulli", params = list(p = 0.5)), 5), 5)

  # gen_covariates no-def branch
  expect_equal(ncol(gen_covariates(5, list(defs = list()))), 0)

  # assignment unknown branch
  expect_error(.assign_treatment(5, data.frame(x = rnorm(5)), list(assignment = "bad")), "Unknown treatment assignment")

  # build_lp beta mismatch and known covariates path
  x <- data.frame(x1 = rnorm(5))
  expect_error(.build_lp(list(formula = "~ x1", beta = c(1, 2, 3)), x, rep(0, 5)), "must equal ncol")
  expect_length(.build_lp(list(intercept = 0, treatment = 0, covariates = list(x1 = 0.5)), x, rep(1, 5)), 5)

  # sim_pwexp/sim_time and solver branches
  t1 <- .sim_pwexp(rates = c(0.05, 0.1), cuts = c(2), lp = rep(0, 10))
  expect_equal(length(t1), 10)
  t2 <- .sim_time("cox_pwexp", list(rates = c(0.1), cuts = numeric(0)), rep(0, 5), 5)
  expect_equal(length(t2), 5)

  # cmin >= target branch
  r0 <- .solve_rate_for_target(T_event = c(10, 10, 10), target = 0.2, admin_time = 1, tol = 1e-3)
  expect_equal(r0, 0)

  expect_error(simulate_from_recipe("bad"), "`recipe` must be a list")
})

test_that("bootstrap wrappers hit summary/success branches via mocked runners", {
  dat <- mk_pilot(40)

  # Linear IPCW bootstrap wrappers
  testthat::local_mocked_bindings(
    .get_linear_ipcw_simulation_runner = function(...) {
      function(n_per_arm) {
        list(
          power = if (n_per_arm >= 20) 0.9 else 0.1,
          estimates = c(0.2, 0.3, 0.4),
          std_errors = c(0.05, 0.06, 0.07)
        )
      }
    }
  )
  lp <- linear.power.boot.app(dat, "time", "status", "arm", sample_sizes = c(10, 20), linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1)
  expect_false(is.null(lp$results_summary))
  ls <- linear.ss.boot.app(dat, "time", "status", "arm", target_power = 0.8, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, patience = 1, n_start = 10, n_step = 10, max_n_per_arm = 30, parallel.cores = 1)
  expect_false(is.null(ls$results_summary))
  expect_equal(ls$results_data$Required_N_per_Arm, 20)
})

test_that("GAM and multiplicative bootstrap wrappers cover parallel and summary branches", {
  dat <- mk_pilot(50)

  testthat::local_mocked_bindings(
    .get_gam_simulation_runner = function(...) {
      function(n_per_group) {
        list(
          power = if (n_per_group >= 8) 0.85 else 0.2,
          estimates = c(0.1, 0.2, 0.25),
          std_errors = c(0.03, 0.04, 0.05)
        )
      }
    }
  )
  gp <- additive.power.boot.app(dat, "time", "status", "arm", strata_var = NULL, sample_sizes = c(6, 8), linear_terms = "x1", smooth_terms = NULL, L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 2)
  expect_false(is.null(gp$results_summary))
  gs <- additive.ss.boot.app(dat, "time", "status", "arm", strata_var = NULL, target_power = 0.8, linear_terms = "x1", smooth_terms = NULL, L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 2, patience = 1, n_start = 6, n_step = 2, max_n_per_arm = 10)
  expect_false(is.null(gs$results_summary))

  testthat::local_mocked_bindings(
    .get_internal_simulation_runner = function(...) {
      function(n_per_stratum) {
        list(power = if (n_per_stratum >= 8) 0.9 else 0.1, estimates = c(1.1, 1.2, 1.3))
      }
    }
  )
  mp <- MS.power.boot.app(dat, "time", "status", "arm", "strata", sample_sizes = c(6, 8), linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 2)
  expect_false(is.null(mp$results_summary))
  ms <- MS.ss.boot.app(dat, "time", "status", "arm", "strata", target_power = 0.8, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 2, patience = 1, n_start = 6, n_step = 2, max_n_per_arm = 10)
  expect_false(is.null(ms$results_summary))
})

test_that("multiplicative analytical callbacks and low-data guard are covered", {
  dat <- mk_pilot(60)
  cb_hits <- 0L
  cb <- function(n, p) { cb_hits <<- cb_hits + 1L }

  p1 <- MS.power.analytical.app(dat, "time", "status", "arm", "strata", sample_sizes = c(8, 10), linear_terms = "x1", L = 2, point_cb = cb)
  expect_true(nrow(p1$results_data) == 2)
  expect_true(cb_hits >= 2)

  cb_hits <- 0L
  s1 <- MS.ss.analytical.app(dat, "time", "status", "arm", "strata", target_power = 0.6, linear_terms = "x1", L = 2, n_start = 6, n_step = 2, max_n_per_arm = 12, point_cb = cb)
  expect_true(nrow(s1$results_data) == 1)
  expect_true(cb_hits >= 1)

  tiny <- dat[1:8, ]
  tiny$status <- c(1, 0, 0, 0, 0, 0, 0, 0)
  tiny$strata <- factor(rep(c("S1", "S2"), each = 4))
  expect_error(
    .estimate_multiplicative_stratified_params(tiny, "time", "status", "arm", "strata", linear_terms = "x1", L = 2),
    "Insufficient events|Not enough data points"
  )
})

test_that("multiplicative stratified bootstrap internal edge branches are covered", {
  dat <- mk_pilot(30)

  # Trigger empty cleaned data path (boot_data NULL/0 rows)
  dat_empty <- dat
  dat_empty$time <- NA_real_
  sim_empty <- .get_internal_simulation_runner(
    pilot_data = dat_empty,
    time_var = "time", status_var = "status", arm_var = "arm", strata_var = "strata",
    linear_terms = "x1", L = 2, alpha = 0.1, n_sim = 2, parallel.cores = 1
  )
  out_empty <- sim_empty(3)
  expect_true(is.list(out_empty))

  # Trigger parallel branch in internal runner
  sim_parallel <- .get_internal_simulation_runner(
    pilot_data = dat,
    time_var = "time", status_var = "status", arm_var = "arm", strata_var = "strata",
    linear_terms = "x1", L = 2, alpha = 0.1, n_sim = 2, parallel.cores = 2
  )
  out_parallel <- sim_parallel(4)
  expect_true(is.list(out_parallel))
})

test_that("multiplicative stratified bootstrap wrappers cover no-summary and stagnation/max paths", {
  dat <- mk_pilot(40)

  # Force no valid estimates summary branch in power wrapper
  testthat::local_mocked_bindings(
    .get_internal_simulation_runner = function(...) {
      function(n_per_stratum) {
        list(power = 0.1, estimates = numeric(0))
      }
    }
  )
  p_none <- MS.power.boot.app(dat, "time", "status", "arm", "strata", sample_sizes = c(4, 6), linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1)
  expect_true(is.null(p_none$results_summary))

  # Force stagnation path
  s_stag <- suppressWarnings(
    MS.ss.boot.app(dat, "time", "status", "arm", "strata", target_power = 0.9, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1, patience = 1, n_start = 4, n_step = 2, max_n_per_arm = 10)
  )
  expect_true(nrow(s_stag$results_data) == 1)
  expect_true(is.null(s_stag$results_summary))

  # Force max-N path without stagnation break
  s_max <- suppressWarnings(
    MS.ss.boot.app(dat, "time", "status", "arm", "strata", target_power = 0.9, linear_terms = "x1", L = 2, n_sim = 2, alpha = 0.1, parallel.cores = 1, patience = 99, n_start = 4, n_step = 2, max_n_per_arm = 6)
  )
  expect_true(nrow(s_max$results_data) == 1)
})
