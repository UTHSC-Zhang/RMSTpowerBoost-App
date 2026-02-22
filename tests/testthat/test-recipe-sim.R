options(stringsAsFactors = FALSE)

test_that("validate_recipe normalizes PH exponential and sets defaults", {
  rec <- list(
    n = 10,
    covariates = list(defs = list()),
    treatment = list(assignment = "randomization", allocation = "1:1"),
    event_time = list(
      model = "ph_exp",
      baseline = list(rate = 0.1),
      effects = list(intercept = 0, treatment = 0)
    ),
    censoring = list(mode = "target_overall", target = 0.2, admin_time = 5)
  )

  v <- validate_recipe(rec)
  expect_equal(v$event_time$model, "cox_pwexp")
  expect_true(is.list(v$event_time$baseline))
  expect_true(is.numeric(v$event_time$baseline$rates))
})

test_that("simulate_from_recipe handles explicit censoring and logistic PS", {
  defs <- list(
    list(name = "x1", type = "continuous", dist = "normal",
         params = list(mean = 0, sd = 1), transform = list("center(0)", "scale(2)")),
    list(name = "grp", type = "categorical", dist = "categorical",
         params = list(prob = c(0.5, 0.5), labels = c("g1", "g2")))
  )

  rec <- list(
    n = 30,
    covariates = list(defs = defs),
    treatment = list(
      assignment = "logistic_ps",
      ps_model = list(formula = "~ x1")
    ),
    event_time = list(
      model = "aft_weibull",
      baseline = list(shape = 1.2, scale = 2),
      effects = list(intercept = 0, treatment = -0.1, formula = "~ x1", beta = c(0, 0.2)),
      frailty = list(type = "lognormal", var = 0.2, group = "grp")
    ),
    censoring = list(
      mode = "explicit",
      administrative = list(time = 3),
      random = list(dist = "exponential", params = list(rate = 0.1)),
      dependent = list(formula = "~ x1", beta = c(0, 0.1), base = 0.05)
    ),
    seed = 1
  )

  dat <- simulate_from_recipe(rec)
  expect_true(is.data.frame(dat))
  expect_true(all(c("time", "status", "arm", "x1", "grp") %in% names(dat)))
  expect_true(is.numeric(attr(dat, "achieved_censoring")))
})

test_that("simulate_from_recipe supports gamma frailty with PH model", {
  defs <- list(
    list(name = "grp", type = "categorical", dist = "categorical",
         params = list(prob = c(0.4, 0.6), labels = c("a", "b")))
  )

  rec <- list(
    n = 20,
    covariates = list(defs = defs),
    treatment = list(assignment = "randomization", allocation = "1:1"),
    event_time = list(
      model = "ph_weibull",
      baseline = list(shape = 1.2, scale = 1.5),
      effects = list(intercept = 0, treatment = 0.1),
      frailty = list(type = "gamma", var = 0.3, group = "grp")
    ),
    censoring = list(
      mode = "explicit",
      administrative = list(time = 2),
      random = list(dist = "exponential", params = list(rate = 0.05))
    )
  )

  dat <- simulate_from_recipe(rec)
  expect_equal(nrow(dat), 20)
  expect_true(is.numeric(attr(dat, "achieved_censoring")))
})

test_that("recipe_grid expands paths and gen_covariates handles factors", {
  defs <- list(
    list(name = "cat", type = "categorical", dist = "categorical",
         params = list(prob = c(0.5, 0.5), labels = c("A", "B"))),
    list(name = "ord", type = "ordinal", dist = "ordinal",
         params = list(prob = c(0.2, 0.8), labels = c("L", "H")))
  )

  rec <- list(
    n = 12,
    covariates = list(defs = defs),
    treatment = list(assignment = "randomization", allocation = "1:1"),
    event_time = list(
      model = "aft_lognormal",
      baseline = list(mu = 2.2, sigma = 0.5),
      effects = list(intercept = 0, treatment = -0.2)
    ),
    censoring = list(mode = "target_overall", target = 0.2, admin_time = 2)
  )

  grid <- recipe_grid(rec, vary = list(n = c(10, 12), "event_time.effects.treatment" = c(-0.1, -0.2)))
  expect_equal(length(grid), 4)

  X <- gen_covariates(5, list(defs = defs))
  expect_true(is.factor(X$cat))
  expect_true(is.ordered(X$ord))
})

base_recipe <- function() {
  list(
    n = 20,
    covariates = list(defs = list(
      list(name = "x1", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)),
      list(name = "grp", type = "categorical", dist = "categorical",
           params = list(prob = c(0.5, 0.5), labels = c("A", "B")))
    )),
    treatment = list(assignment = "randomization", allocation = "1:1"),
    event_time = list(
      model = "aft_lognormal",
      baseline = list(mu = 2.2, sigma = 0.5),
      effects = list(intercept = 0, treatment = -0.2)
    ),
    censoring = list(mode = "target_overall", target = 0.2, admin_time = 3),
    seed = 101
  )
}

test_that("validate_recipe catches key invalid configurations", {
  expect_error(validate_recipe(NULL), "`recipe` must be a list")
  expect_error(validate_recipe(list(n = 0)), "`recipe\\$n` must be a positive number")

  rec <- base_recipe()
  rec$treatment$assignment <- "unknown"
  expect_error(validate_recipe(rec), "Unknown treatment\\$assignment")

  rec <- base_recipe()
  rec$treatment <- list(assignment = "stratified", allocation = "1:1")
  expect_error(validate_recipe(rec), "stratify_by required")

  rec <- base_recipe()
  rec$treatment <- list(assignment = "logistic_ps", ps_model = list())
  expect_error(validate_recipe(rec), "ps_model\\$formula is required")

  rec <- base_recipe()
  rec$event_time$model <- "not_a_model"
  expect_error(validate_recipe(rec), "Unsupported event_time\\$model")

  rec <- base_recipe()
  rec$event_time$baseline <- 1
  expect_error(validate_recipe(rec), "event_time\\$baseline must be a list")

  rec <- base_recipe()
  rec$event_time$effects <- list(formula = "~ x1")
  expect_error(validate_recipe(rec), "effects\\$formula provided but effects\\$beta missing")

  rec <- base_recipe()
  rec$event_time$effects <- list(beta = c(0, 1))
  expect_error(validate_recipe(rec), "effects\\$beta provided but effects\\$formula missing")

  rec <- base_recipe()
  rec$event_time$frailty <- list(type = "bad", var = 0.2, group = "grp")
  expect_error(validate_recipe(rec), "frailty\\$type must be")

  rec <- base_recipe()
  rec$censoring$mode <- "bad_mode"
  expect_error(validate_recipe(rec), "censoring\\$mode must be")
})

test_that("PH alias normalization and quick builder are consistent", {
  rec <- base_recipe()
  rec$event_time$model <- "proportional_hazards_exp"
  rec$event_time$baseline <- list(rate = 0.15)
  v <- validate_recipe(rec)
  expect_equal(v$event_time$model, "cox_pwexp")
  expect_equal(v$event_time$baseline$rates, 0.15)
  expect_equal(v$event_time$baseline$cuts, numeric(0))

  q <- recipe_quick_aft(
    n = 30,
    model = "aft_weibull",
    baseline = list(shape = 1.2, scale = 2.3),
    treat_effect = -0.1,
    covariates = list(list(name = "x", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1))),
    target_censoring = 0.3,
    allocation = "2:1",
    seed = 42
  )
  expect_equal(q$n, 30L)
  expect_equal(q$treatment$allocation, "2:1")
  expect_equal(q$event_time$model, "aft_weibull")
})

test_that("simulate_from_recipe covers stratified and logistic treatment paths", {
  rec <- base_recipe()
  rec$treatment <- list(assignment = "stratified", allocation = "1:1", stratify_by = "grp")
  dat <- simulate_from_recipe(rec)
  expect_equal(nrow(dat), rec$n)
  expect_true(all(dat$arm %in% c(0L, 1L)))

  rec <- base_recipe()
  rec$treatment <- list(assignment = "logistic_ps", ps_model = list(formula = "~ x1", beta = c(0, 1)))
  dat <- simulate_from_recipe(rec)
  expect_equal(nrow(dat), rec$n)
  expect_true(all(dat$arm %in% c(0L, 1L)))

  rec <- base_recipe()
  rec$treatment <- list(assignment = "logistic_ps", ps_model = list(formula = "~ x1", beta = c(1, 1, 1)))
  expect_error(simulate_from_recipe(rec), "ps_model\\$beta length mismatch")
})

test_that("simulate_from_recipe covers event-time and censoring error branches", {
  rec <- base_recipe()
  rec$event_time$model <- "aft_loglogistic"
  rec$event_time$baseline <- list(shape = 1.1, scale = 2)
  dat <- simulate_from_recipe(rec)
  expect_equal(nrow(dat), rec$n)

  rec <- base_recipe()
  rec$event_time$model <- "ph_exponential"
  rec$event_time$baseline <- list(rate = 0.2)
  dat <- simulate_from_recipe(rec)
  expect_equal(nrow(dat), rec$n)

  rec <- base_recipe()
  rec$censoring <- list(mode = "explicit", random = list(dist = "weibull", params = list(rate = 0.1)))
  expect_error(simulate_from_recipe(rec), "explicit random censoring supports dist='exponential'")

  rec <- base_recipe()
  rec$censoring <- list(
    mode = "explicit",
    administrative = list(time = 2),
    dependent = list(formula = "~ x1", beta = c(1, 2, 3), base = 0.05)
  )
  expect_error(simulate_from_recipe(rec), "dependent beta length mismatch")
})

test_that("frailty guards and transform helper paths execute", {
  rec <- base_recipe()
  rec$event_time$model <- "aft_lognormal"
  rec$event_time$frailty <- list(type = "gamma", var = 0.2, group = "grp")
  expect_error(simulate_from_recipe(rec), "gamma frailty supported for PH/Cox models only")

  rec <- base_recipe()
  rec$event_time$frailty <- list(type = "lognormal", var = 0.1, group = "missing_group")
  expect_error(simulate_from_recipe(rec), "frailty group variable 'missing_group' not found")

  x <- RMSTpowerBoostApp:::.apply_transforms(10, c("center(2)", "scale(4)"))
  expect_equal(x, 2)
})

test_that("internal helper error and edge branches are covered", {
  expect_error(.parse_allocation("bad"), "allocation must be of the form")
  expect_equal(names(.parse_allocation("2:1")), c("p0", "p1"))

  expect_error(.rdraw(list(dist = "nope", params = list()), 5), "Unsupported covariate dist")
  expect_equal(length(.rdraw(list(dist = "uniform", params = list(min = 0, max = 1)), 4)), 4)

  x <- data.frame(x1 = rnorm(5))
  eff <- list(intercept = 0, treatment = 0, covariates = list(unknown = 1))
  expect_error(.build_lp(eff, x, arm = rep(0, 5)), "unknown covariate")

  expect_error(.sim_time("unsupported_model", list(), rep(0, 5), 5), "Unsupported model")
  expect_error(.sim_pwexp(numeric(0), numeric(0), lp = rep(0, 3)), "must have length >= 1")
  expect_error(.sim_pwexp(c(0.1, 0.2), numeric(0), lp = rep(0, 3)), "length\\(cuts\\) must be m-1")

  ach <- .achieved_cens_exp(T_event = c(1, 2, 3), rate = 0, admin_time = 2)
  expect_true(is.numeric(ach))
  expect_true(is.finite(ach))

  r_hi <- .solve_rate_for_target(T_event = c(1, 2, 3), target = 0.95, admin_time = Inf, tol = 1e-4)
  expect_true(is.numeric(r_hi))
  expect_true(length(r_hi) == 1)
})
