#' @title Internal Helper to Estimate Multiplicative Stratified Model Parameters
#' @description This internal function contains the common logic for estimating the
#'   log-RMST ratio and its variance from pilot data using a log-linear working
#'   model (Wang et al. 2019), ported from RMSTpowerBoost. Censoring weights come
#'   from a Cox model for censoring with treatment and \code{linear_terms} as
#'   predictors and the stratum as a Cox stratum, and are used as fitted, without
#'   capping. The working model is fitted by weighted least squares and its
#'   variance is the robust sandwich of the estimating equations.
#' @return A list containing `beta_effect`, `se_beta_n1` (sandwich, n = 1 scale),
#'   `se_beta_model_n1` (model-based weighted-least-squares, n = 1 scale),
#'   `n_pilot`, `n_strata`, the weighted fit `fit_wls` and `arm_term`.
#' @keywords internal
#' @export
.estimate_multiplicative_stratified_params <- function(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L) {

  # --- 1. Estimate Parameters from Pilot Data (log-linear approximation) ---
  cat("--- Estimating parameters from pilot data (log-linear approximation)... ---\n")

  covariates <- c(arm_var, linear_terms)
  all_vars <- c(time_var, status_var, arm_var, strata_var, linear_terms)
  df <- .rmst_prepare_df(pilot_data, all_vars, time_var, status_var, L)
  n_pilot <- nrow(df)

  # App-side input check, reported before any model is fitted.
  .check_events_per_stratum(df, status_var, strata_var)

  # Already Delta_Y * W_hat, uncapped.
  df$weights <- .ipcw_stratified_cox_weights(df, time_var, status_var,
                                             covariates, strata_var)

  # Log-linear working model fitted by weighted least squares
  fit_data <- df[df$weights > 0 & df$Y_rmst > 0, ]
  fit_weights <- fit_data$weights
  log_model_formula <- stats::as.formula(
    paste("log(Y_rmst) ~", paste(covariates, collapse = " + "), "+", strata_var))
  message("Approximation Model: ", deparse(log_model_formula))

  if (nrow(fit_data) < (length(covariates) + length(unique(fit_data[[strata_var]])))) {
    stop("Not enough data points to fit the approximation model after filtering.", call. = FALSE)
  }
  fit_log_lm <- stats::lm(log_model_formula, data = fit_data, weights = fit_weights)

  beta_all <- stats::coef(fit_log_lm)
  beta_effect <- beta_all[[arm_var]]

  # --- 2. Robust sandwich variance A^-1 B A^-1 of the WLS estimating equations ---
  # Columns with NA coefficients from a rank-deficient fit are dropped.
  X <- stats::model.matrix(fit_log_lm)
  ok <- !is.na(beta_all)
  X <- X[, ok, drop = FALSE]
  r <- stats::residuals(fit_log_lm)

  A_hat <- crossprod(X * sqrt(fit_weights)) / n_pilot
  A_hat_inv <- tryCatch({
    solve(A_hat)
  }, error = function(e) {
    stop("The design matrix (A_hat) is singular and cannot be inverted. This may be caused by a lack of variation in the covariates within one or more strata.", call. = FALSE)
  })
  epsilon <- X * (fit_weights * r)
  B_hat <- crossprod(epsilon) / n_pilot
  V_hat_n <- A_hat_inv %*% B_hat %*% t(A_hat_inv)
  # Model-based weighted-least-squares variance, sigma^2 A^{-1}, on the n = 1
  # scale: what summary(fit_log_lm) reports and what the WLS t-test uses.
  V_model_n <- summary(fit_log_lm)$sigma^2 * A_hat_inv

  list(
    beta_effect = beta_effect,
    se_beta_n1 = sqrt(V_hat_n[arm_var, arm_var]),
    se_beta_model_n1 = sqrt(V_model_n[arm_var, arm_var]),
    n_pilot = n_pilot,
    n_strata = length(unique(df[[strata_var]])),
    fit_wls = fit_log_lm,
    arm_term = arm_var
  )
}


# Power Calculation -------------------------------------------------------
#' @title Analyze Power for a Multiplicative Stratified RMST Model (Analytic)
#' @description Performs power analysis for a multiplicative, stratified RMST model using an
#'   analytic method based on the work of Wang et al. (2019).
#' @inheritParams .estimate_multiplicative_stratified_params
#' @param sample_sizes A numeric vector of sample sizes *per stratum* to calculate power for.
#' @param alpha The significance level (Type I error rate).
#' @param point_cb Optional callback \code{function(n, power)} for live plotting.
#' @param test Which test the reported power refers to. \code{"sandwich"}
#'   (default) is the robust sandwich Wald test of Wang et al. (2019).
#'   \code{"wls"} is the weighted-least-squares t-test, with the sampling
#'   distribution from the sandwich and the rejection threshold from the
#'   model-based WLS standard error.
#' @return A list containing results.
#' @keywords internal
#' @export
MS.power.analytical.app <- function(pilot_data, time_var, status_var, arm_var, strata_var,
                                sample_sizes, linear_terms = NULL, L, alpha = 0.05,
                                point_cb = NULL, test = c("sandwich", "wls")) {
  test <- match.arg(test)

  # 1. Estimate parameters from pilot data
  params <- .estimate_multiplicative_stratified_params(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L)

  # 2. Calculate Power
  cat("--- Calculating power for specified sample sizes... ---\n")
  power_values <- .rmst_app_power_values(
    params, sample_sizes, params$n_strata, alpha,
    se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
    point_cb = point_cb
  )
  results_df <- data.frame(N_per_Stratum = sample_sizes, Power = power_values)

  # 3. Create Plot and Return
  p <- .rmst_app_power_plot(
    results_df, "N_per_Stratum", "#E69F00",
    title = "Analytic Power Curve: Multiplicative Stratified RMST Model",
    subtitle = if (test == "wls")
      "Weighted least-squares t-test; sandwich sampling variance."
    else "Log-linear approximation with robust sandwich variance (Wang et al. 2019).",
    xlab = "Sample Size Per Stratum"
  )

  results_summary <- data.frame(
    Statistic = "Assumed log(RMST Ratio) (from pilot)",
    Value = params$beta_effect
  )

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}

# Sample Size Search ------------------------------------------------------
#' @title Find Sample Size for a Multiplicative Stratified RMST Model (Analytic)
#' @description Calculates the required sample size for a target power using the analytic
#'   (approximate) method from Wang et al. (2019).
#' @inheritParams .estimate_multiplicative_stratified_params
#' @inheritParams MS.power.analytical.app
#' @param target_power A single numeric value for the desired power.
#' @param n_start The starting sample size *per stratum* for the search.
#' @param n_step The increment in sample size at each step of the search.
#' @param max_n_per_arm The maximum sample size *per stratum* to search up to.
#' @return A list containing results.
#' @keywords internal
#' @export
MS.ss.analytical.app <- function(pilot_data, time_var, status_var, arm_var, strata_var,
                             target_power, linear_terms = NULL, L, alpha = 0.05,
                             n_start = 50, n_step = 25, max_n_per_arm = 2000,
                             point_cb = NULL, test = c("sandwich", "wls")) {
  test <- match.arg(test)

  # 1. Estimate parameters from pilot data
  params <- .estimate_multiplicative_stratified_params(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L)

  # 2. Iterative Search for Sample Size
  cat("--- Searching for Sample Size (Method: Analytic/Approximation) ---\n")
  search <- .rmst_app_ss_search(
    params, params$n_strata, target_power, alpha, n_start, n_step, max_n_per_arm, "/stratum",
    se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
    point_cb = point_cb
  )
  final_n <- search$final_n

  # 3. Finalize and Return Results
  results_summary <- data.frame(Statistic = "Assumed log(RMST Ratio) (from pilot)", Value = params$beta_effect)
  results_df <- data.frame(Target_Power = target_power, Required_N_per_Stratum = final_n)
  search_path_df <- search$search_path_df
  names(search_path_df) <- c("N_per_Stratum", "Power")

  p <- .rmst_app_ss_plot(
    search_path_df, "N_per_Stratum", final_n, target_power,
    title = "Analytic Sample Size Search: Multiplicative Stratified RMST Model",
    subtitle = "Power calculated from formula at each step (approximate method).",
    xlab = "Sample Size Per Stratum"
  )

  cat("\n--- Calculation Summary ---\n")
  print(knitr::kable(results_df, caption = "Required Sample Size"))

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}
