#' @title Estimate the Linear IPCW RMST Model from Pilot Data
#' @description Internal helper shared by the analytical functions and the app's
#'   Repeated method, ported from RMSTpowerBoost. The censoring survival comes
#'   from a Cox model for censoring with treatment and \code{linear_terms} as
#'   predictors and Breslow ties, fitted on the original follow-up and evaluated
#'   at each subject's truncated outcome. The IPCW weights are used as fitted,
#'   without capping. The treatment effect is estimated by weighted least
#'   squares and its variance by the simplified sandwich of Zhang & Schaubel
#'   (2024), Eqs. (11)-(12), treating the weights as fixed.
#' @return A list with \code{beta_effect}, \code{se_beta_n1} (sandwich, n = 1
#'   scale), \code{se_beta_model_n1} (model-based weighted-least-squares, n = 1
#'   scale), \code{n_pilot}, the weighted fit \code{fit_lm} (also as
#'   \code{fit_wls}) and \code{arm_term}, the treatment row of its coefficient
#'   table.
#' @keywords internal
#' @export
.estimate_linear_ipcw_params <- function(pilot_data, time_var, status_var, arm_var, linear_terms, L) {

  cat("--- Estimating parameters from pilot data for analytic calculation... ---\n")

  core_vars <- c(time_var, status_var, arm_var)
  all_vars <- c(core_vars, linear_terms)
  df <- .rmst_prepare_df(pilot_data, all_vars, time_var, status_var, L)
  n_pilot <- nrow(df)

  factor_arm_str <- paste0("factor(", arm_var, ")")
  model_rhs <- paste(c(factor_arm_str, linear_terms), collapse = " + ")
  model_formula <- stats::as.formula(paste("Y_rmst ~", model_rhs))
  message("Model: Y_rmst ~ ", model_rhs)

  df$weights <- .linear_cox_weights(df, time_var, status_var, arm_var, linear_terms, L)

  fit_data <- df[df$weights > 0, ]
  fit_weights <- fit_data$weights
  if (length(unique(fit_data[[arm_var]])) < 2) {
    stop("Pilot data contains events in only one arm after filtering.", call. = FALSE)
  }

  fit_lm <- stats::lm(model_formula, data = fit_data, weights = fit_weights)
  beta_hat <- stats::coef(fit_lm)
  arm_pattern <- paste0("^factor\\(", arm_var, "\\)1$")
  arm_coeff_name <- names(beta_hat)[grep(arm_pattern, names(beta_hat))]
  if (length(arm_coeff_name) == 0) stop("Could not find treatment effect coefficient.")
  beta_effect <- beta_hat[arm_coeff_name]

  # Simplified sandwich A^-1 B A^-1 with the IPCW weights treated as fixed:
  # A_n = X'WX / n, B_n = sum_i (x_i w_i r_i)^{ox2} / n.
  cat("--- Calculating asymptotic variance... ---\n")
  X <- stats::model.matrix(model_formula, data = df)
  A_hat <- crossprod(X * sqrt(df$weights), X * sqrt(df$weights)) / n_pilot
  A_hat_inv <- tryCatch({
    solve(A_hat)
  }, error = function(e) {
    stop("The design matrix (A_hat) is singular. This can happen with small pilot datasets or perfect separation.", call. = FALSE)
  })

  df$predicted_rmst <- stats::predict(fit_lm, newdata = df)
  residuals <- df$Y_rmst - df$predicted_rmst
  epsilon <- X * residuals * df$weights
  epsilon[is.na(epsilon)] <- 0
  B_hat <- crossprod(epsilon) / n_pilot

  V_hat_n <- A_hat_inv %*% B_hat %*% t(A_hat_inv)
  # Model-based weighted-least-squares variance, sigma^2 A^{-1}, on the same
  # n = 1 scale: what summary(fit_lm) reports and what the WLS t-test uses for
  # its rejection threshold.
  V_model_n <- summary(fit_lm)$sigma^2 * A_hat_inv

  list(
    beta_effect = beta_effect,
    se_beta_n1 = sqrt(V_hat_n[arm_coeff_name, arm_coeff_name]),
    se_beta_model_n1 = sqrt(V_model_n[arm_coeff_name, arm_coeff_name]),
    n_pilot = n_pilot,
    fit_lm = fit_lm,
    fit_wls = fit_lm,
    arm_term = arm_coeff_name
  )
}



# Power Calculation -------------------------------------------------------------------
#' @title Analyze Power for a Linear RMST Model (Analytic)
#' @description Performs power analysis using a direct formula based on the
#'   asymptotic variance estimator for the linear RMST model.
#' @inheritParams .estimate_linear_ipcw_params
#' @param sample_sizes A numeric vector of sample sizes *per arm* to calculate power for.
#' @param alpha The significance level for the power calculation (Type I error rate).
#' @param point_cb Optional callback \code{function(n, power)} for live plotting.
#' @param test Which test the reported power refers to. \code{"wls"} (default) is
#'   the weighted-least-squares t-test that \code{lm(..., weights = w)} reports,
#'   with the sampling distribution from the sandwich variance and the rejection
#'   threshold from the model-based WLS standard error. \code{"sandwich"} is the
#'   robust sandwich Wald test.
#' @return A `list` containing results.
#' @keywords internal
#' @export
linear.power.analytical.app <- function(pilot_data, time_var, status_var, arm_var,
                                    sample_sizes, linear_terms = NULL, L, alpha = 0.05,
                                    point_cb = NULL, test = c("wls", "sandwich")) {
  test <- match.arg(test)

  # 1. Estimate nuisance parameters from pilot data using the helper function
  params <- .estimate_linear_ipcw_params(pilot_data, time_var, status_var, arm_var, linear_terms, L)

  # 2. Calculate Power for Each Sample Size
  cat("--- Calculating power for specified sample sizes... ---\n")
  power_values <- .rmst_app_power_values(
    params, sample_sizes, 2, alpha,
    se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
    point_cb = point_cb
  )
  results_df <- data.frame(N_per_Arm = sample_sizes, Power = power_values)

  # 3. Create Summary and Plot
  results_summary <- data.frame(
    Statistic = "Assumed RMST Difference (from pilot)",
    Value = params$beta_effect
  )
  p <- .rmst_app_power_plot(
    results_df, "N_per_Arm", "#D55E00",
    title = "Analytic Power Curve: Linear IPCW RMST Model",
    subtitle = if (test == "wls")
      "Weighted least-squares t-test; sandwich sampling variance (Tian et al. 2014)."
    else "Sandwich Wald test; asymptotic variance from Tian et al. (2014).",
    xlab = "Sample Size Per Arm"
  )

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}

# Sample Size Search ------------------------------------------------------
#' @title Find Sample Size for a Linear RMST Model (Analytic)
#' @description Calculates the required sample size for a target power using an
#'   analytic formula based on the methods of Tian et al. (2014).
#' @inheritParams .estimate_linear_ipcw_params
#' @inheritParams linear.power.analytical.app
#' @param target_power A single numeric value for the desired power (e.g., 0.80 or 0.90).
#' @param n_start The starting sample size *per arm* for the search.
#' @param n_step The increment in sample size at each step of the search.
#' @param max_n_per_arm The maximum sample size *per arm* to search up to.
#' @return A `list` containing results.
#' @keywords internal
#' @export
linear.ss.analytical.app <- function(pilot_data, time_var, status_var, arm_var,
                                 target_power, linear_terms = NULL, L, alpha = 0.05,
                                 n_start = 50, n_step = 25, max_n_per_arm = 2000,
                                 point_cb = NULL, test = c("wls", "sandwich")) {
  test <- match.arg(test)

  # 1. Estimate parameters from pilot data using the helper function
  params <- .estimate_linear_ipcw_params(pilot_data, time_var, status_var, arm_var, linear_terms, L)

  # 2. Iterative Search for Sample Size using Analytic Formula
  cat("--- Searching for Sample Size (Method: Analytic) ---\n")
  search <- .rmst_app_ss_search(
    params, 2, target_power, alpha, n_start, n_step, max_n_per_arm, "/arm",
    se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
    point_cb = point_cb
  )
  final_n <- search$final_n

  # 3. Finalize and Return Results
  results_summary <- data.frame(
    Statistic = "Assumed RMST Difference (from pilot)",
    Value = params$beta_effect
  )
  results_df <- data.frame(Target_Power = target_power, Required_N_per_Arm = final_n)
  search_path_df <- search$search_path_df
  names(search_path_df) <- c("N_per_Arm", "Power")

  p <- .rmst_app_ss_plot(
    search_path_df, "N_per_Arm", final_n, target_power,
    title = "Analytic Sample Size Search: Linear IPCW RMST Model",
    subtitle = if (test == "wls")
      "Weighted least-squares t-test; power from formula at each step."
    else "Sandwich Wald test; power from formula at each step.",
    xlab = "Sample Size Per Arm"
  )

  cat("\n--- Calculation Summary ---\n")
  print(knitr::kable(results_df, caption = "Required Sample Size"))

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}
