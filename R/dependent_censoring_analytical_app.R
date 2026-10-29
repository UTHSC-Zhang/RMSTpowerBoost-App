#' @title Internal Helper to Estimate Covariate-Dependent Censoring Model Parameters
#' @description Internal helper for estimating the treatment effect and its variance
#'   from pilot data under a single censoring mechanism that depends on covariates,
#'   ported from RMSTpowerBoost.
#'
#' One Cox model for censoring, \code{Surv(time, status==0) ~ linear_terms} with
#' Breslow ties and treatment excluded, gives the IPCW
#' \eqn{w_i = \Delta_i^Y/\hat G(Y_i\mid X_i)} at \eqn{Y_i=\min(T_i,L)}, pairing the
#' uncentered baseline hazard with an uncentered linear predictor. The weights are
#' used as fitted, with no cap and no floor on \eqn{\hat G}; with no observed
#' censoring they are one and no censoring model is fitted. A weighted RMST
#' regression \code{Y ~ arm + linear_terms} is fitted among complete truncated
#' outcomes, with a sandwich variance that ignores uncertainty in \eqn{\hat G}.
#'
#' @note \code{dep_cens_status_var} is accepted for API compatibility but ignored here.
#' @return A list with \code{beta_effect}, \code{se_beta_n1} (sandwich, n = 1
#'   scale), \code{se_beta_model_n1} (model-based weighted-least-squares, n = 1
#'   scale), \code{n_pilot}, the weighted fit \code{fit_wls} and \code{arm_term}.
#' @keywords internal
#' @export
.estimate_dependent_censoring_params <- function(pilot_data, time_var, status_var, arm_var, dep_cens_status_var, linear_terms, L) {

   # --- 1) Prep & outcome ---
   covariates <- c(arm_var, linear_terms)
   # Ignore dep_cens_status_var in filtering (kept only for API compatibility)
   all_vars <- unique(c(time_var, status_var, covariates))
   df <- .rmst_prepare_df(pilot_data, all_vars, time_var, status_var, L)
   n_pilot <- nrow(df)
   if (n_pilot < 10) stop("Too few complete cases after filtering.", call. = FALSE)

   # --- 2) Censoring model: covariate-dependent only (no arm, no competing risks) ---
   df$weights <- .dc_cox_weights(df, time_var, status_var, linear_terms)

   # --- 3) Weighted RMST regression: Y_rmst ~ arm + covariates ---
   model_rhs <- paste(c(arm_var, linear_terms), collapse = " + ")
   model_formula <- stats::as.formula(paste("Y_rmst ~", model_rhs))
   fit_data <- df[df$weights > 0, ]
   fit_wls <- stats::lm(model_formula, data = fit_data, weights = fit_data$weights)
   beta <- stats::coef(fit_wls)
   if (!(arm_var %in% names(beta))) stop("Arm coefficient not found in model.", call. = FALSE)
   beta_arm <- beta[arm_var]

   # --- 4) Sandwich variance: A = X' W X / n, B = sum_i (x_i w_i r_i)^{ox2} / n ---
   X <- stats::model.matrix(model_formula, data = df)
   r <- df$Y_rmst - as.numeric(X %*% beta)
   W <- df$weights
   A_hat <- crossprod(X, X * W) / n_pilot
   A_hat_inv <- tryCatch({
      solve(A_hat)
   }, error = function(e) {
      stop("The design matrix (A_hat) is singular. This can happen with small pilot datasets or perfect separation.", call. = FALSE)
   })
   meat <- X * (W * r)
   B_hat <- crossprod(meat) / n_pilot
   V_hat_n <- A_hat_inv %*% B_hat %*% t(A_hat_inv)
   # Model-based weighted-least-squares variance, sigma^2 A^{-1}, on the n = 1
   # scale: what summary(fit_wls) reports and what the WLS t-test uses.
   V_model_n <- summary(fit_wls)$sigma^2 * A_hat_inv

   list(
      beta_effect = beta_arm,
      se_beta_n1 = sqrt(V_hat_n[arm_var, arm_var]),
      se_beta_model_n1 = sqrt(V_model_n[arm_var, arm_var]),
      n_pilot = n_pilot,
      fit_wls = fit_wls,
      arm_term = arm_var
   )
}
# Power Calculation -------------------------------------------------------

#' @title Analyze Power for RMST with Covariate-Dependent Censoring (Analytic)
#' @description Analytic power for RMST regression when censoring depends on covariates
#'   (single mechanism; no competing risks). Uses IPCW and a sandwich variance.
#' @inheritParams .estimate_dependent_censoring_params
#' @param sample_sizes Numeric vector of per-arm sample sizes for power.
#' @param alpha Two-sided Type I error (default 0.05).
#' @param point_cb Optional callback \code{function(n, power)} for live plotting.
#' @param test Which test the reported power refers to. \code{"wls"} (default) is
#'   the weighted-least-squares t-test, with the sampling distribution from the
#'   sandwich variance and the rejection threshold from the model-based WLS
#'   standard error. \code{"sandwich"} is the robust sandwich Wald test.
#' @return A list: \code{results_data} (data.frame), \code{results_plot} (ggplot),
#'   \code{results_summary} (data.frame with pilot effect).
#' @keywords internal
#' @export
DC.power.analytical.app <- function(pilot_data,
                                    time_var,
                                    status_var,
                                    arm_var,
                                    dep_cens_status_var,
                                    sample_sizes,
                                    linear_terms = NULL,
                                    L,
                                    alpha = 0.05,
                                    point_cb = NULL,
                                    test = c("wls", "sandwich")) {
   test <- match.arg(test)

   # 1) Estimate parameters from pilot data
   params <- .estimate_dependent_censoring_params(
      pilot_data, time_var, status_var, arm_var, dep_cens_status_var, linear_terms, L
   )

   # 2) Power across N
   cat("--- Calculating power for specified sample sizes... ---\n")
   power_values <- .rmst_app_power_values(
      params, sample_sizes, 2, alpha,
      se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
      point_cb = point_cb
   )
   results_df <- data.frame(N_per_Arm = sample_sizes, Power = power_values)

   # 3) Plot + return
   p <- .rmst_app_power_plot(
      results_df, "N_per_Arm", "#0072B2",
      title = "Analytic Power Curve: RMST with Covariate-Dependent Censoring (IPCW)",
      subtitle = if (test == "wls")
         "Weighted least-squares t-test; sandwich sampling variance. Single censoring mechanism."
      else "Sandwich Wald test. Single censoring mechanism; no competing risks.",
      xlab = "Sample Size Per Arm"
   )

   results_summary <- data.frame(
      Statistic = "Assumed RMST Difference (from pilot)",
      Value = params$beta_effect
   )

   list(results_data = results_df, results_plot = p, results_summary = results_summary)
}
# Sample Size Search ------------------------------------------------------

#' @title Find Sample Size for RMST with Covariate-Dependent Censoring (Analytic)
#' @description Iterative per-arm N to reach target power using IPCW-based analytic variance.
#' @inheritParams .estimate_dependent_censoring_params
#' @inheritParams DC.power.analytical.app
#' @param target_power Desired power.
#' @param n_start Starting per-arm N.
#' @param n_step Step size for search.
#' @param max_n_per_arm Maximum per-arm N to search.
#' @return A list: \code{results_data}, \code{results_plot}, \code{results_summary}.
#' @keywords internal
#' @export
DC.ss.analytical.app <- function(pilot_data,
                                 time_var,
                                 status_var,
                                 arm_var,
                                 dep_cens_status_var,
                                 target_power,
                                 linear_terms = NULL,
                                 L,
                                 alpha = 0.05,
                                 n_start = 50,
                                 n_step = 25,
                                 max_n_per_arm = 2000,
                                 point_cb = NULL,
                                 test = c("wls", "sandwich")) {
   test <- match.arg(test)

   # 1) One-time estimation from pilot data
   params <- .estimate_dependent_censoring_params(
      pilot_data, time_var, status_var, arm_var, dep_cens_status_var, linear_terms, L
   )

   # 2) Iterative search
   cat("--- Searching for Sample Size (Method: Analytic) ---\n")
   search <- .rmst_app_ss_search(
      params, 2, target_power, alpha, n_start, n_step, max_n_per_arm, "/arm",
      se_test_n1 = if (test == "wls") params$se_beta_model_n1 else NULL,
      point_cb = point_cb
   )
   final_n <- search$final_n

   # 3) Finalize + return
   results_df <- data.frame(Target_Power = target_power, Required_N_per_Arm = final_n)
   search_path_df <- search$search_path_df
   names(search_path_df) <- c("N_per_Arm", "Power")

   p <- .rmst_app_ss_plot(
      search_path_df, "N_per_Arm", final_n, target_power,
      title = "Analytic Sample Size Search (RMST; Covariate-Dependent Censoring)",
      subtitle = "IPCW-based; single censoring mechanism.",
      xlab = "Sample Size Per Arm"
   )

   cat("\n--- Calculation Summary ---\n")
   print(knitr::kable(results_df, caption = "Required Sample Size"))

   results_summary <- data.frame(
      Statistic = "Assumed RMST Difference (from pilot)",
      Value = params$beta_effect
   )

   list(results_data = results_df, results_plot = p, results_summary = results_summary)
}
