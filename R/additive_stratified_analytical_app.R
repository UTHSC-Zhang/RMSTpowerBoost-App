#' @title Internal Helper to Estimate Additive Stratified Model Parameters
#' @description This internal function contains the common logic for estimating the
#'   treatment effect and its variance from pilot data for the additive stratified
#'   model (Zhang & Schaubel 2024), ported from RMSTpowerBoost. It is called by both
#'   the power and sample size functions and by the app's Repeated method.
#'   Censoring weights come from a Cox model for censoring with treatment and
#'   \code{linear_terms} as predictors and the stratum as a Cox stratum, and are
#'   used as fitted, without capping. The effect is estimated in closed form by
#'   stratum-centering, with a sandwich variance.
#' @importFrom magrittr %>%
#' @return A list containing `beta_effect` (the estimated treatment effect),
#'   `se_beta_n1` (the sandwich standard error for a sample size of 1),
#'   `n_pilot` and `n_strata`.
#' @keywords internal
#' @export
.estimate_additive_stratified_params <- function(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L) {

  # --- 1. Prepare Data and Calculate IPCW Weights ---
  cat("--- Estimating parameters from pilot data... ---\n")
  covariates <- c(arm_var, linear_terms)
  all_vars <- c(time_var, status_var, strata_var, covariates)
  df <- .rmst_prepare_df(pilot_data, all_vars, time_var, status_var, L)
  n_pilot <- nrow(df)

  # App-side input checks, reported before any model is fitted.
  .check_events_per_stratum(df, status_var, strata_var)
  if (!is.null(linear_terms)) {
    for (cov in linear_terms) {
      if (!is.numeric(df[[cov]])) {
        stop(paste0("Covariate '", cov, "' must be numeric, but its type is '", class(df[[cov]]), "'. Please check the column mapping in the UI."), call. = FALSE)
      }
    }
  }

  # Already Delta_Y * W_hat, uncapped.
  df$weights <- .ipcw_stratified_cox_weights(df, time_var, status_var,
                                             covariates, strata_var)

  # --- 2. Estimate Beta via Stratum-Centering ---
  cat("--- Estimating additive effect via stratum-centering... ---\n")
  vars_to_center <- c("Y_rmst", covariates)
  stratum_means <- df %>%
    dplyr::group_by(.data[[strata_var]]) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars_to_center),
        ~ stats::weighted.mean(.x, w = weights, na.rm = TRUE)
      ),
      .groups = 'drop'
    )
  names(stratum_means) <- c(strata_var, paste0(vars_to_center, "_mean"))

  df_centered <- df %>% dplyr::left_join(stratum_means, by = strata_var)
  for (cov in vars_to_center) {
    df_centered[[paste0(cov, "_tilde")]] <- df_centered[[cov]] - df_centered[[paste0(cov, "_mean")]]
  }

  Z_tilde <- as.matrix(df_centered[, paste0(covariates, "_tilde")])
  Y_tilde <- df_centered[["Y_rmst_tilde"]]
  W <- df_centered$weights

  A_hat_num <- crossprod(Z_tilde * sqrt(W))
  dimnames(A_hat_num) <- list(covariates, covariates)
  A_hat <- A_hat_num / n_pilot

  A_hat_inv <- tryCatch({
    solve(A_hat)
  }, error = function(e) {
    stop("The covariate matrix (A_hat) is singular and cannot be inverted.\nThis may be caused by a lack of variation in covariates among subjects with an event within one or more strata.", call. = FALSE)
  })

  beta_hat <- (A_hat_inv / n_pilot) %*% (t(Z_tilde * W) %*% Y_tilde)
  rownames(beta_hat) <- covariates
  beta_effect <- beta_hat[arm_var, 1]

  # --- 3. Calculate Asymptotic Sandwich Variance ---
  cat("--- Calculating asymptotic variance... ---\n")
  mu0_hats <- stratum_means %>%
    dplyr::mutate(
      Z_matrix = as.matrix(dplyr::select(., dplyr::all_of(paste0(covariates, "_mean")))),
      mu0_hat = .data[["Y_rmst_mean"]] - Z_matrix %*% beta_hat
    ) %>%
    dplyr::select(dplyr::all_of(strata_var), mu0_hat)

  df_final <- df_centered %>% dplyr::left_join(mu0_hats, by = strata_var)
  Z_matrix <- as.matrix(df_final[, covariates])
  df_final$residuals <- df_final$Y_rmst - (df_final$mu0_hat + as.vector(Z_matrix %*% beta_hat))

  epsilon <- apply(Z_tilde, 2, function(z_col) z_col * W * df_final$residuals)
  B_hat <- crossprod(epsilon) / n_pilot
  dimnames(B_hat) <- list(covariates, covariates)

  V_hat_n <- A_hat_inv %*% B_hat %*% t(A_hat_inv)

  list(
    beta_effect = beta_effect,
    se_beta_n1 = sqrt(V_hat_n[arm_var, arm_var]),
    n_pilot = n_pilot,
    n_strata = length(unique(df[[strata_var]]))
  )
}

#' @title Analyze Power for a Stratified Additive RMST Model (Analytic)
#' @description Performs power analysis for a stratified, additive RMST model. The
#'   power refers to the sandwich Wald test of the stratum-centered estimator.
#' @inheritParams .estimate_additive_stratified_params
#' @param sample_sizes A numeric vector of sample sizes *per stratum* to calculate power for.
#' @param alpha The significance level (Type I error rate).
#' @param point_cb Optional callback \code{function(n, power)} for live plotting.
#' @return A list containing the results.
#' @keywords internal
#' @export
additive.power.analytical.app <- function(pilot_data, time_var, status_var, arm_var, strata_var,
                                      sample_sizes, linear_terms = NULL, L, alpha = 0.05,
                                      point_cb = NULL) {

  # 1. Estimate parameters from pilot data using the helper function
  params <- .estimate_additive_stratified_params(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L)

  # 2. Calculate Power
  cat("--- Calculating power for specified sample sizes... ---\n")
  power_values <- .rmst_app_power_values(params, sample_sizes, params$n_strata, alpha,
                                         point_cb = point_cb)
  results_df <- data.frame(N_per_Stratum = sample_sizes, Power = power_values)

  # 3. Create plot and summary
  results_summary <- data.frame(
    Statistic = "Assumed RMST Difference (from pilot)",
    Value = params$beta_effect
  )
  p <- .rmst_app_power_plot(
    results_df, "N_per_Stratum", "#0072B2",
    title = "Analytic Power Curve: Additive Stratified RMST Model",
    subtitle = "Based on stratum-centered estimating equations.",
    xlab = "Sample Size Per Stratum"
  )

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}


#' @title Find Sample Size for a Stratified Additive RMST Model (Analytic)
#' @description Calculates the required sample size for a target power.
#' @inheritParams .estimate_additive_stratified_params
#' @inheritParams additive.power.analytical.app
#' @param target_power A single numeric value for the desired power.
#' @param n_start The starting sample size *per stratum* for the search.
#' @param n_step The increment in sample size at each step of the search.
#' @param max_n_per_arm The maximum sample size *per stratum* to search up to.
#' @return A list containing the results.
#' @keywords internal
#' @export
additive.ss.analytical.app <- function(pilot_data, time_var, status_var, arm_var, strata_var,
                                   target_power, linear_terms = NULL, L, alpha = 0.05,
                                   n_start = 50, n_step = 25, max_n_per_arm = 2000,
                                   point_cb = NULL) {

  # 1. Estimate parameters from pilot data using the helper function
  params <- .estimate_additive_stratified_params(pilot_data, time_var, status_var, arm_var, strata_var, linear_terms, L)

  # 2. Iterative Search for Sample Size
  cat("--- Searching for Sample Size (Method: Additive Analytic) ---\n")
  search <- .rmst_app_ss_search(params, params$n_strata, target_power, alpha,
                                n_start, n_step, max_n_per_arm, "/stratum",
                                point_cb = point_cb)
  final_n <- search$final_n

  # 3. Finalize and Return Results
  results_summary <- data.frame(Statistic = "Assumed RMST Difference (from pilot)", Value = params$beta_effect)
  results_df <- data.frame(Target_Power = target_power, Required_N_per_Stratum = final_n)
  search_path_df <- search$search_path_df
  names(search_path_df) <- c("N_per_Stratum", "Power")

  p <- .rmst_app_ss_plot(
    search_path_df, "N_per_Stratum", final_n, target_power,
    title = "Analytic Sample Size Search: Additive Stratified RMST Model",
    subtitle = "Power calculated from formula at each step.",
    xlab = "Sample Size Per Stratum"
  )

  cat("\n--- Calculation Summary ---\n")
  print(knitr::kable(results_df, caption = "Required Sample Size"))

  list(results_data = results_df, results_plot = p, results_summary = results_summary)
}
