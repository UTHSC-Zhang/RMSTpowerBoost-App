# estimation_helpers_app.R
# Shared machinery for the analytical engines, ported from RMSTpowerBoost's
# R/estimation-helpers.R so that the app and the package estimate, weight and
# compute power identically. Each engine's .estimate_*_params() helper is used
# by both its power and sample-size wrapper and by the app's Repeated method.
#
# IPCW weights are used exactly as fitted: no capping, truncation or floor on
# the censoring survival. Zhang & Schaubel (2024), Eqs. (11)-(12), derive the
# estimator and its simplified sandwich under the fitted weights, so any cap
# would break the correspondence with the published formulas and compress the
# upper tail that IPCW exists to up-weight.

# --- Common data preparation ------------------------------------------------

#' Truncate follow-up at L and flag events / complete observations
#' @noRd
.rmst_prepare_df <- function(pilot_data, all_vars, time_var, status_var, L) {
  df <- pilot_data[stats::complete.cases(pilot_data[, all_vars, drop = FALSE]), ]
  df$Y_rmst <- pmin(df[[time_var]], L)
  df$is_event <- df[[status_var]] == 1
  df$is_complete <- df$is_event | df[[time_var]] >= L
  df
}

#' Require a minimum number of events in every stratum
#'
#' App-side input check, kept so the UI reports an actionable message before
#' the censoring model is fitted.
#' @noRd
.check_events_per_stratum <- function(df, status_var, strata_var, min_events = 2) {
  event_counts <- stats::aggregate(stats::as.formula(paste(status_var, "~", strata_var)),
                                   data = df, FUN = sum)
  problem_strata <- event_counts[event_counts[[status_var]] < min_events, ]
  if (nrow(problem_strata) > 0) {
    stop(paste(
      "Insufficient events in one or more strata. Each stratum must have at least",
      min_events, "events.",
      "Problematic strata:", paste(problem_strata[[strata_var]], collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# --- Power -------------------------------------------------------------------

#' Wald power at a given total sample size
#'
#' `se_beta_n1` is the n = 1 standard deviation of the estimator and always sets
#' the distribution of the statistic. `se_test_n1` is the n = 1 standard error the
#' planned test uses to build its rejection threshold. When the planned test uses
#' a different variance estimator from the one that describes the estimator's
#' variability, as the weighted-least-squares t-test does (IPCW weights are
#' sampling weights, not inverse-variance weights), the rejection region comes
#' from `se_test_n1` and the sampling distribution from `se_beta_n1`. `NULL`
#' makes the two coincide and recovers the usual sandwich Wald formula.
#' @noRd
.rmst_wald_power <- function(beta_effect, se_beta_n1, total_n, z_alpha,
                             se_test_n1 = NULL) {
  se_final <- se_beta_n1 / sqrt(total_n)
  if (is.null(se_test_n1)) {
    return(stats::pnorm((abs(beta_effect) / se_final) - z_alpha))
  }
  critical_value <- z_alpha * se_test_n1 / sqrt(total_n)
  stats::pnorm((abs(beta_effect) - critical_value) / se_final) +
    stats::pnorm((-abs(beta_effect) - critical_value) / se_final)
}

#' Power at each requested sample size, streaming points to `point_cb`
#' @noRd
.rmst_app_power_values <- function(params, sample_sizes, group_multiplier, alpha,
                                   se_test_n1 = NULL, point_cb = NULL) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  vapply(sample_sizes, function(n_per_group) {
    power <- .rmst_wald_power(params$beta_effect, params$se_beta_n1,
                              n_per_group * group_multiplier, z_alpha, se_test_n1)
    if (is.function(point_cb)) {
      try(point_cb(n_per_group, power), silent = TRUE)
    }
    power
  }, numeric(1))
}

#' Iterative sample-size search on the analytic power formula
#'
#' Mirrors RMSTpowerBoost's .rmst_analytic_ss_search(), with the app's progress
#' lines and live-plot callback.
#' @noRd
.rmst_app_ss_search <- function(params, group_multiplier, target_power, alpha,
                                n_start, n_step, max_n, group_label,
                                se_test_n1 = NULL, point_cb = NULL) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  current_n <- n_start
  search_path <- list()
  final_n <- NA_integer_
  while (current_n <= max_n) {
    calculated_power <- .rmst_wald_power(params$beta_effect, params$se_beta_n1,
                                         current_n * group_multiplier, z_alpha,
                                         se_test_n1)
    if (!is.finite(calculated_power)) calculated_power <- 0
    search_path[[as.character(current_n)]] <- calculated_power
    if (is.function(point_cb)) {
      try(point_cb(current_n, calculated_power), silent = TRUE)
    }
    cat(paste0("  N = ", current_n, group_label, ", Calculated Power = ",
               round(calculated_power, 3), "\n"))
    if (calculated_power >= target_power) {
      final_n <- current_n
      break
    }
    current_n <- current_n + n_step
  }
  if (is.na(final_n)) {
    warning(paste("Target power", target_power, "not achieved by max N of", max_n),
            call. = FALSE)
    final_n <- max_n
  }
  list(final_n = final_n,
       search_path_df = data.frame(N = as.integer(names(search_path)),
                                   Power = as.numeric(unlist(search_path))))
}

# --- Shared plotting (app styling) -------------------------------------------

#' Power-curve plot shared by the analytical engines
#' @noRd
.rmst_app_power_plot <- function(results_df, x_var, color, title, subtitle, xlab) {
  ggplot2::ggplot(results_df, ggplot2::aes(x = .data[[x_var]], y = .data[["Power"]])) +
    ggplot2::geom_line(color = color, linewidth = 1) +
    ggplot2::geom_point(color = color, size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("N=%s\nP=%.3f", .data[[x_var]], .data[["Power"]])),
      vjust = -0.6, size = 3, color = color, check_overlap = TRUE
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::coord_cartesian(ylim = c(0, 1.05), clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = "Estimated Power") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 20, 10, 10))
}

#' Sample-size search-path plot shared by the analytical engines
#' @noRd
.rmst_app_ss_plot <- function(search_path_df, x_var, final_n, target_power,
                              title, subtitle, xlab) {
  ggplot2::ggplot(stats::na.omit(search_path_df),
                  ggplot2::aes(x = .data[[x_var]], y = .data[["Power"]])) +
    ggplot2::geom_line(color = "#009E73", linewidth = 1) +
    ggplot2::geom_point(color = "#009E73", size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("N=%s\nP=%.3f", .data[[x_var]], .data[["Power"]])),
      vjust = -0.6, size = 3, color = "#009E73", check_overlap = TRUE
    ) +
    ggplot2::geom_hline(yintercept = target_power, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = final_n, linetype = "dotted", color = "blue") +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::coord_cartesian(ylim = c(0, 1.05), clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = "Calculated Power") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 20, 10, 10))
}

# --- Censoring models ----------------------------------------------------------

#' Fit a censoring Cox model, turning non-convergence into a clear error
#' @noRd
.fit_censoring_cox <- function(cens_formula, df) {
  tryCatch(
    withCallingHandlers(
      survival::coxph(cens_formula, data = df, ties = "breslow"),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("did not converge|ran out of iterations|infinite", msg, ignore.case = TRUE)) {
          stop("Not enough data points to fit the censoring model: the Cox model for censoring did not converge.", call. = FALSE)
        }
      }
    ),
    error = function(e) {
      stop("Not enough data points to fit the censoring model: ", conditionMessage(e), call. = FALSE)
    }
  )
}

#' Per-subject baseline cumulative censoring hazard at Y_rmst, by stratum
#'
#' basehaz() labels strata either as "level" or "strata_var=level" depending
#' on the survival package version; accept both and fail loudly if neither
#' matches, since silently missing strata would zero out the censoring hazard.
#' @noRd
.strat_cum_hazard <- function(df, bh_cens, strata_var) {
  H_cens <- numeric(nrow(df))
  bh_labels <- if (is.null(bh_cens$strata)) NULL else as.character(bh_cens$strata)
  for (st in unique(df[[strata_var]])) {
    is_stratum <- df[[strata_var]] == st
    is_bh_stratum <- if (is.null(bh_labels)) rep(TRUE, nrow(bh_cens)) else
      bh_labels == as.character(st) | bh_labels == paste0(strata_var, "=", st)
    if (sum(is_bh_stratum) == 0) {
      stop("Could not locate the baseline censoring hazard for stratum '", st,
           "'; IPCW weights cannot be computed.", call. = FALSE)
    }
    H_cens[is_stratum] <- stats::stepfun(
      bh_cens$time[is_bh_stratum],
      c(0, bh_cens$hazard[is_bh_stratum]))(df$Y_rmst[is_stratum])
  }
  H_cens
}

#' IPCW weights Delta_Y / G(Y) from a stratified Cox censoring model
#'
#' Requires `df` from .rmst_prepare_df(). Returns weights already multiplied by
#' Delta_Y, so they are zero for subjects whose truncated outcome is not observed.
#' @noRd
.ipcw_stratified_cox_weights <- function(df, time_var, status_var, covariates,
                                         strata_var) {
  # With no observed censoring, G is identically one and no Cox fit is needed.
  if (all(df[[status_var]] == 1)) {
    g <- rep(1, nrow(df))
  } else {
    cens_formula <- stats::as.formula(paste0(
      "survival::Surv(", time_var, ", ", status_var, " == 0) ~ ",
      paste(covariates, collapse = " + "),
      " + survival::strata(", strata_var, ")"))
    fit_cens <- .fit_censoring_cox(cens_formula, df)
    # Uncentered baseline hazard with an uncentered linear predictor: the two
    # centerings must match.
    bh_cens <- survival::basehaz(fit_cens, centered = FALSE)
    H_cens <- .strat_cum_hazard(df, bh_cens, strata_var)
    lp <- stats::predict(fit_cens, newdata = df, type = "lp", reference = "zero")
    g <- exp(-H_cens * exp(lp))
    if (any(!is.finite(g) | g <= 0 | g > 1)) {
      stop("Invalid Cox censoring survival probabilities; IPCW weights cannot be computed.",
           call. = FALSE)
    }
  }
  w <- as.numeric(df$is_complete) / g
  w[!is.finite(w)] <- 0
  w
}

#' Cox censoring weights for the linear IPCW engine
#'
#' Censoring model with treatment and `linear_terms` as predictors, fitted on the
#' original follow-up and evaluated at each subject's truncated outcome.
#' predict(type = "survival") keeps the baseline and linear-predictor centering
#' consistent and returns probabilities in newdata row order.
#' @noRd
.linear_cox_weights <- function(df, time_var, status_var, arm_var, linear_terms, L) {
  y <- pmin(df[[time_var]], L)
  complete <- df[[status_var]] == 1 | df[[time_var]] >= L
  if (all(df[[status_var]] == 1)) {
    g <- rep(1, nrow(df))
  } else {
    cens_formula <- stats::as.formula(paste0(
      "survival::Surv(", time_var, ", ", status_var, " == 0) ~ ",
      paste(unique(c(arm_var, linear_terms)), collapse = " + ")))
    fit <- .fit_censoring_cox(cens_formula, df)
    newdata <- df
    newdata[[time_var]] <- y
    g <- as.numeric(stats::predict(fit, newdata = newdata, type = "survival"))
    if (any(!is.finite(g) | g <= 0 | g > 1)) {
      stop("Invalid Cox censoring survival probabilities; IPCW weights cannot be computed.",
           call. = FALSE)
    }
  }
  w <- as.numeric(complete) / g
  w[!is.finite(w)] <- 0
  w
}

#' IPCW weights from a covariate-only Cox censoring model (dependent censoring)
#'
#' The censoring model uses `linear_terms` only, without treatment.
#' predict(type = "survival") fails on the intercept-only model that no
#' covariates produce, so the uncentered baseline hazard is paired explicitly
#' with an uncentered linear predictor; pairing it with the default (centered)
#' predictor misstates G whenever a covariate has a non-zero mean.
#' Requires `df` from .rmst_prepare_df().
#' @noRd
.dc_cox_weights <- function(df, time_var, status_var, linear_terms) {
  if (all(df[[status_var]] == 1)) {
    g <- rep(1, nrow(df))
  } else {
    has_covariates <- length(linear_terms) > 0
    cens_rhs <- if (has_covariates) paste(linear_terms, collapse = " + ") else "1"
    cens_formula <- stats::as.formula(paste0(
      "survival::Surv(", time_var, ", ", status_var, " == 0) ~ ", cens_rhs))
    fit_cens <- .fit_censoring_cox(cens_formula, df)
    bh_cens <- survival::basehaz(fit_cens, centered = FALSE)
    H_cens <- stats::stepfun(bh_cens$time, c(0, bh_cens$hazard))(df$Y_rmst)
    lp <- if (has_covariates)
      as.numeric(stats::predict(fit_cens, newdata = df, type = "lp", reference = "zero")) else 0
    g <- exp(-H_cens * exp(lp))
    if (any(!is.finite(g) | g <= 0 | g > 1)) {
      stop("Invalid Cox censoring survival probabilities; IPCW weights cannot be computed.",
           call. = FALSE)
    }
  }
  w <- as.numeric(df$is_complete) / g
  w[!is.finite(w)] <- 0
  w
}
