# nocov start
# R/generate_app_method_datasets.R
# Build app-ready datasets for each analysis method: complete + missing variants.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generate App-Ready Demo Datasets for All Methods
#'
#' Creates two datasets per method used in the Shiny app:
#' one complete dataset and one dataset with controlled missingness in every column
#' (while ensuring no column is entirely missing).
#'
#' The output is designed for direct use in the deployed app upload workflow.
#'
#' @param raw_dir Directory where raw `.rda` artifacts are written.
#' @param data_dir Directory where package `.rda` datasets are written.
#' @param n Sample size per generated dataset.
#' @param seed Base random seed for reproducibility.
#' @param missing_frac Fraction of rows to set missing per column in "missing" datasets.
#' @return Invisibly returns a manifest data.frame of generated datasets.
#' @examples
#' \dontrun{
#' generate_app_method_datasets()
#' }
#' @export
generate_app_method_datasets <- function(
  raw_dir = "data-raw",
  data_dir = "data",
  n = 240L,
  seed = 20260222L,
  missing_frac = 0.08
) {
  if (!is.numeric(n) || length(n) != 1L || n < 30) stop("n must be a single number >= 30.")
  if (!is.numeric(seed) || length(seed) != 1L) stop("seed must be a single numeric value.")
  if (!is.numeric(missing_frac) || length(missing_frac) != 1L || missing_frac <= 0 || missing_frac >= 1) {
    stop("missing_frac must be in (0, 1).")
  }

  n <- as.integer(n)
  seed <- as.integer(seed)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  methods <- c(
    "Linear IPCW Model",
    "Additive Stratified Model",
    "Multiplicative Stratified Model",
    "Semiparametric (GAM) Model",
    "Dependent Censoring Model"
  )

  sanitize <- function(x) {
    y <- tolower(x)
    y <- gsub("[^a-z0-9]+", "_", y)
    y <- gsub("_+", "_", y)
    y <- sub("^_", "", y)
    y <- sub("_$", "", y)
    y
  }

  make_recipe <- function(method, rec_seed) {
    cov_defs <- list(
      list(name = "x1", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)),
      list(name = "x2", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)),
      list(name = "x3", type = "continuous", dist = "normal", params = list(mean = 0, sd = 1)),
      list(
        name = "strata",
        type = "categorical",
        dist = "categorical",
        params = list(prob = c(0.40, 0.35, 0.25), labels = c("S1", "S2", "S3"))
      )
    )

    recipe <- list(
      n = n,
      covariates = list(defs = cov_defs),
      treatment = list(assignment = "randomization", allocation = "1:1"),
      event_time = list(
        model = "aft_weibull",
        baseline = list(shape = 1.35, scale = 2.4),
        effects = list(
          intercept = 0,
          treatment = -0.20,
          formula = "~ x1 + x2 + x3 + strata",
          beta = c(0, 0.22, -0.16, 0.10, 0.08, -0.04)
        )
      ),
      censoring = list(mode = "target_overall", target = 0.28, admin_time = 3),
      seed = rec_seed
    )

    if (identical(method, "Semiparametric (GAM) Model")) {
      recipe$event_time$model <- "aft_lognormal"
      recipe$event_time$baseline <- list(mu = 1.2, sigma = 0.55)
      recipe$event_time$effects$treatment <- -0.16
      recipe$censoring$target <- 0.30
    }

    if (identical(method, "Dependent Censoring Model")) {
      recipe$censoring <- list(
        mode = "explicit",
        administrative = list(time = 3),
        random = list(dist = "exponential", params = list(rate = 0.06)),
        dependent = list(formula = "~ x1 + x2 + arm", beta = c(0, 0.35, -0.25, 0.20), base = 0.04)
      )
    }

    validate_recipe(recipe)
  }

  derive_dep_cens_status <- function(df, dep_seed) {
    set.seed(dep_seed)
    z1 <- as.numeric(scale(df$x1))
    z2 <- as.numeric(scale(df$x2))
    lp <- -0.9 + 0.8 * z1 - 0.6 * z2 + 0.3 * df$arm
    p <- stats::plogis(lp)
    as.integer(stats::runif(nrow(df)) < p)
  }

  inject_missing_each_column <- function(df, frac, miss_seed) {
    set.seed(miss_seed)
    out <- df
    n_rows <- nrow(out)
    k <- max(1L, as.integer(floor(n_rows * frac)))
    if (k >= n_rows) k <- n_rows - 1L
    for (nm in names(out)) {
      idx <- sample.int(n_rows, size = k, replace = FALSE)
      out[idx, nm] <- NA
    }
    out
  }

  manifest_rows <- list()
  m_ix <- 0L
  for (method in methods) {
    m_ix <- m_ix + 1L
    rec_seed <- seed + m_ix * 100L
    sim_seed <- seed + m_ix * 100L + 1L
    dep_seed <- seed + m_ix * 100L + 2L
    miss_seed <- seed + m_ix * 100L + 3L

    rec <- make_recipe(method, rec_seed)
    dat_complete <- simulate_from_recipe(rec, seed = sim_seed)
    dat_complete$status <- as.integer(dat_complete$status)
    dat_complete$arm <- as.integer(dat_complete$arm)
    dat_complete$strata <- as.factor(as.character(dat_complete$strata))
    dat_complete$dep_cens_status <- derive_dep_cens_status(dat_complete, dep_seed)

    dat_missing <- inject_missing_each_column(dat_complete, missing_frac, miss_seed)

    stopifnot(all(vapply(dat_missing, function(x) sum(is.na(x)) < length(x), logical(1))))
    stopifnot(all(vapply(dat_missing, function(x) any(is.na(x)), logical(1))))

    stub <- sanitize(method)
    complete_obj_name <- paste0(stub, "_complete")
    missing_obj_name <- paste0(stub, "_missing")
    complete_file <- file.path(raw_dir, paste0(complete_obj_name, ".rda"))
    missing_file <- file.path(raw_dir, paste0(missing_obj_name, ".rda"))
    assign(complete_obj_name, dat_complete)
    assign(missing_obj_name, dat_missing)
    save(list = complete_obj_name, file = complete_file, compress = "bzip2", version = 2)
    save(list = missing_obj_name, file = missing_file, compress = "bzip2", version = 2)
    rm(list = c(complete_obj_name, missing_obj_name))

    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      method = method,
      dataset_type = "complete",
      n = nrow(dat_complete),
      file_rda = complete_file,
      object_name = complete_obj_name,
      recommended_time_var = "time",
      recommended_status_var = "status",
      recommended_arm_var = "arm",
      recommended_strata_var = "strata",
      recommended_dep_cens_var = "dep_cens_status",
      stringsAsFactors = FALSE
    )
    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      method = method,
      dataset_type = "missing",
      n = nrow(dat_missing),
      file_rda = missing_file,
      object_name = missing_obj_name,
      recommended_time_var = "time",
      recommended_status_var = "status",
      recommended_arm_var = "arm",
      recommended_strata_var = "strata",
      recommended_dep_cens_var = "dep_cens_status",
      stringsAsFactors = FALSE
    )
  }

  manifest <- do.call(rbind, manifest_rows)
  app_method_datasets_manifest_raw <- manifest
  save(
    app_method_datasets_manifest_raw,
    file = file.path(raw_dir, "app_method_datasets_manifest_raw.rda"),
    compress = "bzip2",
    version = 2
  )

  # Save package data objects to data/
  method_datasets <- list()
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, , drop = FALSE]
    stub <- sanitize(row$method[[1]])
    obj_name <- paste0(stub, "_", row$dataset_type[[1]])
    e <- new.env(parent = emptyenv())
    load(row$file_rda[[1]], envir = e)
    method_datasets[[obj_name]] <- e[[row$object_name[[1]]]]
  }
  app_method_datasets_manifest <- manifest
  app_method_datasets <- method_datasets
  save(
    app_method_datasets_manifest,
    app_method_datasets,
    file = file.path(data_dir, "app_method_datasets.rda"),
    compress = "bzip2",
    version = 2
  )

  invisible(manifest)
}
# nocov end
