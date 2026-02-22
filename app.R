# app.R - RMSTpowerBoost (single file, no external Rmd required)

# ------------------ Packages ------------------
packages <- c(
  "shiny","shinyjs","bslib","DT","ggplot2","plotly","survival",
  "kableExtra","magrittr","rmarkdown","dplyr","tidyr","purrr","stringr","tibble"
)
missing_pkgs <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(
    "Missing required package(s): ",
    paste(missing_pkgs, collapse = ", "),
    ". Please install them before launching the app.",
    call. = FALSE
  )
}
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}))

# ------------------ App Root / Repo Guard ------------------
app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
app_root <- if (!is.null(app_file) && nzchar(app_file)) {
  normalizePath(dirname(app_file), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
is_app_repo <- file.exists(file.path(app_root, "RMSTpowerBoost-App.Rproj")) ||
  file.exists(file.path(app_root, "RMSTpowerBoost-App.code-workspace"))
if (interactive() && !is_app_repo) {
  stop(
    "Run the app from the RMSTpowerBoost-App repo root (where app.R lives). ",
    "Current working directory: ", app_root,
    call. = FALSE
  )
}
pipeline_html_path <- function() {
  p1 <- file.path(app_root, "www", "pipeline.html")
  if (file.exists(p1)) return(p1)
  ""
}

# ------------------ Source your R/ scripts (simulation engine etc.) ------------------
app_r_dir <- file.path(app_root, "R")
if (dir.exists(app_r_dir)) {
  r_files <- sort(list.files(app_r_dir, pattern = "\\.R$", full.names = TRUE))
  if (!length(r_files)) {
    if (requireNamespace("RMSTpowerBoostApp", quietly = TRUE)) {
      suppressPackageStartupMessages(library(RMSTpowerBoostApp))
    } else {
      stop("No R scripts found under 'R/'. App helpers could not be loaded.", call. = FALSE)
    }
  } else {
    for (rf in r_files) {
      tryCatch(
        source(rf, local = FALSE),
        error = function(e) {
          stop("Failed to source helper script '", rf, "': ", conditionMessage(e), call. = FALSE)
        }
      )
    }
    cat("All R scripts in the 'R/' directory have been sourced.\n")
  }
} else {
  if (requireNamespace("RMSTpowerBoostApp", quietly = TRUE)) {
    suppressPackageStartupMessages(library(RMSTpowerBoostApp))
  } else {
    stop("No R/ directory found and RMSTpowerBoostApp package is not available.", call. = FALSE)
  }
}

`%||%` <- function(x,y) if (is.null(x)) y else x

# ------------------ Helpers ------------------
coerce_time_positive <- function(x, field_name = "time") {
  xn <- suppressWarnings(as.numeric(x))
  bad <- is.na(xn) | !is.finite(xn) | xn <= 0
  if (any(bad)) {
    stop(sprintf("'%s' must contain only finite values > 0 with no missing values.", field_name), call. = FALSE)
  }
  xn
}

coerce_status_binary <- function(x, field_name = "status") {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    xl <- tolower(trimws(x))
    one_labels <- c("1", "event", "events", "dead", "death", "yes", "true")
    zero_labels <- c("0", "censor", "censored", "alive", "no", "false")
    out <- rep(NA_integer_, length(xl))
    out[xl %in% one_labels] <- 1L
    out[xl %in% zero_labels] <- 0L
    bad <- is.na(out)
    if (any(bad)) {
      stop(sprintf("'%s' must be binary (0/1 or recognized event/censor labels) with no missing values.", field_name), call. = FALSE)
    }
    return(out)
  }
  xn <- suppressWarnings(as.numeric(x))
  if (any(is.na(xn) | !is.finite(xn))) {
    stop(sprintf("'%s' must be binary (0/1) with no missing values.", field_name), call. = FALSE)
  }
  if (!all(unique(xn) %in% c(0, 1))) {
    stop(sprintf("'%s' must be coded as 0/1 (or recognized binary labels).", field_name), call. = FALSE)
  }
  as.integer(xn)
}

coerce_arm_binary <- function(x, field_name = "arm") {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    xl <- tolower(trimws(x))
    one_labels <- c("1", "treatment", "treated", "treat", "tx", "experimental", "active")
    zero_labels <- c("0", "control", "placebo", "comparator", "standard")
    out <- rep(NA_integer_, length(xl))
    out[xl %in% one_labels] <- 1L
    out[xl %in% zero_labels] <- 0L
    if (all(!is.na(out))) return(out)
    lev <- unique(xl[!is.na(xl) & nzchar(xl)])
    if (length(lev) == 2L) {
      mapped <- ifelse(xl == lev[1], 0L, ifelse(xl == lev[2], 1L, NA_integer_))
      if (any(is.na(mapped))) {
        stop(sprintf("'%s' contains missing/invalid values after binary mapping.", field_name), call. = FALSE)
      }
      return(mapped)
    }
    stop(sprintf("'%s' must have exactly 2 groups.", field_name), call. = FALSE)
  }
  xn <- suppressWarnings(as.numeric(x))
  if (any(is.na(xn) | !is.finite(xn))) {
    stop(sprintf("'%s' must have finite non-missing values.", field_name), call. = FALSE)
  }
  lev <- sort(unique(xn))
  if (identical(lev, c(0, 1))) return(as.integer(xn))
  if (length(lev) == 2L) return(as.integer(ifelse(xn == lev[1], 0L, 1L)))
  stop(sprintf("'%s' must have exactly 2 groups.", field_name), call. = FALSE)
}

safe_output_id <- function(prefix, label) {
  raw <- ifelse(is.null(label), "", as.character(label))
  if (!nzchar(raw)) raw <- "var"
  paste0(prefix, make.names(raw, unique = TRUE))
}

reset_cov_builder_ui <- function(session) {
  updateTextInput(session, "cov_name", value = "")
  updateSelectInput(session, "cov_type", selected = "continuous")
  updateSelectInput(session, "cont_dist", selected = "normal")
  updateNumericInput(session, "cont_beta", value = 0)
  updateNumericInput(session, "tf_center", value = 0)
  updateNumericInput(session, "tf_scale", value = 1)
}

reset_cat_entry_ui <- function(session) {
  updateTextInput(session, "cat_add_name", value = "")
  updateNumericInput(session, "cat_add_prob", value = NA)
  updateNumericInput(session, "cat_add_coef", value = 0)
}




# --- Transparency helpers ---
transparent_theme <- function(base_size = 11) {
  theme_light(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = NA, color = NA),
      plot.background  = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      legend.box.background = element_rect(fill = NA, color = NA)
    )
}

to_plotly_clear <- function(p) {
  plotly::ggplotly(p) %>%
    plotly::layout(
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor  = 'rgba(0,0,0,0)'
    )
}

as_plotly_clear <- function(p) {
  if (inherits(p, "plotly")) {
    return(plotly::layout(
      p,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ))
  }
  to_plotly_clear(p)
}

format_num3 <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  txt <- sprintf("%.3f", x[ok])
  txt <- sub("0+$", "", txt)
  txt <- sub("\\.$", "", txt)
  out[ok] <- txt
  out
}

format_df_3dp <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  out <- df
  num_cols <- vapply(out, is.numeric, logical(1))
  out[num_cols] <- lapply(out[num_cols], format_num3)
  out
}
# Put a title above every subplot domain (for plotly::subplot)
add_subplot_titles <- function(p, titles, vgap = 0.04, fontsize = 14){
  xaxs <- grep("^xaxis", names(p$x$layout), value = TRUE)
  yaxs <- grep("^yaxis", names(p$x$layout), value = TRUE)
  anns <- vector("list", length(titles))
  for (i in seq_along(titles)) {
    xd <- p$x$layout[[xaxs[i]]]$domain
    yd <- p$x$layout[[yaxs[i]]]$domain
    anns[[i]] <- list(
      text = titles[i],
      x = mean(xd), y = yd[2] + vgap,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = fontsize)
    )
  }
  plotly::layout(p, annotations = c(p$x$layout$annotations, anns))
}

DT_25 <- function(df) {
  DT::datatable(format_df_3dp(df), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
}

as_factor_safe <- function(x) { if (is.factor(x)) x else factor(x) }

# HTML table helper that gracefully degrades if kableExtra isn't available
kable_html_safe <- function(df, caption = NULL, include_rownames = TRUE) {
  df <- format_df_3dp(df)
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    kableExtra::kbl(df, "html", caption = caption, row.names = include_rownames) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped","hover","condensed"), full_width = FALSE) %>%
      HTML()
  } else {
    HTML(knitr::kable(df, "html", caption = caption, row.names = include_rownames))
  }
}

# Convert hex color to rgba string with alpha
hex_to_rgba <- function(hex, alpha = 0.3) {
  hex <- gsub("#","", hex)
  if (nchar(hex) == 3) {
    hex <- paste0(rep(strsplit(hex, "")[[1]], each = 2), collapse = "")
  }
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, alpha)
}

# Build KM plotly directly to avoid ggplotly ribbon artifacts
km_plot_plotly <- function(fit, conf.int = TRUE, conf.int.alpha = 0.3, conf.level = 0.95,
                           palette = NULL, legend.title = NULL, legend.labs = NULL,
                           xlab = NULL, ylab = "Survival probability", title = NULL,
                           showlegend = TRUE) {
  sf <- summary(fit, conf.int = conf.int, conf.level = conf.level)
  df <- data.frame(
    time = sf$time,
    surv = sf$surv,
    lower = sf$lower,
    upper = sf$upper,
    strata = if (is.null(sf$strata)) "All" else as.character(sf$strata),
    stringsAsFactors = FALSE
  )
  df$strata <- sub("^.*=", "", df$strata)
  if (!is.null(legend.labs) && length(unique(df$strata)) == length(legend.labs)) {
    df$strata <- factor(df$strata, levels = unique(df$strata), labels = legend.labs)
  } else {
    df$strata <- factor(df$strata, levels = unique(df$strata))
  }
  # Ensure each KM curve starts at (time=0, survival=1) so arms share a proper baseline.
  strata_levels <- levels(df$strata)
  starter <- data.frame(
    time = rep(0, length(strata_levels)),
    surv = rep(1, length(strata_levels)),
    lower = rep(1, length(strata_levels)),
    upper = rep(1, length(strata_levels)),
    strata = factor(strata_levels, levels = strata_levels),
    stringsAsFactors = FALSE
  )
  df <- rbind(starter, df)
  
  groups <- levels(df$strata)
  if (is.null(palette) || length(palette) == 0) palette <- c("#0d6efd", "#dc3545")
  pal <- rep(palette, length.out = length(groups))
  
  p <- plotly::plot_ly()
  for (i in seq_along(groups)) {
    g <- groups[i]
    d <- df[df$strata == g, , drop = FALSE]
    d <- d[order(d$time), , drop = FALSE]
    col <- pal[i]
    
    if (conf.int) {
      # Draw CI as step-wise band between lower and upper curves.
      # Using tonexty avoids polygon-closure triangles at the tail.
      p <- plotly::add_trace(
        p,
        x = d$time,
        y = d$lower,
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(0,0,0,0)", width = 0, shape = "hv"),
        hoverinfo = "skip",
        showlegend = FALSE,
        legendgroup = as.character(g)
      )
      p <- plotly::add_trace(
        p,
        x = d$time,
        y = d$upper,
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(0,0,0,0)", width = 0, shape = "hv"),
        fill = "tonexty",
        fillcolor = hex_to_rgba(col, conf.int.alpha),
        hoverinfo = "skip",
        showlegend = FALSE,
        legendgroup = as.character(g)
      )
    }
    
    p <- plotly::add_trace(
      p,
      x = d$time,
      y = d$surv,
      type = "scatter",
      mode = "lines",
      line = list(color = col, width = 2, shape = "hv"),
      name = as.character(g),
      legendgroup = as.character(g),
      showlegend = showlegend
    )
  }
  
  p <- plotly::layout(
    p,
    title = list(text = title %||% "", x = 0.02, xanchor = "left"),
    xaxis = list(title = xlab %||% ""),
    # keep a tiny headroom above 1 so flat segments at survival=1 are visible
    yaxis = list(title = ylab, range = c(0, 1.02)),
    legend = list(title = list(text = legend.title %||% "")),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)"
  )
  
  p
}

# KM plot helper that avoids a hard dependency on survminer
km_plot_gg <- function(fit, data, conf.int = TRUE, conf.int.alpha = 0.3, conf.level = 0.95,
                       palette = NULL, legend.title = NULL, legend.labs = NULL,
                       xlab = NULL, ylab = "Survival probability", ggtheme = transparent_theme(),
                       use_survminer = FALSE) {
  if (use_survminer && requireNamespace("survminer", quietly = TRUE)) {
    p <- survminer::ggsurvplot(
      fit, data = data,
      conf.int = conf.int, conf.int.alpha = conf.int.alpha, conf.int.style = "ribbon",
      conf.level = conf.level,
      palette = palette,
      legend.title = legend.title,
      legend.labs  = legend.labs,
      xlab = xlab,
      ylab = ylab,
      ggtheme = ggtheme
    )$plot
    return(p)
  }
  
  sf <- summary(fit, conf.int = conf.int, conf.level = conf.level)
  df <- data.frame(
    time = sf$time,
    surv = sf$surv,
    lower = sf$lower,
    upper = sf$upper,
    strata = if (is.null(sf$strata)) "All" else as.character(sf$strata),
    stringsAsFactors = FALSE
  )
  df$strata <- sub("^.*=", "", df$strata)
  if (!is.null(legend.labs) && length(unique(df$strata)) == length(legend.labs)) {
    df$strata <- factor(df$strata, levels = unique(df$strata), labels = legend.labs)
  } else {
    df$strata <- factor(df$strata, levels = unique(df$strata))
  }
  
  df <- df[order(df$strata, df$time), , drop = FALSE]
  
  lab_args <- list(x = xlab %||% "", y = ylab, color = legend.title)
  if (conf.int) lab_args$fill <- legend.title
  
  p <- ggplot(df, aes(x = time, y = surv, color = strata, group = strata)) +
    do.call(labs, lab_args) +
    ggtheme
  
  if (conf.int) {
    p <- p + geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata, group = strata),
                         alpha = conf.int.alpha, color = NA)
  }
  p <- p + geom_step(size = 1)
  
  if (!is.null(palette)) {
    pal <- rep(palette, length.out = nlevels(df$strata))
    p <- p + scale_color_manual(values = pal) + scale_fill_manual(values = pal)
  }
  
  p
}

# Summaries that never error
covariate_summary <- function(df, arm_var = NULL) {
  if (is.null(df) || !nrow(df)) return(list())
  ignore <- c("time","status",arm_var)
  vars <- setdiff(names(df), ignore)
  if (!length(vars)) return(list())
  is_num <- sapply(df[vars], is.numeric)
  cont_vars <- vars[is_num]
  cat_vars  <- vars[!is_num]
  out <- list()
  if (length(cont_vars)) {
    cont_tab <- purrr::map_df(cont_vars, function(v) {
      x <- suppressWarnings(as.numeric(df[[v]]))
      tibble::tibble(
        Variable = v,
        Mean = round(mean(x, na.rm = TRUE), 3),
        SD   = round(stats::sd(x, na.rm = TRUE), 3),
        Min  = round(min(x, na.rm = TRUE), 3),
        Q1   = round(stats::quantile(x, 0.25, na.rm = TRUE), 3),
        Median  = round(stats::median(x, na.rm = TRUE), 3),
        Q3   = round(stats::quantile(x, 0.75, na.rm = TRUE), 3),
        Max  = round(max(x, na.rm = TRUE), 3),
        N_Missing = round(sum(!is.finite(x)), 3)
      )
    })
    out$continuous <- cont_tab
  }
  if (length(cat_vars)) {
    cat_tab <- purrr::map_df(cat_vars, function(v) {
      x <- as_factor_safe(df[[v]])
      if (all(is.na(x))) {
        tibble::tibble(Variable = v, Level = NA_character_, Count = 0, Percent = 0)
      } else {
        tt <- as.data.frame(table(x, useNA = "ifany"))
        names(tt) <- c("Level","Count")
        tt$Count <- round(tt$Count, 3)
        tt$Percent <- round(100 * tt$Count / sum(tt$Count), 3)
        tt$Variable <- v
        tt[, c("Variable","Level","Count","Percent")]
      }
    })
    out$categorical <- cat_tab
  }
  out
}

# ---------- THEMED PLOTTING HELPERS ----------

# Covariate plots that accept a palette from the app theme
covariate_plots <- function(df, arm_var = NULL, palette = c("#0d6efd","#dc3545","#198754","#0dcaf0")) {
  if (is.null(df) || !nrow(df)) return(list())
  vars <- setdiff(names(df), c("time","status",arm_var))
  if (!length(vars)) return(list())
  plots <- list()
  for (v in vars) {
    x <- df[[v]]
    if (is.numeric(x)) {
      p <- ggplot(df, aes(x = .data[[v]], y = after_stat(density))) +
        geom_histogram(bins = 30, alpha = 0.9, fill = palette[1], color = NA, na.rm = TRUE) +
        labs(title = paste("Histogram of", v), x = v, y = "Density") +
        transparent_theme()
      plots[[v]] <- to_plotly_clear(p)
    } else {
      levs <- unique(as.factor(df[[v]]))
      p <- ggplot(df, aes(x = as.factor(.data[[v]]), fill = as.factor(.data[[v]]))) +
        geom_bar(alpha = 0.9, color = NA) +
        scale_fill_manual(values = rep(palette, length.out = length(levs))) +
        guides(fill = "none") +
        labs(title = paste("Bar chart of", v), x = v, y = "Count") +
        transparent_theme()
      plots[[v]] <- to_plotly_clear(p)
    }
  }
  plots
}



# Build a model.matrix column order from covariate definitions
build_mm_columns <- function(cov_defs, include_intercept = TRUE) {
  if (!length(cov_defs)) return(character(0))
  
  # Determine the longest categorical level count
  rows <- 1
  cols <- list()
  
  for (d in cov_defs) {
    if (d$type == "continuous") {
      cols[[d$name]] <- 0
    } else if (d$type == "categorical") {
      lv <- d$params$labels
      if (is.null(lv) || !length(lv)) {
        k <- length(d$params$prob %||% c(0,1))
        lv <- paste0(d$name, seq_len(k))
      }
      cols[[d$name]] <- factor(lv, levels = lv)
      rows <- max(rows, length(lv))
    }
  }
  
  # Rep every column to the SAME length
  df <- as.data.frame(
    lapply(cols, function(x) rep(x, length.out = max(1, rows))),
    stringsAsFactors = FALSE
  )
  
  form <- as.formula(paste0(
    if (include_intercept) "~ 1 +" else "~ -1 +",
    paste(vapply(cov_defs, function(d) d$name, character(1)), collapse = " + ")
  ))
  
  mm <- model.matrix(form, data = df)
  colnames(mm)
}


# Coeff vector in model.matrix column order
assemble_beta <- function(cov_defs, user_betas, include_intercept = TRUE, intercept_value = 0) {
  cols <- build_mm_columns(cov_defs, include_intercept = include_intercept)
  out <- numeric(0)
  if (include_intercept) out <- c(out, user_betas[["(Intercept)"]] %||% intercept_value)
  for (d in cov_defs) {
    if (d$type == "continuous") {
      b <- as.numeric(user_betas[[d$name]])
      if (length(b) != 1 || !is.finite(b)) stop("Coefficient for continuous '", d$name, "' must be a single finite number.")
      out <- c(out, b)
    } else if (d$type == "categorical") {
      lv <- d$params$labels
      if (is.null(lv) || !length(lv)) {
        k <- length(d$params$prob)
        lv <- paste0(d$name, seq_len(k))
      }
      if (include_intercept) {
        need <- length(lv) - 1
        b <- as.numeric(user_betas[[d$name]])
        if (length(b) < need) stop("Categorical '", d$name, "' requires ", need, " coefficients (intercept included).")
        out <- c(out, b[seq_len(need)])
      } else {
        need <- length(lv)
        b <- as.numeric(user_betas[[d$name]])
        if (length(b) < need) stop("Categorical '", d$name, "' requires ", need, " coefficients (no intercept).")
        out <- c(out, b[seq_len(need)])
      }
    }
  }
  out
}

# ------------------ REPORT TEMPLATE (NEW SECTIONING) ------------------
make_inline_template <- function() {
  tf <- tempfile(fileext = ".Rmd")
  txt <- c(
    "---",
    "title: \"RMSTpowerBoost Report\"",
    "output: pdf_document",
    "params:",
    "  inputs: NA",
    "  results: NA",
    "  log: NA",
    "  data_provenance: NA",
    "  data: NA",
    "  data_cleaning: NA",
    "  reproducibility: NA",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "suppressPackageStartupMessages({",
    "  library(ggplot2); library(survival); library(survminer);",
    "  library(dplyr); library(tidyr); library(tibble)",
    "})",
    "to_kv <- function(x){",
    "  if (is.null(x)) return(\"\")",
    "  keys <- names(x); if (is.null(keys)) return(paste(x, collapse=\", \"))",
    "  paste(paste(keys, sapply(x, function(z){",
    "    if (is.null(z)) return(\"NULL\")",
    "    if (is.atomic(z)) return(paste(z, collapse=\", \"))",
    "    if (is.function(z)) return(\"<function>\")",
    "    if (is.list(z)) return(paste(unlist(z), collapse=\", \"))",
    "    as.character(z)",
    "  }), sep = \" = \"), collapse = \"; \")",
    "}",
    "or_else <- function(x, y) if (is.null(x)) y else x",
    "```",
    "",
    "# 1) Data Generating Mechanism",
    "",
    "## 1.1) Covariate generation",
    "```{r covariate-generation, echo=FALSE}",
    "cov_list <- or_else(params$data_provenance$Covariates_defined, list())",
    "if (length(cov_list)) {",
    "  cov_tbl <- do.call(rbind, lapply(cov_list, function(d) {",
    "    data.frame(",
    "      Variable     = or_else(d$name, \"\"),",
    "      Type         = or_else(d$type, \"\"),",
    "      Distribution = or_else(d$dist, \"\"),",
    "      Parameters   = to_kv(d$params),",
    "      Transform    = paste(or_else(d$transform, character(0)), collapse = \", \"),",
    "      stringsAsFactors = FALSE",
    "    )",
    "  }))",
    "  knitr::kable(cov_tbl, caption = \"Covariate definitions\", row.names = FALSE)",
    "} else {",
    "  cat(\"No covariates were defined or the data were uploaded.\\n\")",
    "}",
    "```",
    "",
    "## 1.2) Event time and censoring",
    "```{r event-time-censoring, echo=FALSE}",
    "prov <- params$data_provenance",
    "et <- or_else(prov$Event_time, list())",
    "cz <- or_else(prov$Censoring, list())",
    "tbl <- data.frame(",
    "  Field = c(\"Event-time model\", \"Baseline parameters\", \"Censoring mode\", \"Target overall censoring\", \"Administrative time\"),",
    "  Value = c(",
    "    as.character(or_else(et$model, or_else(et$Model, \"\"))),",
    "    to_kv(or_else(et$baseline, or_else(et$Baseline, list()))),",
    "    as.character(or_else(cz$mode, or_else(cz$Mode, \"\"))),",
    "    as.character(or_else(cz$target, or_else(cz$Target, \"\"))),",
    "    as.character(or_else(cz$admin_time, or_else(cz$Admin_time, \"\")))",
    "  ), stringsAsFactors = FALSE",
    ")",
    "knitr::kable(tbl, caption = \"Event-time and censoring\", row.names = FALSE)",
    "```",
    "",
    "# 2) Analysis Configuration",
    "```{r input-parameters, echo=FALSE}",
    "inputs_df <- data.frame(",
    "  Parameter = names(params$inputs),",
    "  Value     = unlist(lapply(params$inputs, function(x) paste(x, collapse = \", \"))),",
    "  stringsAsFactors = FALSE",
    ")",
    "knitr::kable(inputs_df, caption = \"Input Parameters\",",
    "    row.names = FALSE, col.names = c(\"Parameter\",\"Value\"))",
    "```",
    "",
    "# 3) Analysis Results",
    "",
    "## 3.1) Survival Analysis",
    "```{r km-plot, eval = !is.null(params$results$km_note)}",
    "cat(params$results$km_note)",
    "```",
    "```{r logrank-summary, echo=FALSE, eval = !is.null(params$results$logrank_summary)}",
    "cap <- if (!is.null(params$inputs$strata_var) && nzchar(params$inputs$strata_var)) {",
    "  paste0('Stratified log-rank test results (strata: ', params$inputs$strata_var, ')')",
    "} else {",
    "  'Log-rank test results'",
    "}",
    "knitr::kable(params$results$logrank_summary,",
    "    caption = cap, row.names = FALSE)",
    "```",
    "",
    "## 3.2) Power and Sample Size",
    "```{r power-curve, eval = !is.null(params$results$results_plot)}",
    "params$results$results_plot",
    "```",
    "```{r results-table, eval = !is.null(params$results$results_data)}",
    "knitr::kable(params$results$results_data,",
    "    caption = \"Power and sample size results\", row.names = FALSE)",
    "```",
    "```{r effect-size, eval = !is.null(params$results$results_summary)}",
    "knitr::kable(params$results$results_summary,",
    "    caption = \"Summary measures derived from the data\", row.names = FALSE)",
    "```",
    "",
    "# 4) Data Summary and Cleaning",
    "```{r data-summary-cleaning, echo=FALSE}",
    "if (!is.null(params$data) && nrow(params$data) > 0) {",
    "  d <- params$data",
    "  var_tbl <- data.frame(",
    "    Variable = names(d),",
    "    Type = vapply(d, function(x) class(x)[1], character(1)),",
    "    Missing_Count = vapply(d, function(x) sum(is.na(x)), integer(1)),",
    "    Missing_Percent = round(100 * vapply(d, function(x) mean(is.na(x)), numeric(1)), 3),",
    "    Unique_Values = vapply(d, function(x) length(unique(x[!is.na(x)])), integer(1)),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  knitr::kable(var_tbl, caption = 'Variable summary (analysis dataset)', row.names = FALSE)",
    "  num_vars <- names(d)[vapply(d, is.numeric, logical(1))]",
    "  if (length(num_vars)) {",
    "    num_tbl <- do.call(rbind, lapply(num_vars, function(v) {",
    "      x <- d[[v]]",
    "      data.frame(",
    "        Variable = v,",
    "        Mean = round(mean(x, na.rm = TRUE), 3),",
    "        SD = round(stats::sd(x, na.rm = TRUE), 3),",
    "        Min = round(min(x, na.rm = TRUE), 3),",
    "        Max = round(max(x, na.rm = TRUE), 3),",
    "        stringsAsFactors = FALSE",
    "      )",
    "    }))",
    "    knitr::kable(num_tbl, caption = 'Numeric variable statistics', row.names = FALSE)",
    "  }",
    "} else {",
    "  cat('No data was provided to the report.\\n')",
    "}",
    "dc <- params$data_cleaning",
    "src <- or_else(params$data_provenance$Source, 'uploaded')",
    "show_upload_impute <- !is.null(dc) && !identical(src, 'simulated') &&",
    "  (isTRUE(dc$used_mice) || (dc$mode %in% c('impute','both')))",
    "if (is.null(dc)) {",
    "  cat('\\nComplete data used.\\n')",
    "} else if (isTRUE(show_upload_impute)) {",
    "  dc_tbl <- data.frame(",
    "    Field = c(",
    "      'Cleaning mode', 'Message',",
    "      'Dropped rows', 'Dropped columns', 'Threshold-dropped columns',",
    "      'MICE m', 'MICE maxit', 'MICE default method', 'MICE seed', 'Completed dataset index',",
    "      'Unimputed columns', 'Post-impute missing columns'",
    "    ),",
    "    Value = c(",
    "      as.character(or_else(dc$mode, '')),",
    "      as.character(or_else(dc$message, '')),",
    "      as.character(or_else(dc$dropped_rows, '')),",
    "      paste(or_else(dc$dropped_columns, character(0)), collapse = ', '),",
    "      paste(or_else(dc$threshold_dropped_columns, character(0)), collapse = ', '),",
    "      as.character(or_else(dc$mice_parameters$m, '')),",
    "      as.character(or_else(dc$mice_parameters$maxit, '')),",
    "      as.character(or_else(dc$mice_parameters$method_default, '')),",
    "      as.character(or_else(dc$mice_parameters$seed, '')),",
    "      as.character(or_else(dc$mice_parameters$completed_dataset, '')),",
    "      paste(or_else(dc$mice_parameters$unimputed_columns, character(0)), collapse = ', '),",
    "      paste(or_else(dc$post_impute_missing_columns, character(0)), collapse = ', ')",
    "    ),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  knitr::kable(dc_tbl, caption = 'Data cleaning details (uploaded data)', row.names = FALSE)",
    "} else if (isTRUE(dc$used_mice)) {",
    "  dc_tbl <- data.frame(",
    "    Field = c('Cleaning mode', 'Message', 'MICE m', 'MICE maxit', 'MICE default method', 'MICE seed', 'Completed dataset index'),",
    "    Value = c(",
    "      as.character(or_else(dc$mode, 'impute')),",
    "      as.character(or_else(dc$message, 'MICE imputation used.')),",
    "      as.character(or_else(dc$mice_parameters$m, '')),",
    "      as.character(or_else(dc$mice_parameters$maxit, '')),",
    "      as.character(or_else(dc$mice_parameters$method_default, '')),",
    "      as.character(or_else(dc$mice_parameters$seed, '')),",
    "      as.character(or_else(dc$mice_parameters$completed_dataset, '1'))",
    "    ),",
    "    stringsAsFactors = FALSE",
    "  )",
    "  knitr::kable(dc_tbl, caption = 'Data cleaning details (MICE)', row.names = FALSE)",
    "} else {",
    "  cat('\\n', as.character(or_else(dc$message, 'Complete data used.')), '\\n', sep = '')",
    "}",
    "```",
    "",
    "# 5) Console Output",
    "```{r console-log, results='asis', echo=FALSE, eval = !is.null(params$log)}",
    "cat(params$log)",
    "```",
    "",
    "# 6) Reproducibility",
    "```{r reproducibility, echo=FALSE, eval = !is.null(params$reproducibility)}",
    "rep <- params$reproducibility",
    "rep_df <- data.frame(",
    "  Field = c('Timestamp', 'Seed (simulation)', 'Seed (repeated)', 'R version'),",
    "  Value = c(as.character(rep$timestamp %||% ''), as.character(rep$sim_seed %||% ''), as.character(rep$seed_reps %||% ''), as.character(rep$r_version %||% '')),",
    "  stringsAsFactors = FALSE",
    ")",
    "knitr::kable(rep_df, caption = 'Reproducibility snapshot', row.names = FALSE)",
    "if (!is.null(rep$package_versions) && nrow(rep$package_versions)) {",
    "  knitr::kable(rep$package_versions, caption = 'Package versions', row.names = FALSE)",
    "}",
    "if (!is.null(rep$session_info)) {",
    "  cat('\\n\\nSession info:\\n')",
    "  cat(rep$session_info)",
    "}",
    "```"
  )
  writeLines(txt, tf)
  tf
}
report_inputs_builder <- function(input) {
  list(
    model_selection = input$model_selection,
    analysis_type   = input$analysis_type,
    time_var        = input$time_var,
    status_var      = input$status_var,
    arm_var         = input$arm_var,
    strata_var      = input$strata_var %||% "",
    dc_linear_terms = input$dc_linear_terms %||% character(0),  # added
    calc_method     = input$calc_method %||% "Analytical",
    full_truncation = input$L,
    alpha           = input$alpha,
    sample_sizes    = input$sample_sizes,
    target_power    = input$target_power,
    sim_seed        = input$sim_seed,
    seed_reps       = input$seed_reps,
    replications    = input$R_reps,
    data_mode       = input$data_mode
  )
}

build_app_theme <- function(bootswatch = "sandstone") {
  bs_theme(
    version = 5,
    bootswatch = bootswatch,
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Bebas Neue")
  )
}


# ------------------ UI ------------------
ui <- fluidPage(
  theme = build_app_theme("sandstone"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  titlePanel("RMSTpowerBoost: Power and Sample Size Calculator"),
  
  div(
    class = "app-shell",
    sidebarLayout(
    sidebarPanel(
      width = 4,
      div(
        class = "pb-mb-8",
        actionButton("reset_workflow", "Reset", class = "btn btn-danger btn-sm workflow-btn")
      ),
      # Step 1: Data (Upload or Generate)
      div(id = "step1_panel", wellPanel(
        div(
          id = "step1_choice_block",
          h4("Step 1. Data Source"),
          radioButtons(
            "data_mode",
            "Choose data source:",
            choices = c("A) Generate Data" = "Generate", "B) Upload Pilot Data" = "Upload"),
            inline = TRUE
          ),
          fluidRow(
            column(6, actionButton("confirm_step1", "Confirm", class = "btn btn-success btn-sm workflow-btn")),
            column(6, actionButton("reset_step1", "Reset", class = "btn btn-danger btn-sm workflow-btn"))
          )
        ),
        shinyjs::hidden(div(
          id = "upload_panel",
          h5("1B) Upload Pilot Data"),
          fileInput(
            "pilot_data_upload",
            "Upload Pilot Data (.csv, .txt, .tsv, .rds, .RData)",
            accept = c(".csv", ".txt", ".tsv", ".rds", ".RData", ".rdata")
          ),
          fluidRow(
            column(12, downloadButton("download_csv_template", "Download Template"))
          ),
          br(),
          selectInput("bundled_dataset_choice", "Built-in Test Dataset (no upload)", choices = c()),
          actionButton("load_bundled_dataset", "Load Built-in Dataset", class = "btn btn-secondary btn-sm workflow-btn")
        )),
        shinyjs::hidden(div(
          id = "simulate_panel",
          h5("Step 2. Generation"),
          bslib::accordion(
            id = "step1_accordion",
            bslib::accordion_panel(
              title = "1A. Covariate Builder",
              # Row 1: name/type
              fluidRow(
                column(6, textInput("cov_name", "Variable name", value = "", placeholder = "x1, x2 ... auto if empty")),
                column(6, selectInput("cov_type", "Type", choices = c("continuous","categorical")))
              ),
              conditionalPanel(
                condition = "input.cov_type == 'continuous'",
                fluidRow(
                  column(6, selectInput("cont_dist", "Distribution", choices = c("normal","lognormal","gamma","weibull","uniform","t","beta"))),
                  column(6, numericInput("cont_beta", "Coefficient beta", value = 0))
                ),
                uiOutput("cont_param_ui"),
                tags$hr(),
                h5("Transform (continuous only)"),
                fluidRow(
                  column(6, numericInput("tf_center", "Location (center a)", value = 0)),
                  column(6, numericInput("tf_scale",  "Scale (divide by b)", value = 1, min = 0.0001, step = 0.1))
                )
              ),
              conditionalPanel(
                condition = "input.cov_type == 'categorical'",
                fluidRow(
                  column(6, textInput("cat_add_name", "Add category name", placeholder = "auto if blank")),
                  column(3, numericInput("cat_add_prob", "Probability", value = NA, min = 0, max = 1, step = 0.01)),
                  column(3, numericInput("cat_add_coef", "Coefficient beta", value = 0))
                ),
                fluidRow(
                  column(4, actionButton("add_cat_row", "Add", icon=icon("plus"))),
                  column(4, actionButton("reset_cat_rows", "Reset", icon=icon("trash"))),
                  column(4, actionButton("remove_cat_row", "Remove", icon=icon("minus")))
                ),
                br(),
                DTOutput("cat_table")
              ),
              fluidRow(
                column(6, actionButton("add_cov", "Add", icon = icon("plus"), class = "btn btn-success")),
                column(6, actionButton("reset_cov_builder", "Reset", icon = icon("trash")))
              ),
              br(), DTOutput("cov_table"),
              div(class="pb-mt-8",
                  actionButton("remove_cov", "Remove", icon = icon("minus"))
              ),
              tags$hr(),
              fluidRow(
                column(6, actionButton("confirm_covariates", "Confirm", icon = icon("check"), class = "btn btn-primary")),
                column(6, div())
              )
            ),
            bslib::accordion_panel(
              title = "1B. Event Time Settings",
              fluidRow(
                column(4, numericInput("sim_n", "Sample size", value = 300, min = 10)),
                column(4, textInput("sim_allocation", "Allocation (a:b)", value = "1:1")),
                column(4, numericInput("sim_treat_eff", "Treatment beta (arm)", value = -0.2, step = 0.05))
              ),
              fluidRow(
                column(6, checkboxInput("intercept_in_mm", "Include intercept in model.matrix (beta0 inside beta)", value = TRUE)),
                column(6, numericInput("user_intercept", "beta0 (used if no intercept in model.matrix)", value = 0))
              ),
              fluidRow(
                column(6, selectInput("sim_model", "Event-time model",
                                      choices = c(
                                        "AFT (Lognormal)" = "aft_lognormal",
                                        "AFT (Weibull)" = "aft_weibull",
                                        "PH (Exponential)" = "ph_exponential",
                                        "PH (Weibull)" = "ph_weibull",
                                        "PH (Piecewise Exponential)" = "ph_pwexp"
                                      ))),
                column(6, sliderInput("sim_cens", "Target censoring", min = 0, max = 0.9, value = 0.25, step = 0.01))
              ),
              tags$hr(),
              fluidRow(
                column(4, numericInput("sim_seed", "Seed (optional)", value = NA))
              ),
              tags$hr(),
              fluidRow(column(12, uiOutput("sim_baseline_ui")))
              ,
              tags$hr(),
              fluidRow(
                column(6, actionButton("generate_sim", "Generate", icon = icon("gears"), class = "btn btn-primary workflow-btn")),
                column(6, actionButton("reset_generate", "Reset", icon = icon("trash"), class = "btn btn-danger workflow-btn"))
              )
            ),
            open = c("1A. Covariate Builder")
          ),
          fluidRow(
            column(6, actionButton("confirm_step2_data", "Confirm", class = "btn btn-success btn-sm workflow-btn")),
            column(6, actionButton("reset_step2_data", "Reset", class = "btn btn-danger btn-sm workflow-btn"))
          )
        ))
      )),
      shinyjs::hidden(
        wellPanel(
          id = "data_cleaning_panel",
          h4("Step 2. MICE / Cleaning (Upload Only)"),
          uiOutput("data_cleaning_sidebar_ui"),
          fluidRow(
            column(6, actionButton("confirm_step2_data_upload", "Confirm", class = "btn btn-success btn-sm workflow-btn")),
            column(6, actionButton("reset_step2_data_upload", "Reset", class = "btn btn-danger btn-sm workflow-btn"))
          )
        )
      ),
      
      # Step 3+4: Model/Mapping + Analysis (hidden until data ready)
      shinyjs::hidden(
        wellPanel(
          id = "model_analysis_panel",
          div(
            id = "step3_controls",
            h4("Step 3. Model and Mapping"),
            fluidRow(
              column(12, selectInput("model_selection", "Select RMST Model",
                                    choices = c("Linear IPCW Model","Additive Stratified Model",
                                                "Multiplicative Stratified Model","Semiparametric (GAM) Model",
                                                "Dependent Censoring Model"),
                                    selected = "Linear IPCW Model"))
            ),
            uiOutput("col_mapping_ui"),
            fluidRow(
              column(6, actionButton("confirm_step2", "Confirm", class = "btn btn-success workflow-btn")),
              column(6, actionButton("reset_step3", "Reset", class = "btn btn-danger workflow-btn"))
            )
          ),
          div(
            id = "step4_controls",
            h4("Step 4. Analysis"),
            fluidRow(
              column(4, numericInput("L", tagList("Truncation TIme ", tags$span(icon("circle-info"), title = "Truncation TIme for RMST")), value = 365, min = 1)),
              column(4, radioButtons("analysis_type", "Target Quantity", choices = c("Power", "Sample Size"), selected = "Power")),
              column(4, sliderInput("alpha", "Significance Level (alpha)", min = 0.01, max = 0.1, value = 0.05, step = 0.01))
            ),
            radioButtons("calc_method", "Calculation Method", choices = c("Analytical", "Repeated"), selected = "Analytical", inline = TRUE),
            uiOutput("analysis_inputs_ui"),
            conditionalPanel(
              condition = "input.calc_method == 'Repeated'",
              fluidRow(
                column(6, numericInput("R_reps", "Replications", value = 500, min = 100, step = 100)),
                column(6, numericInput("seed_reps", "Seed (optional)", value = NA))
              )
            ),
            uiOutput("run_checklist_ui"),
            tags$hr(),
            shinyjs::hidden(fluidRow(
              id = "analysis_run_panel",
              column(4, actionButton("run_analysis", "Run", icon = icon("play"), class = "btn-primary btn-lg")),
              column(8, shinyjs::hidden(
                div(id="download_reset_row",
                    downloadButton("download_report_pdf", "PDF"),
                    downloadButton("download_report_html", "HTML")
                )
              ))
            ))
          )
        )
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Pipeline",
          div(class = "section-card", uiOutput("pipeline_page_ui"))
        ),
        tabPanel("Data",
                 uiOutput("data_export_ui"),
                 uiOutput("missing_profile_ui"),
                 DT::dataTableOutput("data_preview_table")),
        tabPanel("Summary",
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Data Summary"),
                   uiOutput("data_summary_ui")
                 ),
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Covariate Distributions"),
                   p(class = "section-lead", "Quick distribution checks for data quality and scale sanity."),
                   uiOutput("cov_plots_ui")
                 )
        ),
        tabPanel("KM Plot",
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Kaplan-Meier Survival Plots"),
                   span(class = "metric-note", "Exploratory diagnostic"),
                   htmlOutput("km_note"),
                   plotlyOutput("survival_plotly_output", height = "650px")
                 )
        ),
        tabPanel("Analysis",
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Key Results"),
                   uiOutput("key_results_ui")
                 ),
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Power and Sample Size Results"),
                   uiOutput("results_table_ui")
                 ),
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Analysis Summary"),
                   uiOutput("summary_table_ui")
                 ),
                 div(
                   class = "section-card",
                   h4(class = "section-title", "Power vs. Sample Size"),
                   span(class = "metric-note", "Primary planning output"),
                   plotlyOutput("results_plot", height = "520px")
                 )
        ),
        tabPanel("Run Log", uiOutput("run_log_summary_ui"), verbatimTextOutput("console_log_output")),
        tabPanel("About", uiOutput("about_tab_ui"))
      )
    )
  ),
  div(
    id = "theme_widget_container",
    actionButton("theme_widget_open", "Theme", class = "btn btn-primary theme-widget-open"),
    shinyjs::hidden(
      div(
        id = "theme_widget_panel",
        class = "theme-widget-panel",
        div(
          class = "theme-widget-header",
          tags$span("Theme"),
          actionButton("theme_widget_close", "Close", class = "btn btn-default btn-sm")
        ),
        selectInput(
          "theme_preset",
          "Theme preset",
          choices = c(
            "Sandstone" = "sandstone",
            "Flatly" = "flatly",
            "Minty" = "minty",
            "Cosmo" = "cosmo",
            "Journal" = "journal"
          ),
          selected = "sandstone"
        ),
        actionButton("apply_theme", "Apply", class = "btn btn-primary workflow-btn")
      )
    )
  )
)
)

# ------------------ Repeated Power (no 'bootstrap' wording) ------------------
repeated_power_from_pilot <- function(pilot_df, time_var, status_var, arm_var,
                                      n_per_arm_vec, alpha = 0.05, R = 500,
                                      strata_var = NULL, seed = NULL,
                                      point_cb = NULL) {
  stopifnot(is.data.frame(pilot_df))
  needed <- c(time_var, status_var, arm_var, strata_var)
  needed <- needed[!is.null(needed)]
  if (!all(needed %in% names(pilot_df))) stop("Required columns not found in pilot data.")
  df <- pilot_df[, unique(c(time_var, status_var, arm_var, strata_var)), drop = FALSE]
  names(df)[match(c(time_var, status_var, arm_var), names(df))] <- c("time","status","arm")
  df$arm <- as.factor(df$arm)
  if (nlevels(df$arm) != 2L) stop("Repeated method currently expects exactly 2 arms.")
  if (!is.null(seed) && is.finite(seed)) set.seed(as.integer(seed))
  
  if (!is.null(strata_var)) {
    names(df)[names(df) == strata_var] <- "stratum"
    df$stratum <- as.factor(df$stratum)
    split_stratum <- split(df, df$stratum, drop = TRUE)
    split_by <- lapply(split_stratum, function(d) split(d, d$arm, drop = TRUE))
  } else {
    split_by <- split(df, df$arm, drop = TRUE)
  }
  
  out <- lapply(n_per_arm_vec, function(n_arm){
    rej <- logical(R)
    for (r in seq_len(R)) {
      if (is.null(strata_var)) {
        s0 <- split_by[[1]][sample.int(nrow(split_by[[1]]), n_arm, replace = TRUE), , drop = FALSE]
        s1 <- split_by[[2]][sample.int(nrow(split_by[[2]]), n_arm, replace = TRUE), , drop = FALSE]
        dat <- rbind(s0, s1)
        fml <- Surv(time, status) ~ arm
      } else {
        blocks <- lapply(split_by, function(by_arm) {
          a0 <- by_arm[[1]]; a1 <- by_arm[[2]]
          s0 <- a0[sample.int(nrow(a0), n_arm, replace = TRUE), , drop = FALSE]
          s1 <- a1[sample.int(nrow(a1), n_arm, replace = TRUE), , drop = FALSE]
          rbind(s0, s1)
        })
        dat <- do.call(rbind, blocks)
        dat$stratum <- factor(dat$stratum)
        fml <- Surv(time, status) ~ arm + strata(stratum)
      }
      dat$arm <- factor(dat$arm)
      lr <- tryCatch(survdiff(fml, data = dat), error = function(e) NULL)
      if (is.null(lr) || is.null(lr$chisq)) {
        rej[r] <- NA
      } else {
        df_chi <- length(lr$n) - 1
        p <- 1 - pchisq(lr$chisq, df = df_chi)
        rej[r] <- is.finite(p) && (p < alpha)
      }
    }
    m <- mean(rej, na.rm = TRUE); k <- sum(is.finite(rej))
    se <- if (k > 0) sqrt(m*(1-m)/k) else NA_real_
    row <- data.frame(N_per_arm = n_arm, Power = m, SE = se, Reps = k)
    if (is.function(point_cb)) {
      try(point_cb(n_arm, m), silent = TRUE)
    }
    row
  })
  do.call(rbind, out)
}

# ------------------ Server ------------------
server <- function(input, output, session) {
  if (interactive()) bslib::bs_themer()
  license_content <- tryCatch(paste(readLines("LICENSE", warn = FALSE), collapse = "\n"), error = function(e) "LICENSE not found.")
  output$pipeline_page_ui <- renderUI({
    p <- pipeline_html_path()
    if (!nzchar(p)) return(div(class = "alert alert-warning", "Pipeline documentation not found."))
    includeHTML(p)
  })
  output$about_tab_ui <- renderUI({
    tags$div(
      class = "section-card",
      h3(class = "section-title", "About RMSTpowerBoost"),
      p(
        class = "section-lead",
        "This app supports RMST-based power and sample size design with upload and simulation workflows."
      ),
      tags$h4(class = "section-title", "License"),
      tags$pre(style = "white-space: pre-wrap;", license_content),
      tags$h4(class = "section-title", "Report a Bug"),
      tags$p(
        tags$a(
          href = "https://github.com/UTHSC-Zhang/RMSTpowerBoost-Package/issues",
          target = "_blank",
          rel = "noopener noreferrer",
          "Open issue tracker"
        )
      ),
      tags$h4(class = "section-title", "Coverage"),
      tags$p(
        tags$a(
          href = "https://codecov.io/github/arnabaich96/RMSTpowerBoost-App",
          target = "_blank",
          rel = "noopener noreferrer",
          tags$img(
            src = "https://codecov.io/github/arnabaich96/RMSTpowerBoost-App/graph/badge.svg?token=5C7QOI1GAB",
            alt = "Codecov badge"
          )
        )
      ),
      tags$p(
        tags$a(
          href = "https://codecov.io/github/arnabaich96/RMSTpowerBoost-App/graphs/sunburst.svg?token=5C7QOI1GAB",
          target = "_blank",
          rel = "noopener noreferrer",
          "Coverage sunburst report"
        )
      ),
      tags$h4(class = "section-title", "Citation / Contact"),
      tags$p("RMSTpowerBoost supports linear IPCW, additive stratified, multiplicative stratified, semiparametric additive, and dependent censoring workflows."),
      tags$p("Use this application for trial design exploration and planning; pilot data quality and model assumptions affect conclusions."),
      tags$p("Citation text and maintainer contact can be finalized here."),
      tags$h4(class = "section-title", "External Resources"),
      tags$ul(
        tags$li(tags$a(
          href = "https://arnab96.shinyapps.io/uthsc-app/",
          target = "_blank",
          rel = "noopener noreferrer",
          "Live application"
        )),
        tags$li(tags$a(
          href = "https://uthsc-zhang.github.io/RMSTpowerBoost-Package/articles/RMSTpowerBoost-Main.html",
          target = "_blank",
          rel = "noopener noreferrer",
          "Package documentation and source"
        )),
        tags$li(tags$a(
          href = "https://github.com/arnabaich96/RMSTpowerBoost-App",
          target = "_blank",
          rel = "noopener noreferrer",
          "App repository"
        ))
      )
    )
  })
  
  theme_palette <- reactive({
    th <- bslib::bs_current_theme()
    # grab a few bootstrap variables; fall back if missing
    vars <- bslib::bs_get_variables(th, c("primary", "danger", "success", "info"))
    cols <- unname(unlist(vars))
    # ensure we always have at least two colors
    if (length(cols) < 2) cols <- c("#0d6efd", "#dc3545", "#198754", "#0dcaf0")
    cols
  })
  
  rv <- reactiveValues(
    covariates = list(),
    cat_rows = tibble::tibble(cat = character(), prob = numeric(), coef = numeric()),
    data_mode = "Upload",
    data_df = NULL,
    data_source = NULL,
    console_buf = character(0),
    live_power_active = FALSE,
    live_power_points = data.frame(N = numeric(), Power = numeric(), stringsAsFactors = FALSE),
    live_power_meta = list(xlab = "Sample size per arm", title = "Power vs. Sample Size", target_power = NULL),
    last_warning_lines = character(0),
    last_error_lines = character(0),
    auto_run_pending = FALSE,
    effective_sim_seed = NULL,
    effective_reps_seed = NULL,
    data_df_raw_upload = NULL,
    missing_summary = NULL,
    cleaning_logs = character(0),
    cleaning_mode = NULL,
    cleaning_report = list(
      mode = "complete_data",
      used_mice = FALSE,
      message = "Complete data used.",
      mice_parameters = NULL
    ),
    cleaning_required = FALSE,
    recommended_clean_mode = "ignore",
    step2_confirmed = FALSE,
    step1_confirmed = FALSE,
    step2_data_confirmed = FALSE
  )

  append_clean_log <- function(msg) {
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$cleaning_logs <- c(rv$cleaning_logs, sprintf("[%s] %s", stamp, msg))
  }

  compute_missing_summary <- function(df) {
    if (is.null(df) || !nrow(df)) return(NULL)
    miss_n <- vapply(df, function(x) sum(is.na(x)), integer(1))
    data.frame(
      Column = names(df),
      Type = vapply(df, function(x) class(x)[1], character(1)),
      Missing_Count = as.integer(miss_n),
      Missing_Percent = round(100 * miss_n / nrow(df), 3),
      Non_Missing_Count = as.integer(nrow(df) - miss_n),
      stringsAsFactors = FALSE
    )
  }
  
  parse_allocation_ratio <- function(x) {
    xs <- trimws(as.character(x %||% ""))
    if (!grepl("^[0-9]+\\s*:\\s*[0-9]+$", xs)) return(NULL)
    parts <- as.numeric(trimws(strsplit(xs, ":", fixed = TRUE)[[1]]))
    if (length(parts) != 2 || !isTRUE(all(is.finite(parts))) || any(parts <= 0, na.rm = TRUE)) return(NULL)
    parts
  }
  
  sim_validation <- reactive({
    msgs <- character(0)
    n_val <- suppressWarnings(as.numeric(input$sim_n))
    if (!isTRUE(length(n_val) == 1 && is.finite(n_val) && n_val > 0 && abs(n_val - round(n_val)) <= 1e-8)) {
      msgs <- c(msgs, "Sample size must be an integer greater than 0.")
    }
    if (is.null(parse_allocation_ratio(input$sim_allocation))) msgs <- c(msgs, "Allocation must be in a:b format with positive integers.")
    c_val <- suppressWarnings(as.numeric(input$sim_cens))
    if (!isTRUE(length(c_val) == 1 && is.finite(c_val) && c_val >= 0 && c_val <= 0.9)) {
      msgs <- c(msgs, "Target censoring must be between 0 and 0.9.")
    }
    l_val <- suppressWarnings(as.numeric(input$L))
    if (!isTRUE(length(l_val) == 1 && is.finite(l_val) && l_val > 0)) {
      msgs <- c(msgs, "Truncation TIme must be greater than 0.")
    }
    model <- input$sim_model %||% "aft_lognormal"
    if (identical(model, "aft_lognormal")) {
      if (is.null(input$b_sigma)) msgs <- c(msgs, "Open '1b. Event Time Settings' to load Lognormal baseline inputs.")
      sigma_val <- suppressWarnings(as.numeric(input$b_sigma))
      if (!isTRUE(length(sigma_val) == 1 && is.finite(sigma_val) && sigma_val > 0)) {
        msgs <- c(msgs, "For AFT (Lognormal), sigma must be greater than 0.")
      }
    } else if (identical(model, "aft_weibull")) {
      if (is.null(input$b_shape) || is.null(input$b_scale)) msgs <- c(msgs, "Open '1b. Event Time Settings' to load Weibull baseline inputs.")
      shape_val <- suppressWarnings(as.numeric(input$b_shape))
      scale_val <- suppressWarnings(as.numeric(input$b_scale))
      if (!isTRUE(length(shape_val) == 1 && is.finite(shape_val) && shape_val > 0 &&
                  length(scale_val) == 1 && is.finite(scale_val) && scale_val > 0)) {
        msgs <- c(msgs, "For AFT (Weibull), shape and scale must be greater than 0.")
      }
    } else if (identical(model, "ph_exponential")) {
      if (is.null(input$b_rate)) msgs <- c(msgs, "Open '1b. Event Time Settings' to load Exponential baseline inputs.")
      rate_val <- suppressWarnings(as.numeric(input$b_rate))
      if (!isTRUE(length(rate_val) == 1 && is.finite(rate_val) && rate_val > 0)) {
        msgs <- c(msgs, "For PH (Exponential), rate must be greater than 0.")
      }
    } else if (identical(model, "ph_weibull")) {
      if (is.null(input$b_wshape2) || is.null(input$b_wscale2)) msgs <- c(msgs, "Open '1b. Event Time Settings' to load PH Weibull baseline inputs.")
      w_shape_val <- suppressWarnings(as.numeric(input$b_wshape2))
      w_scale_val <- suppressWarnings(as.numeric(input$b_wscale2))
      if (!isTRUE(length(w_shape_val) == 1 && is.finite(w_shape_val) && w_shape_val > 0 &&
                  length(w_scale_val) == 1 && is.finite(w_scale_val) && w_scale_val > 0)) {
        msgs <- c(msgs, "For PH (Weibull), shape and scale must be greater than 0.")
      }
    } else if (identical(model, "ph_pwexp")) {
      if (is.null(input$b_rates) || is.null(input$b_cuts)) msgs <- c(msgs, "Open '1b. Event Time Settings' to load piecewise-exponential baseline inputs.")
      rates <- suppressWarnings(as.numeric(trimws(strsplit(input$b_rates %||% "", ",")[[1]])))
      cuts_raw <- trimws(strsplit(input$b_cuts %||% "", ",")[[1]])
      cuts <- if (length(cuts_raw) == 1 && !nzchar(cuts_raw)) numeric(0) else suppressWarnings(as.numeric(cuts_raw))
      if (!length(rates) || !isTRUE(all(is.finite(rates))) || any(rates <= 0, na.rm = TRUE)) {
        msgs <- c(msgs, "For PH (Piecewise Exponential), all rates must be numeric and greater than 0.")
      }
      if ((length(cuts) > 0 && !isTRUE(all(is.finite(cuts)))) ||
          (length(cuts) > 1 && any(diff(cuts) <= 0, na.rm = TRUE))) {
        msgs <- c(msgs, "For PH (Piecewise Exponential), cuts must be increasing numeric values.")
      }
    }
    list(ok = !length(msgs), messages = msgs)
  })
  
  mapping_resolved <- reactive({
    df <- rv$data_df
    if (is.null(df) || !nrow(df)) return(list(time_var = NULL, status_var = NULL, arm_var = NULL, strata_var = NULL))
    cn <- names(df)
    pick <- function(selected, candidates = character(0), fallback = NULL, exclude = character(0)) {
      cn_use <- setdiff(cn, exclude %||% character(0))
      if (!is.null(selected) && nzchar(selected) && selected %in% cn) return(selected)
      if (length(candidates)) {
        low <- tolower(cn_use)
        # exact then partial matches
        for (cand in tolower(candidates)) {
          hit <- which(low == cand)
          if (length(hit)) return(cn_use[hit[1]])
        }
        for (cand in tolower(candidates)) {
          hit <- grep(cand, low, fixed = TRUE)
          if (length(hit)) return(cn_use[hit[1]])
        }
      }
      if (!is.null(fallback) && fallback >= 1 && fallback <= length(cn_use)) return(cn_use[fallback])
      NULL
    }
    time_var <- pick(input$time_var, candidates = c("time", "tte", "time_to_event", "followup_time"), fallback = 1)
    status_var <- pick(
      input$status_var,
      candidates = c("status", "event", "delta", "censor_status"),
      fallback = 1,
      exclude = c(time_var)
    )
    arm_var <- pick(
      input$arm_var,
      candidates = c("arm", "treatment", "trt", "group", "tx"),
      fallback = 1,
      exclude = c(time_var, status_var)
    )
    strata_var <- if (!is.null(input$strata_var) && nzchar(input$strata_var) && input$strata_var %in% cn) input$strata_var else NULL
    list(time_var = time_var, status_var = status_var, arm_var = arm_var, strata_var = strata_var)
  })
  
  analysis_checklist <- reactive({
    out <- list()
    df <- rv$data_df
    m <- mapping_resolved()
    out$data_available <- !is.null(df) && nrow(df) > 0
    out$time_mapped <- out$data_available && !is.null(m$time_var) && nzchar(m$time_var) && (m$time_var %in% names(df))
    out$status_mapped <- out$data_available && !is.null(m$status_var) && nzchar(m$status_var) && (m$status_var %in% names(df))
    out$arm_mapped <- out$data_available && !is.null(m$arm_var) && nzchar(m$arm_var) && (m$arm_var %in% names(df))
    out$status_binary <- FALSE
    out$arm_two_groups <- FALSE
    if (isTRUE(out$status_mapped)) out$status_binary <- !inherits(try(coerce_status_binary(df[[m$status_var]], m$status_var), silent = TRUE), "try-error")
    if (isTRUE(out$arm_mapped)) out$arm_two_groups <- !inherits(try(coerce_arm_binary(df[[m$arm_var]], m$arm_var), silent = TRUE), "try-error")
    needs_strata <- (input$model_selection %in% c("Additive Stratified Model", "Multiplicative Stratified Model"))
    out$strata_mapped <- !needs_strata || (!is.null(m$strata_var) && nzchar(m$strata_var) && m$strata_var %in% names(df))
    out$analysis_target <- if (identical(input$analysis_type, "Power")) {
      n_vec <- suppressWarnings(as.numeric(trimws(strsplit(input$sample_sizes %||% "100,150,200", ",")[[1]])))
      any(is.finite(n_vec) & n_vec > 0)
    } else {
      tp <- suppressWarnings(as.numeric(input$target_power %||% 0.8))
      is.finite(tp) && tp > 0 && tp <= 1
    }
    out$ready <- all(unlist(out[c("data_available","time_mapped","status_mapped","arm_mapped","status_binary","arm_two_groups","strata_mapped","analysis_target")]))
    out
  })

  init_live_power_plot <- function(xlab = "Sample size per arm",
                                   title = "Power vs. Sample Size",
                                   target_power = NULL) {
    rv$live_power_points <- data.frame(N = numeric(), Power = numeric(), stringsAsFactors = FALSE)
    rv$live_power_meta <- list(xlab = xlab, title = title, target_power = target_power)
    rv$live_power_active <- TRUE
  }

  build_live_power_plot <- function() {
    pal <- theme_palette()
    pts <- rv$live_power_points
    lbl <- if (nrow(pts)) sprintf("N=%s<br>P=%.3f", pts$N, pts$Power) else character(0)
    p <- plotly::plot_ly(
      x = pts$N,
      y = pts$Power,
      type = "scatter",
      mode = "lines+markers+text",
      text = lbl,
      textposition = "top center",
      line = list(color = pal[1], width = 2),
      marker = list(color = pal[2], size = 9),
      hovertemplate = "N=%{x}<br>Power=%{y:.3f}<extra></extra>"
    )
    p <- plotly::layout(
      p,
      title = list(text = rv$live_power_meta$title %||% "Power vs. Sample Size", x = 0.02, xanchor = "left"),
      xaxis = list(title = rv$live_power_meta$xlab %||% "Sample size per arm"),
      yaxis = list(title = "Power", range = c(0, 1.05)),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 60, r = 20, b = 70, l = 60)
    )
    if (is.finite(rv$live_power_meta$target_power %||% NA_real_)) {
      tp <- as.numeric(rv$live_power_meta$target_power)
      p <- plotly::layout(
        p,
        shapes = list(list(
          type = "line",
          xref = "paper",
          x0 = 0,
          x1 = 1,
          y0 = tp,
          y1 = tp,
          line = list(color = "red", dash = "dash")
        )),
        annotations = list(list(
          xref = "paper",
          x = 0.99,
          y = tp,
          text = sprintf("Target %.2f", tp),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(size = 11, color = "red")
        ))
      )
    }
    p
  }

  push_live_power_point <- function(n_value, power_value) {
    if (!is.finite(n_value) || !is.finite(power_value)) return(invisible(NULL))
    rv$live_power_points <- rbind(
      rv$live_power_points,
      data.frame(N = as.numeric(n_value), Power = as.numeric(power_value), stringsAsFactors = FALSE)
    )
    txt <- sprintf("N=%s<br>P=%.3f", n_value, power_value)
    try({
      plotly::plotlyProxy("results_plot", session) %>%
        plotly::plotlyProxyInvoke(
          "extendTraces",
          list(
            x = list(list(as.numeric(n_value))),
            y = list(list(as.numeric(power_value))),
            text = list(list(txt))
          ),
          list(0)
        )
    }, silent = TRUE)
  }
  
  # Toggle Upload vs Generate
  observeEvent(input$data_mode, {
    rv$data_mode <- input$data_mode
    shinyjs::toggle(id = "upload_panel", condition = input$data_mode == "Upload")
    shinyjs::toggle(id = "simulate_panel", condition = input$data_mode == "Generate")
  }, ignoreInit = FALSE)

  observeEvent(input$confirm_step1, {
    mode <- input$data_mode %||% "Upload"
    rv$step1_confirmed <- TRUE
    showNotification(sprintf("Step 1 confirmed: %s selected.", mode), type = "message")
  })

  observeEvent(input$reset_step1, {
    updateRadioButtons(session, "data_mode", selected = "Upload")
    rv$data_mode <- "Upload"
    rv$step1_confirmed <- FALSE
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    showNotification("Step 1 reset to Upload.", type = "message")
  })

  observeEvent(input$confirm_step2_data, {
    if (!identical(input$data_mode, "Generate")) return()
    has_generated <- identical(rv$data_source, "simulated") && !is.null(rv$data_df) && nrow(rv$data_df) > 0
    if (!isTRUE(has_generated)) {
      showNotification("Generate data first, then confirm Step 2.", type = "warning", duration = 8)
      return()
    }
    rv$step2_data_confirmed <- TRUE
    showNotification("Step 2 confirmed for generated data.", type = "message")
  })

  observeEvent(input$reset_step2_data, {
    if (!identical(input$data_mode, "Generate")) return()
    updateNumericInput(session, "sim_n", value = 300)
    updateTextInput(session, "sim_allocation", value = "1:1")
    updateNumericInput(session, "sim_treat_eff", value = -0.2)
    updateSelectInput(session, "sim_model", selected = "aft_lognormal")
    updateSliderInput(session, "sim_cens", value = 0.25)
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    showNotification("Step 2 generation settings reset.", type = "message")
  })

  observeEvent(input$confirm_step2_data_upload, {
    if (!is_upload_like_source(rv$data_source)) return()
    if (isTRUE(rv$cleaning_required)) {
      showNotification("Resolve missing values first, then confirm Step 2.", type = "warning", duration = 8)
      return()
    }
    rv$step2_data_confirmed <- TRUE
    showNotification("Step 2 confirmed for uploaded data cleaning.", type = "message")
  })

  observeEvent(input$reset_step2_data_upload, {
    if (!is_upload_like_source(rv$data_source)) return()
    updateRadioButtons(session, "clean_action_mode", selected = "ignore")
    updateRadioButtons(session, "ignore_drop_mode", selected = "rows")
    updateNumericInput(session, "mice_m", value = 5)
    updateNumericInput(session, "mice_maxit", value = 5)
    updateTextInput(session, "mice_seed", value = "")
    updateSelectInput(session, "mice_method", selected = "pmm")
    updateSliderInput(session, "drop_pct_threshold", value = 40)
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    showNotification("Step 2 cleaning options reset.", type = "message")
  })
  
  output$run_checklist_ui <- renderUI({
    ck <- analysis_checklist()
    mark <- function(ok) if (isTRUE(ok)) "\u2713" else "\u2717"
    cls <- function(ok) if (isTRUE(ok)) "text-success" else "text-danger"
    div(
      class = "section-card",
      h5("Run readiness checklist"),
      tags$ul(
        tags$li(class = cls(ck$data_available), sprintf("%s Data available", mark(ck$data_available))),
        tags$li(class = cls(ck$time_mapped), sprintf("%s Time mapped", mark(ck$time_mapped))),
        tags$li(class = cls(ck$status_binary), sprintf("%s Status mapped and binary", mark(ck$status_binary))),
        tags$li(class = cls(ck$arm_two_groups), sprintf("%s Arm mapped with exactly two groups", mark(ck$arm_two_groups))),
        tags$li(class = cls(ck$strata_mapped), sprintf("%s Strata mapped if required", mark(ck$strata_mapped))),
        tags$li(class = cls(ck$analysis_target), sprintf("%s Analysis target input provided", mark(ck$analysis_target)))
      )
    )
  })
  
  observe({
    shinyjs::toggleState("run_analysis", condition = isTRUE(analysis_checklist()$ready))
  })
  
  observe({
    ready_for_run <- !is.null(rv$data_df) && nrow(rv$data_df) > 0 &&
      !(is_upload_like_source(rv$data_source) && isTRUE(rv$cleaning_required))
    shinyjs::toggle(id = "analysis_run_panel", condition = isTRUE(rv$step2_confirmed) && ready_for_run)
  })
  
  observeEvent(input$confirm_step2, {
    ready_for_step2 <- !is.null(rv$data_df) && nrow(rv$data_df) > 0 &&
      !(is_upload_like_source(rv$data_source) && isTRUE(rv$cleaning_required))
    if (!isTRUE(ready_for_step2)) {
      showNotification("Resolve data readiness first before confirming Step 3.", type = "warning", duration = 8)
      return()
    }
    if (!isTRUE(analysis_checklist()$ready)) {
      showNotification("Complete checklist items in Step 3 before continuing to Step 4.", type = "warning", duration = 8)
      return()
    }
    rv$step2_confirmed <- TRUE
    showNotification("Step 3 confirmed. Step 4 analysis controls are unlocked.", type = "message")
  })

  observeEvent(input$reset_step3, {
    updateSelectInput(session, "model_selection", selected = "Linear IPCW Model")
    m <- auto_map_vars(rv$data_df)
    if (!is.null(rv$data_df) && nrow(rv$data_df)) {
      cn <- names(rv$data_df)
      updateSelectInput(session, "time_var", choices = cn, selected = m$time_var)
      updateSelectInput(session, "status_var", choices = cn, selected = m$status_var)
      updateSelectInput(session, "arm_var", choices = cn, selected = m$arm_var)
      if ("strata_var" %in% names(input)) {
        cand_strata <- setdiff(cn, unique(na.omit(c(m$time_var, m$status_var, m$arm_var))))
        updateSelectInput(session, "strata_var", choices = cand_strata, selected = m$strata_var)
      }
    }
    rv$step2_confirmed <- FALSE
    showNotification("Step 3 model and mapping reset.", type = "message")
  })
  
  observeEvent(
    list(
      input$time_var, input$status_var, input$arm_var, input$strata_var,
      input$model_selection
    ),
    {
      rv$step2_confirmed <- FALSE
    },
    ignoreInit = TRUE
  )
  
  # ---------- Covariate details UI ----------
  
  # Continuous parameter UI
  output$cont_param_ui <- renderUI({
    switch(input$cont_dist %||% "normal",
           normal = tagList(
             numericInput("p_mean", "mean", value = 0),
             numericInput("p_sd",   "sd", value = 1, min = 0)
           ),
           lognormal = tagList(
             numericInput("p_meanlog", "meanlog", value = 0),
             numericInput("p_sdlog",   "sdlog", value = 1, min = 0)
           ),
           gamma = tagList(
             numericInput("p_shape", "shape", value = 2, min = 0.001),
             numericInput("p_scale", "scale", value = 1, min = 0.0001)
           ),
           weibull = tagList(
             numericInput("p_wshape", "shape", value = 1.5, min = 0.0001),
             numericInput("p_wscale", "scale", value = 1, min = 0.0001)
           ),
           uniform = tagList(
             numericInput("p_min", "min", value = 0),
             numericInput("p_max", "max", value = 1)
           ),
           t = tagList(
             numericInput("p_df", "df", value = 5, min = 1)
           ),
           beta = tagList(
             numericInput("p_shape1", "shape1", value = 2, min = 0.0001),
             numericInput("p_shape2", "shape2", value = 2, min = 0.0001)
           )
    )
  })
  
  # Category rows table & actions
  observeEvent(input$add_cat_row, {
    nm <- input$cat_add_name %||% ""
    if (!nzchar(nm)) nm <- paste0("L", nrow(rv$cat_rows) + 1)
    pr <- input$cat_add_prob
    if (!is.na(pr) && (pr < 0 || pr > 1)) { showNotification("Probability must be between 0 and 1.", type = "warning"); return() }
    cf <- input$cat_add_coef
    if (!is.finite(cf)) { showNotification("Coefficient must be numeric.", type = "warning"); return() }
    
    rv$cat_rows <- dplyr::bind_rows(rv$cat_rows, tibble::tibble(cat = nm, prob = pr, coef = cf))
    
    # NEW: clear the per-row inputs immediately after adding
    reset_cat_entry_ui(session)
  })
  
  observeEvent(input$reset_cat_rows, { rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric()) })
  output$cat_table <- renderDT({
    if (!nrow(rv$cat_rows)) {
      DT::datatable(
        data.frame(Message="No categories yet - add rows above."),
        options = list(dom='t'),
        rownames = FALSE,
        selection = "none"
      )
    } else {
      DT::datatable(rv$cat_rows, options = list(pageLength = 25, scrollX = TRUE),
                    rownames = FALSE, selection = "single")
    }
  })
  observeEvent(input$remove_cat_row, {
    idx <- input$cat_table_rows_selected
    if (length(idx) && idx >= 1 && idx <= nrow(rv$cat_rows)) {
      rv$cat_rows <- rv$cat_rows[-idx, , drop = FALSE]
    } else {
      showNotification("Select a category row to remove.", type = "warning")
    }
  })
  
  
  # Transform visibility (continuous only)
  
  # Reset builder
  observeEvent(input$reset_cov_builder, {
    updateTextInput(session, "cov_name", value = "")
    updateSelectInput(session, "cov_type", selected = "continuous")
    updateSelectInput(session, "cont_dist", selected = "normal")
    updateNumericInput(session, "cont_beta", value = 0)
    updateNumericInput(session, "tf_center", value = 0)
    updateNumericInput(session, "tf_scale", value = 1)
    rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
    reset_cat_entry_ui(session)   # add this
  })
  
  observeEvent(input$reset_cat_rows, {
    rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
    reset_cat_entry_ui(session)   # nice touch
  })
  # Helper: auto covariate name x1, x2...
  next_cov_name <- reactive({
    nm <- input$cov_name
    if (nzchar(nm)) return(nm)
    existing <- vapply(rv$covariates, function(d) d$name, character(1))
    i <- 1
    repeat {
      cand <- paste0("x", i)
      if (!(cand %in% existing)) return(cand)
      i <- i + 1
    }
  })
  
  # Add covariate to list
  observeEvent(input$add_cov, {
    req(input$cov_type)
    vname <- next_cov_name()
    
    if (input$cov_type == "continuous") {
      # ------- CONTINUOUS -------
      cont_dist <- input$cont_dist %||% "normal"
      if (length(cont_dist) != 1 || !nzchar(cont_dist)) cont_dist <- "normal"
      cont_dist <- as.character(cont_dist[[1]])
      pars <- switch(cont_dist,
                     normal    = list(mean = input$p_mean, sd = input$p_sd),
                     lognormal = list(meanlog = input$p_meanlog, sdlog = input$p_sdlog),
                     gamma     = list(shape = input$p_shape,   scale = input$p_scale),
                     weibull   = list(shape = input$p_wshape,  scale = input$p_wscale),
                     uniform   = list(min   = input$p_min,     max   = input$p_max),
                     t         = list(df    = input$p_df),
                     beta      = list(shape1 = input$p_shape1, shape2 = input$p_shape2),
                     list())
      tf <- c(sprintf("center(%s)", input$tf_center), sprintf("scale(%s)", input$tf_scale))
      beta_val <- suppressWarnings(as.numeric(input$cont_beta))
      if (length(beta_val) != 1L || !is.finite(beta_val)) {
        showNotification("Continuous coefficient must be a single number.", type = "error"); return()
      }
      
      rv$covariates <- c(rv$covariates, list(list(
        name = vname, type = "continuous", dist = cont_dist, params = pars,
        transform = tf, beta = beta_val
      )))
      
      # >>> RESET UI after adding a continuous covariate
      reset_cov_builder_ui(session)
      rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
      reset_cat_entry_ui(session)
      
    } else {
      # ------- CATEGORICAL -------
      if (!nrow(rv$cat_rows)) { showNotification("Add at least one category.", type = "error"); return() }
      
      cats <- rv$cat_rows$cat
      prob <- rv$cat_rows$prob
      coef <- rv$cat_rows$coef
      
      # Validate coefs
      if (any(!is.finite(coef))) { showNotification("All category coefficients must be numeric.", type="error"); return() }
      
      # Fill NA probs with equal shares of the remainder
      p_known <- prob
      p_known[is.na(p_known)] <- 0
      rem <- 1 - sum(p_known)
      if (rem < -1e-8) { showNotification("Sum of specified probabilities exceeds 1.", type="error"); return() }
      if (any(is.na(prob))) {
        k_na <- sum(is.na(prob))
        add_each <- if (k_na > 0) rem / k_na else 0
        prob[is.na(prob)] <- add_each
        rem <- 1 - sum(prob)
      }
      
      # If still < 1, append a remainder category
      if (rem > 1e-8) {
        new_name <- paste0("category-", length(cats) + 1)
        cats <- c(cats, new_name)
        prob <- c(prob, rem)
        coef <- c(coef, 0)  # neutral effect for auto-added level
      }
      
      # Special case: only one level entered with prob < 1
      if (length(cats) == 1 && abs(1 - prob[1]) > 1e-8) {
        cats <- c(cats, paste0("category-", 2))
        prob <- c(prob, 1 - prob[1])
        coef <- c(coef, 0)
      }
      
      # Final check
      if (any(prob < 0) || abs(sum(prob) - 1) > 1e-6) {
        showNotification("Category probabilities must be >= 0 and sum to 1 (after auto-completion).", type="error"); return()
      }
      
      # Bernoulli vs multiclass
      if (length(cats) == 2) {
        pars <- list(p = prob[2])
        rv$covariates <- c(rv$covariates, list(list(
          name = vname, type = "categorical", dist = "bernoulli",
          params = pars, transform = NULL, beta = coef
        )))
      } else {
        pars <- list(prob = prob, labels = cats)
        rv$covariates <- c(rv$covariates, list(list(
          name = vname, type = "categorical", dist = "categorical",
          params = pars, transform = NULL, beta = coef
        )))
      }
      
      # >>> RESET UI after adding a categorical covariate
      reset_cov_builder_ui(session)
      rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
      reset_cat_entry_ui(session)
    }
  })
  
  
  # Covariate list table
  output$cov_table <- renderDT({
    if (!length(rv$covariates)) return(DT::datatable(data.frame(), selection = "none"))
    show <- purrr::map_df(rv$covariates, function(d) {
      beta_txt <- if (length(d$beta)>1) paste(d$beta, collapse=", ") else as.character(d$beta)
      tibble::tibble(
        name = d$name, type = d$type, dist = d$dist,
        params = paste(names(d$params), unlist(d$params), sep="=", collapse="; "),
        transform = paste(d$transform %||% character(0), collapse = ", "),
        beta = beta_txt
      )
    })
    DT::datatable(show, options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE, selection = "single")
  })
  # Remove selected covariate
  observeEvent(input$remove_cov, {
    idx <- input$cov_table_rows_selected
    if (length(idx) && idx >= 1) {
      # remove the idx-th covariate from the list
      rv$covariates <- rv$covariates[-idx]
    } else {
      showNotification("Select a covariate to remove.", type = "warning")
    }
  })
  
  observeEvent(input$confirm_covariates, {
    if (!length(rv$covariates)) {
      showNotification("Add at least one covariate before confirming.", type = "warning")
      return()
    }
    try(bslib::accordion_panel_close("step1_accordion", "1A. Covariate Builder"), silent = TRUE)
    bslib::accordion_panel_open("step1_accordion", "1B. Event Time Settings")
    showNotification("Covariates confirmed. Continue with Event Time Settings.", type = "message")
  })
  
  
  # Baseline UI (grouped)
  output$sim_baseline_ui <- renderUI({
    pad <- function(x) div(style = "display:inline-block; margin-right:12px;", x)
    switch(input$sim_model %||% "aft_lognormal",
           aft_lognormal = div(pad(numericInput("b_mu", "mu", value = 2.3)),
                               pad(numericInput("b_sigma", "sigma", value = 0.5, min = 0.0001))),
           aft_weibull   = div(pad(numericInput("b_shape", "shape", value = 1.5, min = 0.0001)),
                               pad(numericInput("b_scale", "scale", value = 10,  min = 0.0001))),
           ph_exponential= div(pad(numericInput("b_rate", "rate", value = 0.05, min = 0.000001))),
           ph_weibull    = div(pad(numericInput("b_wshape2", "shape", value = 1.3, min = 0.0001)),
                               pad(numericInput("b_wscale2", "scale", value = 8,   min = 0.0001))),
           ph_pwexp      = div(pad(textInput("b_rates", "rates (comma)", value = "0.05,0.02")),
                               pad(textInput("b_cuts",  "cuts (comma)",  value = "5")))
    )
  })
  
  output$download_csv_template <- downloadHandler(
    filename = function() "rmst_template.csv",
    contentType = "text/csv",
    content = function(file) {
      tmpl <- data.frame(
        time = c(10.5, 8.2, 12.4, 5.6),
        status = c(1, 0, 1, 0),
        arm = c(0, 1, 0, 1),
        strata = c("A", "A", "B", "B"),
        x1 = c(0.2, -1.1, 0.5, 1.4),
        x2 = c(56, 62, 49, 73)
      )
      utils::write.csv(tmpl, file, row.names = FALSE)
    }
  )
  
  # Upload
  read_uploaded_dataset <- function(path, original_name) {
    ext <- tolower(tools::file_ext(original_name %||% ""))
    if (identical(ext, "csv")) {
      return(utils::read.csv(path, check.names = FALSE))
    }
    if (ext %in% c("txt", "tsv")) {
      df <- tryCatch(utils::read.delim(path, check.names = FALSE), error = function(e) NULL)
      if (!is.null(df) && ncol(df) > 1) return(df)
      df2 <- tryCatch(utils::read.csv(path, check.names = FALSE), error = function(e) NULL)
      if (!is.null(df2)) return(df2)
      stop("Could not parse text upload as TSV or CSV.", call. = FALSE)
    }
    if (identical(ext, "rds")) {
      obj <- readRDS(path)
      if (is.data.frame(obj)) return(obj)
      if (is.matrix(obj)) return(as.data.frame(obj, check.names = FALSE))
      stop("RDS file must contain a data.frame or matrix.", call. = FALSE)
    }
    if (ext %in% c("rdata", "rda")) {
      e <- new.env(parent = emptyenv())
      loaded <- load(path, envir = e)
      if (!length(loaded)) stop("RData file does not contain any objects.", call. = FALSE)
      objs <- mget(loaded, envir = e, inherits = FALSE)
      pick_name <- names(objs)[which(vapply(objs, is.data.frame, logical(1)))[1]]
      if (is.na(pick_name) || !nzchar(pick_name)) {
        pick_name <- names(objs)[which(vapply(objs, is.matrix, logical(1)))[1]]
        if (is.na(pick_name) || !nzchar(pick_name)) {
          stop("RData file must contain a data.frame or matrix object.", call. = FALSE)
        }
      }
      obj <- objs[[pick_name]]
      if (is.matrix(obj)) obj <- as.data.frame(obj, check.names = FALSE)
      return(obj)
    }
    stop("Unsupported upload type. Use CSV, TXT/TSV, RDS, or RData.", call. = FALSE)
  }

  is_upload_like_source <- function(src) {
    identical(src, "uploaded") || identical(src, "bundled")
  }

  get_bundled_test_datasets <- function() {
    out <- NULL
    data_file <- file.path(app_root, "data", "app_method_datasets.rda")
    if (file.exists(data_file)) {
      e <- new.env(parent = emptyenv())
      try(load(data_file, envir = e), silent = TRUE)
      if (exists("app_method_datasets", envir = e, inherits = FALSE)) {
        out <- get("app_method_datasets", envir = e, inherits = FALSE)
      }
    }
    if (is.null(out) && "package:RMSTpowerBoostApp" %in% search()) {
      out <- tryCatch({
        data("app_method_datasets", package = "RMSTpowerBoostApp", envir = environment())
        get("app_method_datasets", envir = environment(), inherits = FALSE)
      }, error = function(e) NULL)
    }
    if (is.null(out) || !is.list(out)) return(list())
    out
  }

  ingest_uploaded_like_data <- function(df, source_label = "uploaded") {
    if (is.null(df) || !nrow(df)) {
      showNotification("Loaded data is empty or could not be parsed.", type = "error")
      return(invisible(FALSE))
    }
    rv$data_df_raw_upload <- df
    rv$missing_summary <- compute_missing_summary(df)
    rv$cleaning_logs <- character(0)
    n_miss_total <- sum(is.na(df))
    total_cells <- nrow(df) * ncol(df)
    max_miss_pct <- if (ncol(df) > 0) max(100 * colMeans(is.na(df))) else 0
    overall_miss_pct <- if (total_cells > 0) 100 * n_miss_total / total_cells else 0
    rv$cleaning_required <- isTRUE(n_miss_total > 0)
    rv$recommended_clean_mode <- if (!rv$cleaning_required) {
      "ignore"
    } else if (max_miss_pct >= 40) {
      "both"
    } else if (overall_miss_pct >= 5) {
      "impute"
    } else {
      "ignore"
    }
    if (isTRUE(rv$cleaning_required)) {
      append_clean_log(sprintf("Upload detected %d missing values across %d columns.", n_miss_total, ncol(df)))
      append_clean_log(sprintf("Auto recommendation: %s", rv$recommended_clean_mode))
      append_clean_log("Data cleaning is required before reliable analysis.")
      rv$cleaning_report <- list(
        mode = "pending",
        used_mice = FALSE,
        message = "Missing values detected in uploaded data. Data cleaning is required before analysis.",
        mice_parameters = NULL
      )
    } else {
      append_clean_log("Upload has no missing values. Data cleaning step skipped.")
      rv$cleaning_report <- list(
        mode = "complete_data",
        used_mice = FALSE,
        message = "Complete data used (no missing values detected in uploaded dataset).",
        mice_parameters = NULL
      )
    }
    rv$data_df <- df
    rv$data_source <- source_label
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    rv$auto_run_pending <- FALSE
    updateTabsetPanel(session, "main_tabs", selected = "Data")
    invisible(TRUE)
  }

  observe({
    ds <- get_bundled_test_datasets()
    choices <- names(ds)
    if (!length(choices)) choices <- c("(No bundled datasets found)" = "")
    updateSelectInput(session, "bundled_dataset_choice", choices = choices, selected = choices[1])
  })
  
  observeEvent(input$pilot_data_upload, {
    req(input$pilot_data_upload)
    df <- tryCatch(
      read_uploaded_dataset(input$pilot_data_upload$datapath, input$pilot_data_upload$name),
      error = function(e) {
        showNotification(paste("Error reading uploaded file:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(df) || !nrow(df)) {
      showNotification("Uploaded data is empty or could not be parsed.", type = "error")
      return()
    }
    ingest_uploaded_like_data(df, source_label = "uploaded")
  })

  observeEvent(input$load_bundled_dataset, {
    ds <- get_bundled_test_datasets()
    key <- input$bundled_dataset_choice %||% ""
    if (!nzchar(key) || !key %in% names(ds)) {
      showNotification("No built-in dataset selected.", type = "warning")
      return()
    }
    df <- ds[[key]]
    if (!is.data.frame(df)) {
      showNotification("Selected built-in object is not a data.frame.", type = "error")
      return()
    }
    ok <- ingest_uploaded_like_data(df, source_label = "bundled")
    if (isTRUE(ok)) showNotification(paste("Loaded built-in dataset:", key), type = "message")
  })
  
  # Reset data (generation card)
  observeEvent(input$reset_generate, {
    rv$data_df <- NULL
    rv$data_df_raw_upload <- NULL
    rv$missing_summary <- NULL
    rv$cleaning_logs <- character(0)
    rv$cleaning_mode <- NULL
    rv$cleaning_report <- list(
      mode = "complete_data",
      used_mice = FALSE,
      message = "Complete data used.",
      mice_parameters = NULL
    )
    rv$cleaning_required <- FALSE
    rv$recommended_clean_mode <- "ignore"
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    rv$data_source <- NULL
    rv$provenance <- NULL
    rv$auto_run_pending <- FALSE
    rv$covariates <- list()
    rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
    reset_cov_builder_ui(session)
    reset_cat_entry_ui(session)
    updateTabsetPanel(session, "main_tabs", selected = "Pipeline")
    shinyjs::show(id = "simulate_panel")
    showNotification("Data and covariate builder reset.", type="message")
  })
  
  # Generate
  observeEvent(input$generate_sim, {
    v <- sim_validation()
    if (!isTRUE(v$ok)) {
      showNotification(
        paste(c("Cannot generate data:", v$messages), collapse = " "),
        type = "error",
        duration = 8
      )
      return()
    }
    if (!length(rv$covariates)) { showNotification("Please add at least one covariate before simulating.", type = "warning"); return() }
    include_intercept <- isTRUE(input$intercept_in_mm)
    cov_defs <- lapply(rv$covariates, function(d){
      list(name = d$name, type = d$type, dist = d$dist, params = d$params, transform = d$transform)
    })
    user_betas <- list()
    if (include_intercept) user_betas[["(Intercept)"]] <- 0
    for (d in rv$covariates) {
      if (d$type == "continuous") user_betas[[d$name]] <- as.numeric(d$beta) else {
        lv <- if (d$dist == "bernoulli") c("0","1") else (d$params$labels %||% paste0(d$name, seq_along(d$params$prob)))
        need <- if (include_intercept) length(lv)-1 else length(lv)
        if (length(d$beta) < need) { showNotification(sprintf("'%s' needs %d coefficients but %d provided.", d$name, need, length(d$beta)), type="error"); return() }
        user_betas[[d$name]] <- as.numeric(d$beta)
      }
    }
    beta_vec <- assemble_beta(cov_defs, user_betas, include_intercept = include_intercept, intercept_value = input$user_intercept)
    mm_cols  <- build_mm_columns(cov_defs, include_intercept = include_intercept)
    baseline <- switch(input$sim_model,
                       "aft_lognormal" = list(mu = as.numeric(input$b_mu), sigma = as.numeric(input$b_sigma)),
                       "aft_weibull"   = list(shape = as.numeric(input$b_shape), scale = as.numeric(input$b_scale)),
                       "ph_exponential"= list(rate = as.numeric(input$b_rate)),
                       "ph_weibull"    = list(shape = as.numeric(input$b_wshape2), scale = as.numeric(input$b_wscale2)),
                       "ph_pwexp"      = {
                         rates <- suppressWarnings(as.numeric(trimws(strsplit(input$b_rates %||% "", ",")[[1]])))
                         cuts  <- trimws(strsplit(input$b_cuts %||% "", ",")[[1]])
                         cuts  <- if (length(cuts) == 1 && cuts == "") numeric(0) else suppressWarnings(as.numeric(cuts))
                         list(rates = rates, cuts = cuts)
                       })
    if (any(vapply(baseline, function(x) any(!is.finite(as.numeric(x))), logical(1)))) {
      showNotification("Baseline parameters are missing/invalid. Open '1b. Event Time Settings' and provide valid values.", type = "error")
      return()
    }
    form <- as.formula(paste0(if (include_intercept) "~ 1 +" else "~ -1 +",
                              paste(vapply(cov_defs, function(d) d$name, character(1)), collapse = " + ")))
    effects_list <- list(
      intercept = if (include_intercept) 0 else input$user_intercept,
      treatment = input$sim_treat_eff,
      formula   = deparse(form),
      beta      = beta_vec
    )
    effective_sim_seed <- if (is.na(input$sim_seed)) sample.int(.Machine$integer.max, 1L) else as.integer(input$sim_seed)
    rv$effective_sim_seed <- effective_sim_seed
    rec <- list(
      n = as.integer(input$sim_n),
      covariates = list(defs = cov_defs),
      treatment = list(assignment = "randomization", allocation = input$sim_allocation),
      event_time = list(model = input$sim_model, baseline = baseline, effects = effects_list),
      censoring = list(mode = "target_overall", target = input$sim_cens, admin_time = Inf),
      seed = effective_sim_seed
    )
    buf <- capture.output({
      cat("---- Data Simulation ----\n"); print(str(rec))
      cat("Model matrix columns order:\n"); print(mm_cols)
      cat("Beta vector:\n"); print(beta_vec)
    })
    rv$console_buf <- c(rv$console_buf, buf)
    dat <- tryCatch({ simulate_from_recipe(rec, seed = rec$seed) }, error = function(e) { showNotification(paste("Simulation failed:", e$message), type = "error"); NULL })
    if (is.null(dat)) return()
    rv$data_df <- dat
    rv$data_df_raw_upload <- NULL
    rv$missing_summary <- NULL
    rv$cleaning_logs <- character(0)
    rv$cleaning_mode <- NULL
    rv$cleaning_report <- list(
      mode = "complete_data",
      used_mice = FALSE,
      message = "Complete data used (generated dataset).",
      mice_parameters = NULL
    )
    rv$cleaning_required <- FALSE
    rv$recommended_clean_mode <- "ignore"
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    rv$data_source <- "simulated"
    rv$auto_run_pending <- FALSE
    showNotification("Simulation complete.", type = "message")
    updateTabsetPanel(session, "main_tabs", selected = "Data")
    rv$provenance <- list(
      Source = "simulated",
      Number_of_rows = nrow(dat),
      Variable_names = names(dat),
      Covariates_defined = lapply(rv$covariates, function(d) d),
      Event_time = list(model = input$sim_model, baseline = baseline),
      Treatment  = list(assignment = "randomization", allocation = input$sim_allocation),
      Effects    = list(treatment = input$sim_treat_eff,
                        intercept_report = if (include_intercept) "(in model.matrix beta)" else input$user_intercept,
                        intercept_in_mm = include_intercept,
                        formula = deparse(form),
                        mm_cols = mm_cols),
      Censoring  = list(mode = "target_overall", target = input$sim_cens, admin_time = Inf)
    )
  })
  
  # Column mapping (now includes strata & dep. censoring status when needed)
  output$col_mapping_ui <- renderUI({
    df <- rv$data_df; req(df)
    cn <- names(df)
    
    # Which models need extra fields?
    needs_strata <- (input$model_selection %in% c("Additive Stratified Model", "Multiplicative Stratified Model"))
    needs_dep    <- (input$model_selection %in% c("Dependent Censoring Model"))
    
    # Safe defaults for the core mappings
    default_time   <- if ("time"   %in% cn) "time"   else cn[1]
    default_status <- if ("status" %in% cn) "status" else cn[min(2, length(cn))]
    default_arm    <- if ("arm"    %in% cn) "arm"    else cn[min(3, length(cn))]
    
    tagList(
      # Base mappings: time / status / arm
      fluidRow(
        column(
          4,
          selectInput(
            "time_var", "Time-to-Event",
            choices = cn, selected = default_time
          )
        ),
        column(
          4,
          selectInput(
            "status_var", "Status (1=event)",
            choices = cn, selected = default_status
          )
        ),
        column(
          4,
          selectInput(
            "arm_var", "Treatment Arm (1=treat)",
            choices = cn, selected = default_arm
          )
        )
      ),
      
      # Stratification (if applicable)
      if (needs_strata) {
        fluidRow({
          cand_strata <- setdiff(cn, unique(na.omit(c(input$time_var, input$status_var, input$arm_var))))
          default_strata <- if (length(cand_strata)) cand_strata[1] else NULL
          column(
            6,
            selectInput(
              "strata_var", "Stratification Variable",
              choices = cand_strata, selected = default_strata
            )
          )
        })
      },
      
      # Dependent censoring (linear terms for Cox G-model; no arm, no status)
      if (needs_dep) {
        fluidRow({
          cand_dep <- setdiff(
            cn,
            unique(na.omit(c(input$time_var, input$status_var, input$arm_var, input$strata_var)))
          )
          def_dep <- intersect(c("age", "sex", "x"), cand_dep)
          if (!length(def_dep)) def_dep <- NULL
          
          column(
            12,
            selectizeInput(
              "dc_linear_terms",
              "Covariates for censoring Cox model (linear terms)",
              choices  = cand_dep,
              selected = def_dep,
              multiple = TRUE,
              options  = list(placeholder = "Choose 0+ covariates")
            ),
            helpText("Censoring model: Surv(time, status == 0) ~ <selected terms>. Treatment is excluded.")
          )
        })
      }
    )
  })
  
  
  # Analysis inputs
  output$analysis_inputs_ui <- renderUI({
    if (input$analysis_type == "Power") {
      textInput("sample_sizes", "Sample Sizes (per arm/stratum, comma-separated)", value = "100, 150, 200")
    } else {
      sliderInput("target_power", "Target Power", min = 0.1, max = 1, value = 0.8, step = 0.01)
    }
  })

  observeEvent(rv$data_df, {
    df <- rv$data_df
    req(df)
    cn <- names(df)
    m <- mapping_resolved()
    updateSelectInput(session, "time_var", choices = cn, selected = m$time_var)
    updateSelectInput(session, "status_var", choices = cn, selected = m$status_var)
    updateSelectInput(session, "arm_var", choices = cn, selected = m$arm_var)
    if ("strata_var" %in% names(input)) {
      cand_strata <- setdiff(cn, unique(na.omit(c(m$time_var, m$status_var, m$arm_var))))
      updateSelectInput(
        session,
        "strata_var",
        choices = cand_strata,
        selected = if (!is.null(m$strata_var) && m$strata_var %in% cand_strata) m$strata_var else NULL
      )
    }
  }, ignoreInit = FALSE)
  
  # Data Preview
  output$data_preview_table <- DT::renderDataTable({ req(rv$data_df); DT_25(rv$data_df) })

  output$missing_profile_ui <- renderUI({
    ms <- rv$missing_summary
    if (is.null(ms) || !nrow(ms)) return(NULL)
    div(
      class = "section-card",
      h4(class = "section-title", "Missingness Profile"),
      kable_html_safe(ms, caption = "Missingness profile by column", include_rownames = FALSE)
    )
  })
  
  output$data_cleaning_sidebar_ui <- renderUI({
    if (!is_upload_like_source(rv$data_source)) return(NULL)
    ms <- rv$missing_summary
    if (is.null(ms) || !nrow(ms)) return(NULL)
    total_missing <- sum(ms$Missing_Count)
    missing_cols <- ms$Column[ms$Missing_Count > 0]
    selected_mode <- isolate(input$clean_action_mode %||% rv$recommended_clean_mode %||% "ignore")
    tagList(
      if (total_missing > 0) div(
        class = "section-card",
        h4("Cleaning Action"),
        radioButtons(
          "clean_action_mode",
          "Choose missing-data handling strategy",
          choices = c(
            "1) Ignore missing values" = "ignore",
            "2) Use imputation (MICE)" = "impute",
            "3) Do both (drop + impute)" = "both"
          ),
          selected = selected_mode
        ),
        if ((input$clean_action_mode %||% selected_mode) %in% c("ignore", "both")) tagList(
          radioButtons(
            "ignore_drop_mode",
            "For dropping, choose strategy",
            choices = c("Drop rows with missing entries" = "rows", "Drop columns with missing entries" = "cols", "Drop both rows and columns" = "both"),
            selected = "rows"
          ),
          selectizeInput("ignore_drop_cols", "Columns to drop (when column drop is selected)", choices = missing_cols, selected = missing_cols, multiple = TRUE)
        ),
        if ((input$clean_action_mode %||% selected_mode) %in% c("impute", "both")) tagList(
          fluidRow(
            column(4, numericInput("mice_m", "Number of datasets (m)", value = 5, min = 1)),
            column(4, numericInput("mice_maxit", "Iterations (maxit)", value = 5, min = 1)),
            column(4, textInput("mice_seed", "Imputation seed (optional)", value = ""))
          ),
          selectInput("mice_method", "MICE method", choices = c("pmm", "norm", "cart", "rf"), selected = "pmm"),
          if (identical(input$clean_action_mode %||% "", "both")) tagList(
            sliderInput("drop_pct_threshold", "Drop columns with missing % above", min = 0, max = 100, value = 40, step = 1)
          )
        ),
        actionButton("apply_data_cleaning", "Apply", class = "btn btn-primary")
      )
    )
  })
  
  observeEvent(input$apply_data_cleaning, {
    req(is_upload_like_source(rv$data_source))
    df0 <- rv$data_df_raw_upload %||% rv$data_df
    req(df0)
    mode <- input$clean_action_mode %||% "ignore"
    rv$cleaning_mode <- mode
    append_clean_log(sprintf("Cleaning mode selected: %s", mode))
    d <- df0
    dropped_rows <- 0L
    dropped_cols <- character(0)
    threshold_dropped_cols <- character(0)
    mice_details <- NULL
    mice_used <- FALSE
    mice_unimputed_cols <- character(0)
    post_impute_missing_cols <- character(0)
    if (mode %in% c("ignore", "both")) {
      if (mode == "both") {
        th <- as.numeric(input$drop_pct_threshold %||% 40)
        miss_pct <- 100 * colMeans(is.na(d))
        drop_by_th <- names(miss_pct)[miss_pct > th]
        if (length(drop_by_th)) {
          d <- d[, setdiff(names(d), drop_by_th), drop = FALSE]
          threshold_dropped_cols <- unique(c(threshold_dropped_cols, drop_by_th))
          dropped_cols <- unique(c(dropped_cols, drop_by_th))
          append_clean_log(sprintf("Dropped %d columns above %.1f%% missingness: %s", length(drop_by_th), th, paste(drop_by_th, collapse = ", ")))
        } else {
          append_clean_log(sprintf("No columns exceeded %.1f%% missingness threshold.", th))
        }
      }
      drop_mode <- input$ignore_drop_mode %||% "rows"
      drop_cols <- intersect(input$ignore_drop_cols %||% character(0), names(d))
      if (drop_mode %in% c("cols", "both") && length(drop_cols)) {
        d <- d[, setdiff(names(d), drop_cols), drop = FALSE]
        dropped_cols <- unique(c(dropped_cols, drop_cols))
        append_clean_log(sprintf("Dropped selected columns: %s", paste(drop_cols, collapse = ", ")))
      }
      if (drop_mode %in% c("rows", "both")) {
        before <- nrow(d)
        keep <- stats::complete.cases(d)
        d <- d[keep, , drop = FALSE]
        dropped_rows <- as.integer(before - nrow(d))
        append_clean_log(sprintf("Dropped %d rows containing missing values.", dropped_rows))
      }
    }
    if (mode %in% c("impute", "both")) {
      if (!requireNamespace("mice", quietly = TRUE)) {
        showNotification("MICE package is required for imputation. Please install package 'mice'.", type = "error", duration = 10)
        append_clean_log("Imputation skipped because package 'mice' is not installed.")
      } else if (anyNA(d)) {
        m_val <- as.integer(input$mice_m %||% 5)
        maxit_val <- as.integer(input$mice_maxit %||% 5)
        seed_txt <- trimws(input$mice_seed %||% "")
        imp_seed <- if (nzchar(seed_txt)) suppressWarnings(as.integer(seed_txt)) else sample.int(.Machine$integer.max, 1L)
        method_default <- input$mice_method %||% "pmm"
        meth <- mice::make.method(d)
        meth[meth != ""] <- method_default
        missing_cols <- names(d)[colSums(is.na(d)) > 0]
        mice_unimputed_cols <- intersect(missing_cols, names(meth)[meth == ""])
        if (length(mice_unimputed_cols)) {
          append_clean_log(sprintf(
            "MICE will not impute %d columns with empty method: %s",
            length(mice_unimputed_cols),
            paste(mice_unimputed_cols, collapse = ", ")
          ))
        }
        if (all(meth == "")) {
          append_clean_log("Imputation skipped because no variables are eligible for MICE (all methods empty).")
        } else {
          append_clean_log(sprintf("Running MICE: m=%d, maxit=%d, method=%s, seed=%s", m_val, maxit_val, method_default, imp_seed))
          imp <- tryCatch(
            mice::mice(d, m = m_val, maxit = maxit_val, method = meth, seed = imp_seed, printFlag = FALSE),
            error = function(e) {
              append_clean_log(paste("MICE failed:", e$message))
              showNotification(paste("MICE failed:", e$message), type = "error", duration = 10)
              NULL
            }
          )
          if (!is.null(imp)) {
            d <- mice::complete(imp, 1)
            mice_used <- TRUE
            mice_details <- list(
              m = m_val,
              maxit = maxit_val,
              method_default = method_default,
              seed = imp_seed,
              completed_dataset = 1L,
              method_by_variable = as.list(meth),
              unimputed_columns = mice_unimputed_cols
            )
            append_clean_log("Imputation complete. Using completed dataset #1 for analysis.")
          }
        }
      } else {
        append_clean_log("Imputation selected but no missing values remained after drop step.")
      }
    }
    rv$data_df <- d
    rv$missing_summary <- compute_missing_summary(d)
    remaining <- sum(is.na(d))
    post_impute_missing_cols <- names(d)[colSums(is.na(d)) > 0]
    rv$cleaning_required <- isTRUE(remaining > 0)
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    rv$cleaning_report <- list(
      mode = mode,
      used_mice = mice_used,
      message = if (isTRUE(mice_used)) {
        "MICE imputation was applied before analysis."
      } else if (identical(mode, "ignore")) {
        "Complete data used after dropping missing rows/columns."
      } else if (identical(mode, "both")) {
        "Drop + imputation pipeline applied. Final analysis dataset used."
      } else {
        "Complete data used."
      },
      dropped_rows = dropped_rows,
      dropped_columns = dropped_cols,
      threshold_dropped_columns = threshold_dropped_cols,
      mice_parameters = mice_details,
      post_impute_missing_columns = post_impute_missing_cols
    )
    append_clean_log(sprintf("Final analysis dataset dimensions: %d rows x %d columns. Remaining missing values: %d", nrow(d), ncol(d), remaining))
    if (length(post_impute_missing_cols)) {
      append_clean_log(sprintf("Columns with remaining missing values: %s", paste(post_impute_missing_cols, collapse = ", ")))
    }
    if (!rv$cleaning_required) {
      append_clean_log("Data cleaning successful: missing values resolved. Step 2 is now available.")
      updateTabsetPanel(session, "main_tabs", selected = "Data")
      showNotification("Data cleaning successful. Proceed to Step 2.", type = "message")
    } else {
      append_clean_log("Data cleaning applied but missing values still remain; adjust settings and re-apply.")
      updateTabsetPanel(session, "main_tabs", selected = "Run Log")
      showNotification("Missing values still remain after cleaning. Review Run Log and re-apply cleaning.", type = "warning", duration = 10)
    }
  })
  
  output$data_export_ui <- renderUI({
    if (is.null(rv$data_df) || !nrow(rv$data_df)) return(NULL)
    if (!identical(rv$data_source, "simulated")) {
      return(div(class = "alert alert-info", "Data export controls are available for generated datasets."))
    }
    fluidRow(
      column(
        4,
        selectInput(
          "generated_data_format",
          "Generated Data Format",
          choices = c("CSV" = "csv", "TXT (Tab-delimited)" = "txt", "TSV" = "tsv", "RDS" = "rds", "RData" = "rdata"),
          selected = "csv"
        )
      ),
      column(
        4,
        br(),
        downloadButton("download_generated_data", "Download Generated Data")
      )
    )
  })
  
  output$download_generated_data <- downloadHandler(
    filename = function() {
      fmt <- input$generated_data_format %||% "csv"
      ext <- switch(fmt, csv = "csv", txt = "txt", tsv = "tsv", rds = "rds", rdata = "RData", "csv")
      paste0("RMSTpowerBoost_generated_", Sys.Date(), ".", ext)
    },
    contentType = "application/octet-stream",
    content = function(file) {
      if (is.null(rv$data_df) || !nrow(rv$data_df) || !identical(rv$data_source, "simulated")) {
        writeLines("No generated dataset is currently available for export.", con = file, useBytes = TRUE)
        return(invisible(NULL))
      }
      fmt <- input$generated_data_format %||% "csv"
      if (identical(fmt, "csv")) {
        utils::write.csv(rv$data_df, file, row.names = FALSE)
      } else if (identical(fmt, "txt")) {
        utils::write.table(rv$data_df, file, sep = "\t", row.names = FALSE, quote = TRUE)
      } else if (identical(fmt, "tsv")) {
        utils::write.table(rv$data_df, file, sep = "\t", row.names = FALSE, quote = TRUE)
      } else if (identical(fmt, "rds")) {
        saveRDS(rv$data_df, file = file)
      } else if (identical(fmt, "rdata")) {
        generated_data <- rv$data_df
        save(generated_data, file = file)
      } else {
        utils::write.csv(rv$data_df, file, row.names = FALSE)
      }
    }
  )
  
  # Covariate plots
  # UI container
  output$cov_plots_ui <- renderUI({
    req(rv$data_df)
    plots <- covariate_plots(rv$data_df, arm_var = input$arm_var, palette = theme_palette())
    if (!length(plots)) return(p("No covariate plots are available."))
    tagList(lapply(names(plots), function(nm) {
      plotlyOutput(safe_output_id("cov_plot_", nm), height = "300px")
    }))
  })
  
  # Render each plot
  observe({
    req(rv$data_df)
    plots <- covariate_plots(rv$data_df, arm_var = input$arm_var, palette = theme_palette())
    lapply(names(plots), function(nm) {
      local({
        id <- safe_output_id("cov_plot_", nm)
        p  <- plots[[nm]]
        output[[id]] <- renderPlotly({ p })
      })
    })
  })
  
  
  # Run analysis
  run_output <- reactiveVal(list(results = NULL, log = "Analysis has not been run yet."))
  console_log <- reactiveVal("")
  run_request <- reactiveVal(0L)

  observeEvent(rv$data_df, {
    run_output(list(results = NULL, log = "Analysis has not been run yet."))
    rv$live_power_active <- FALSE
  }, ignoreInit = TRUE)
  
  # Keep power plot output alive even when its tab is hidden.
  # Register after outputs are initialized to avoid startup ordering issues.
  session$onFlushed(function() {
    try(outputOptions(output, "results_plot", suspendWhenHidden = FALSE), silent = TRUE)
  }, once = TRUE)
  
  observeEvent(input$run_analysis, {
    run_request(run_request() + 1L)
    updateTabsetPanel(session, "main_tabs", selected = "Analysis")
  }, ignoreInit = TRUE)
  
  observe({
    ck <- analysis_checklist()
    current_results <- run_output()$results
    if (isTRUE(ck$ready) && is.null(current_results)) {
      rv$auto_run_pending <- FALSE
    }
  })
  
  run_analysis_results <- eventReactive(run_request(), {
    tryCatch({
      validate(need(isTRUE(analysis_checklist()$ready), "Complete all checklist items before running analysis."))
      m <- mapping_resolved()
      resolved_time_var <- m$time_var
      resolved_status_var <- m$status_var
      resolved_arm_var <- m$arm_var
      resolved_strata_var <- m$strata_var
      sample_sizes_input <- input$sample_sizes %||% "100,150,200"
      target_power_input <- suppressWarnings(as.numeric(input$target_power %||% 0.8))
      if (!is.finite(target_power_input) || target_power_input <= 0 || target_power_input > 1) target_power_input <- 0.8
      reps_seed <- if (identical(input$calc_method, "Repeated")) {
        if (is.na(input$seed_reps)) sample.int(.Machine$integer.max, 1L) else as.integer(input$seed_reps)
      } else {
        NULL
      }
      rv$effective_reps_seed <- reps_seed
      validate(
        need(!is.null(resolved_time_var) && resolved_time_var %in% names(rv$data_df), "Map a valid time column."),
        need(!is.null(resolved_status_var) && resolved_status_var %in% names(rv$data_df), "Map a valid status column."),
        need(!is.null(resolved_arm_var) && resolved_arm_var %in% names(rv$data_df), "Map a valid treatment arm column.")
      )
      on.exit({ rv$live_power_active <- FALSE }, add = TRUE)
      
      analysis_results <- NULL
      log_text <- capture.output({
        withProgress(message = 'Running Analysis', value = 0, {
        cat("--- Analysis Start ---\n")
        cat(sprintf("Model: %s | Method: %s | Type: %s\n", input$model_selection, input$calc_method, input$analysis_type))
        setProgress(0.2, detail = "Preparing analysis data...")
        cat("Preparing analysis data...\n")
        clean_time <- coerce_time_positive(rv$data_df[[resolved_time_var]], resolved_time_var)
        clean_status <- coerce_status_binary(rv$data_df[[resolved_status_var]], resolved_status_var)
        clean_arm <- coerce_arm_binary(rv$data_df[[resolved_arm_var]], resolved_arm_var)
        pilot_data_clean <- rv$data_df
        pilot_data_clean[[resolved_time_var]] <- clean_time
        pilot_data_clean[[resolved_status_var]] <- clean_status
        pilot_data_clean[[resolved_arm_var]] <- clean_arm
        analysis_data <- data.frame(
          time = clean_time,
          status = clean_status,
          arm = factor(clean_arm, levels = c(0, 1))
        )
        if (!is.null(resolved_strata_var) && nzchar(resolved_strata_var) &&
            (input$model_selection %in% c("Additive Stratified Model","Multiplicative Stratified Model"))) {
          analysis_data$stratum <- as.factor(pilot_data_clean[[resolved_strata_var]])
        }
        # ----- Log-rank (stratified if applicable) -----
        setProgress(0.5, detail = "Log-rank test...")
        cat("Running log-rank test...\n")
        logrank_summary_df <- NULL
        analysis_data_for_plot <- NULL
        km_note_text <- NULL
        try({
          cat("\n--- Survival Analysis ---\n")
          if ("stratum" %in% names(analysis_data)) {
            fixed_formula <- as.formula("Surv(time, status) ~ arm + strata(stratum)")
            km_note_text  <- sprintf("<em>Showing KM curves by arm within each stratum of <b>%s</b>.</em>", resolved_strata_var)
          } else {
            fixed_formula <- as.formula("Surv(time, status) ~ arm")
            km_note_text  <- "<em>KM curves by arm.</em>"
          }
          logrank_test <- survdiff(fixed_formula, data = analysis_data)
          p_value <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)
          cat(sprintf("Log-rank completed. Chi-square=%.3f, df=%d, p-value=%s\n",
                      logrank_test$chisq, length(logrank_test$n) - 1,
                      format.pval(p_value, eps = .001, digits = 3)))
          logrank_summary_df <- data.frame(
            Statistic = "Chi-Square",
            Value = round(logrank_test$chisq, 3),
            DF = length(logrank_test$n) - 1,
            `P-Value` = format.pval(p_value, eps = .001, digits = 3)
          )
          analysis_data_for_plot <- analysis_data
        })
        
        # ----- Power / Sample size -----
        setProgress(0.8, detail = if (input$calc_method == "Analytical") "Computing (analytical) ..." else "Computing (repeated) ...")
        cat(sprintf("Computing %s %s...\n", tolower(input$calc_method), tolower(input$analysis_type)))
        if (input$analysis_type == "Power") {
          n_vec <- as.numeric(trimws(strsplit(sample_sizes_input, ",")[[1]]))
          n_vec <- n_vec[is.finite(n_vec) & n_vec > 0]
          if (!length(n_vec)) n_vec <- c(100, 150, 200)
          iter_total <- length(n_vec)
        } else {
          grid <- seq(30, 1000, by = 10)
          iter_total <- length(grid)
        }
        iter_count <- 0L
        iter_step <- if (iter_total > 0) 0.18 / iter_total else 0
        x_label_live <- if (input$model_selection %in% c("Additive Stratified Model", "Multiplicative Stratified Model")) {
          "Sample size per stratum"
        } else {
          "Sample size per arm"
        }
        init_live_power_plot(
          xlab = x_label_live,
          title = sprintf("Method: %s (%s)", tolower(input$calc_method), input$analysis_type),
          target_power = if (input$analysis_type == "Sample Size") target_power_input else NULL
        )
        point_cb <- function(n_value, power_value) {
          push_live_power_point(n_value, power_value)
          iter_count <<- iter_count + 1L
          if (iter_step > 0) {
            incProgress(
              iter_step,
              detail = sprintf("Computed %d/%d points (N=%s, Power=%.3f)", iter_count, iter_total, n_value, power_value)
            )
          }
        }

        make_power_plot <- function(df_power, title_suffix = "") {
          pal <- theme_palette()
          ggplot(df_power, aes(x = N_per_arm, y = Power, group = 1)) +
            geom_line(size = 1.2, alpha = 0.9, color = pal[1]) +
            geom_point(size = 3.5, color = pal[2]) +
            geom_text(
              aes(label = sprintf("N=%s\nP=%.3f", N_per_arm, Power)),
              vjust = -0.6, size = 3, color = pal[2], check_overlap = TRUE
            ) +
            scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0.02, 0.12))) +
            coord_cartesian(ylim = c(0, 1.05), clip = "off") +
            labs(x = "Sample size per arm", y = "Power", title = paste("Method:", title_suffix)) +
            theme(plot.margin = margin(10, 20, 10, 10)) +
            transparent_theme(base_size = 13)
        }
        
        
        if (input$calc_method == "Repeated") {
          R <- input$R_reps %||% 500
          if (input$analysis_type == "Power") {
            cat(sprintf("Repeated method: estimating power at %d sample sizes with R=%d.\n", length(n_vec), R))
            power_df <- repeated_power_from_pilot(
              pilot_data_clean, resolved_time_var, resolved_status_var, resolved_arm_var,
              n_per_arm_vec = n_vec, alpha = input$alpha, R = R,
              strata_var = if ("stratum" %in% names(analysis_data)) resolved_strata_var else NULL,
              seed = reps_seed,
              point_cb = point_cb
            )
            results_plot <- make_power_plot(power_df, "repeated")
            results_data <- power_df
          } else {
            # search minimal N achieving target power
            cat(sprintf("Repeated method: searching required N for target power %.3f with R=%d.\n", target_power_input, R))
            power_df <- repeated_power_from_pilot(
              pilot_data_clean, resolved_time_var, resolved_status_var, resolved_arm_var,
              n_per_arm_vec = grid, alpha = input$alpha, R = R,
              strata_var = if ("stratum" %in% names(analysis_data)) resolved_strata_var else NULL,
              seed = reps_seed,
              point_cb = point_cb
            )
            meet <- subset(power_df, Power >= target_power_input)
            n_star <- if (nrow(meet)) min(meet$N_per_arm) else NA
            results_plot <- make_power_plot(power_df, "repeated") +
              geom_hline(yintercept = target_power_input, linetype = 2) +
              ggplot2::annotate("text", x = max(power_df$N_per_arm), y = target_power_input,
                                label = sprintf("Target %.2f", target_power_input), hjust = 1, vjust = -0.5, size = 3.5)
            results_data <- power_df
            if (!is.na(n_star)) {
              results_plot <- results_plot + ggplot2::annotate("point", x = n_star, y = target_power_input, size = 4)
            }
          }
        } else {
          # ----- ANALYTICAL BRANCH -----
          cat("Analytical method: estimating parameters and asymptotic variance...\n")
          if (input$model_selection == "Dependent Censoring Model") {
            # Use the new DC analytic helpers
            if (input$analysis_type == "Power") {
              n_vec <- as.numeric(trimws(strsplit(sample_sizes_input, ",")[[1]]))
              n_vec <- n_vec[is.finite(n_vec) & n_vec > 0]
              if (!length(n_vec)) n_vec <- c(100,150,200)
              
              dc <- DC.power.analytical.app(
                pilot_data          = pilot_data_clean,
                time_var            = resolved_time_var,
                status_var          = resolved_status_var,
                arm_var             = resolved_arm_var,
                dep_cens_status_var = NULL,  # ignored by the estimator
                sample_sizes        = n_vec,
                linear_terms        = input$dc_linear_terms %||% character(0),
                L                   = input$L,
                alpha               = input$alpha,
                point_cb            = point_cb
              )
              results_plot    <- dc$results_plot
              results_data    <- dc$results_data
              results_summary <- dc$results_summary
              
            } else {
              dc <- DC.ss.analytical.app(
                pilot_data          = pilot_data_clean,
                time_var            = resolved_time_var,
                status_var          = resolved_status_var,
                arm_var             = resolved_arm_var,
                dep_cens_status_var = NULL,  # ignored by the estimator
                target_power        = target_power_input,
                linear_terms        = input$dc_linear_terms %||% character(0),
                L                   = input$L,
                alpha               = input$alpha,
                n_start             = 50,
                n_step              = 25,
                max_n_per_arm       = 2000,
                point_cb            = point_cb
              )
              results_plot    <- dc$results_plot
              results_data    <- dc$results_data
              results_summary <- dc$results_summary
            }
            
          } else if (input$model_selection == "Linear IPCW Model") {
            if (input$analysis_type == "Power") {
              lin <- linear.power.analytical.app(
                pilot_data   = pilot_data_clean,
                time_var     = resolved_time_var,
                status_var   = resolved_status_var,
                arm_var      = resolved_arm_var,
                sample_sizes = n_vec,
                linear_terms = NULL,
                L            = input$L,
                alpha        = input$alpha,
                point_cb     = point_cb
              )
            } else {
              lin <- linear.ss.analytical.app(
                pilot_data    = pilot_data_clean,
                time_var      = resolved_time_var,
                status_var    = resolved_status_var,
                arm_var       = resolved_arm_var,
                target_power  = target_power_input,
                linear_terms  = NULL,
                L             = input$L,
                alpha         = input$alpha,
                n_start       = 50,
                n_step        = 25,
                max_n_per_arm = 2000,
                point_cb      = point_cb
              )
            }
            results_plot    <- lin$results_plot
            results_data    <- lin$results_data
            results_summary <- lin$results_summary
            
          } else if (input$model_selection == "Additive Stratified Model") {
            if (input$analysis_type == "Power") {
              add <- additive.power.analytical.app(
                pilot_data   = pilot_data_clean,
                time_var     = resolved_time_var,
                status_var   = resolved_status_var,
                arm_var      = resolved_arm_var,
                strata_var   = resolved_strata_var,
                sample_sizes = n_vec,
                linear_terms = NULL,
                L            = input$L,
                alpha        = input$alpha,
                point_cb     = point_cb
              )
            } else {
              add <- additive.ss.analytical.app(
                pilot_data    = pilot_data_clean,
                time_var      = resolved_time_var,
                status_var    = resolved_status_var,
                arm_var       = resolved_arm_var,
                strata_var    = resolved_strata_var,
                target_power  = target_power_input,
                linear_terms  = NULL,
                L             = input$L,
                alpha         = input$alpha,
                n_start       = 50,
                n_step        = 25,
                max_n_per_arm = 2000,
                point_cb      = point_cb
              )
            }
            results_plot    <- add$results_plot
            results_data    <- add$results_data
            results_summary <- add$results_summary
            
          } else if (input$model_selection == "Multiplicative Stratified Model") {
            if (input$analysis_type == "Power") {
              mul <- MS.power.analytical.app(
                pilot_data   = pilot_data_clean,
                time_var     = resolved_time_var,
                status_var   = resolved_status_var,
                arm_var      = resolved_arm_var,
                strata_var   = resolved_strata_var,
                sample_sizes = n_vec,
                linear_terms = NULL,
                L            = input$L,
                alpha        = input$alpha,
                point_cb     = point_cb
              )
            } else {
              mul <- MS.ss.analytical.app(
                pilot_data    = pilot_data_clean,
                time_var      = resolved_time_var,
                status_var    = resolved_status_var,
                arm_var       = resolved_arm_var,
                strata_var    = resolved_strata_var,
                target_power  = target_power_input,
                linear_terms  = NULL,
                L             = input$L,
                alpha         = input$alpha,
                n_start       = 50,
                n_step        = 25,
                max_n_per_arm = 2000,
                point_cb      = point_cb
              )
            }
            results_plot    <- mul$results_plot
            results_data    <- mul$results_data
            results_summary <- mul$results_summary
            
          } else if (input$model_selection == "Semiparametric (GAM) Model") {
            stop("Analytical calculation is not implemented for Semiparametric (GAM) Model. Please use the Repeated method.", call. = FALSE)
            
          } else {
            stop(paste("Unsupported model selection:", input$model_selection), call. = FALSE)
          }
        }
        
        results_summary <- data.frame(
          Arm_1 = sum(analysis_data$arm == levels(analysis_data$arm)[1]),
          Arm_2 = sum(analysis_data$arm == levels(analysis_data$arm)[2]),
          Events = sum(analysis_data$status == 1), Censored = sum(analysis_data$status == 0)
        )
        analysis_results <- list(
          results_plot = results_plot,
          results_data = results_data,
          results_summary = results_summary,
          logrank_summary = logrank_summary_df,
          analysis_data_for_plot = analysis_data_for_plot,
          km_note = km_note_text
        )
        if (is.null(analysis_results$results_plot) ||
            is.null(analysis_results$results_data) ||
            !is.data.frame(analysis_results$results_data) ||
            !nrow(analysis_results$results_data)) {
          stop("Analysis did not produce Power/Sample Size outputs. Check model/method compatibility and inputs.", call. = FALSE)
        }
        })
      }, type = c("output","message"))
      
      joined_log <- paste(log_text, collapse = "\n")
      console_log(joined_log)
      list(results = analysis_results, log = joined_log)
    }, error = function(e) {
      msg <- paste("Analysis failed:", conditionMessage(e))
      console_log(msg)
      showNotification(msg, type = "error", duration = 10)
      list(results = NULL, log = msg)
    })
  }, ignoreInit = TRUE)
  
  observeEvent(run_analysis_results(), {
    rv$live_power_active <- FALSE
    run_output(run_analysis_results())
    log_lines <- strsplit(run_analysis_results()$log %||% "", "\n", fixed = TRUE)[[1]]
    rv$last_warning_lines <- grep("warning", log_lines, ignore.case = TRUE, value = TRUE)
    rv$last_error_lines <- grep("error", log_lines, ignore.case = TRUE, value = TRUE)
    shinyjs::show(id = "download_reset_row")
    updateTabsetPanel(session, "main_tabs", selected = "Analysis")
  })
  
  # KM plots (faceted by stratum; at most 2 per row)
  output$km_note <- renderUI({
    note <- run_output()$results$km_note
    if (is.null(note) || !nzchar(note)) note <- "<em>Run analysis to populate Kaplan-Meier interpretation notes.</em>"
    HTML(note)
  })
  pretty_arm_labels <- function(f) {
    lv <- levels(f)
    if (identical(lv, c("0","1")) || identical(lv, c("0", "1")) || identical(lv, c(0,1))) {
      c("Control","Treatment")
    } else lv
  }
  
  output$survival_plotly_output <- renderPlotly({
    m <- mapping_resolved()
    plot_time_var <- m$time_var
    plot_status_var <- m$status_var
    plot_arm_var <- m$arm_var
    plot_strata_var <- m$strata_var
    plot_data <- run_output()$results$analysis_data_for_plot
    if (is.null(plot_data) || !nrow(plot_data)) {
      req(rv$data_df)
      tmp <- rv$data_df
      validate(
        need(!is.null(plot_time_var) && plot_time_var %in% names(tmp), "Map a valid time column."),
        need(!is.null(plot_status_var) && plot_status_var %in% names(tmp), "Map a valid status column."),
        need(!is.null(plot_arm_var) && plot_arm_var %in% names(tmp), "Map a valid arm column.")
      )
      tmp$time <- coerce_time_positive(tmp[[plot_time_var]], plot_time_var)
      tmp$status <- coerce_status_binary(tmp[[plot_status_var]], plot_status_var)
      tmp$arm <- factor(coerce_arm_binary(tmp[[plot_arm_var]], plot_arm_var), levels = c(0, 1))
      needs_strata_plot <- input$model_selection %in% c("Additive Stratified Model", "Multiplicative Stratified Model")
      if (isTRUE(needs_strata_plot) && !is.null(plot_strata_var) && nzchar(plot_strata_var) && plot_strata_var %in% names(tmp)) {
        tmp$stratum <- as.factor(tmp[[plot_strata_var]])
      }
      plot_data <- tmp
    }
    req(input$alpha)
    pal <- theme_palette()
    
    use_strata_panels <- isTRUE(input$model_selection %in% c("Additive Stratified Model", "Multiplicative Stratified Model")) &&
      ("stratum" %in% names(plot_data))
    if (use_strata_panels) {
      str_levels <- levels(as.factor(plot_data$stratum))
      plots <- lapply(seq_along(str_levels), function(i) {
        st <- str_levels[i]
        d <- subset(plot_data, stratum == st)
        d$arm <- factor(d$arm)
        fit <- survfit(Surv(time, status) ~ arm, data = d)
        km_plot_plotly(
          fit,
          conf.int = TRUE, conf.int.alpha = 0.3,
          conf.level = 1 - input$alpha,
          palette = pal[1:2],
          legend.title = plot_arm_var,
          legend.labs = pretty_arm_labels(d$arm),
          xlab = paste("Time in the units of", plot_time_var),
          ylab = "Survival probability",
          title = NULL,
          showlegend = (i == 1)
        )
      })
      ncols <- 2
      nrows <- ceiling(length(plots) / ncols)
      sp <- do.call(plotly::subplot, c(
        plots,
        nrows = nrows,
        shareX = TRUE, shareY = TRUE,
        titleX = TRUE, titleY = TRUE,
        margin = 0.04
      ))
      sp <- add_subplot_titles(sp, paste(plot_strata_var, "=", str_levels))
      plotly::layout(
        sp,
        title = list(
          text = sprintf("Kaplan-Meier - %s by %s", plot_arm_var, plot_strata_var),
          x = 0.02, xanchor = "left"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    } else {
      plot_data$arm <- factor(plot_data$arm)
      fit <- survfit(Surv(time, status) ~ arm, data = plot_data)
      km_plot_plotly(
        fit,
        conf.int = TRUE, conf.int.alpha = 0.3,
        conf.level = 1 - input$alpha,
        palette = pal[1:2],
        legend.title = plot_arm_var,
        legend.labs  = pretty_arm_labels(plot_data$arm),
        xlab = paste("Time in the units of", plot_time_var),
        ylab = "Survival probability",
        title = sprintf("Kaplan-Meier - %s", plot_arm_var),
        showlegend = TRUE
      )
    }
  })
  
  
  
  
  
  # Power plot (supports live incremental updates during computation)
  output$results_plot <- renderPlotly({
    if (isTRUE(rv$live_power_active)) {
      return(build_live_power_plot())
    }
    p <- run_output()$results$results_plot
    validate(need(!is.null(p), "Run analysis to display the power/sample-size plot."))
    as_plotly_clear(p)
  })
  
  # Summary (tables only)
  output$results_table_ui <- renderUI({
    rd <- run_output()$results$results_data
    if (is.null(rd) || !nrow(rd)) {
      return(div(class = "alert alert-danger", "Power/sample-size results are unavailable. Run analysis to generate this table."))
    }
    rd %>%
      kable_html_safe(caption = "Power and sample size results")
  })
  output$summary_table_ui <- renderUI({
    rs <- run_output()$results$results_summary
    if (is.null(rs) || !nrow(rs)) {
      return(div(class = "alert alert-danger", "Analysis summary is unavailable. Run analysis to generate this table."))
    }
    rs %>%
      kable_html_safe(caption = "Summary measures derived from the data")
  })
  output$data_summary_ui <- renderUI({
    if (is.null(rv$data_df) || !nrow(rv$data_df)) {
      return(div(class = "alert alert-warning", "Data summary is unavailable because no dataset is loaded."))
    }
    arm_var <- mapping_resolved()$arm_var
    sm <- covariate_summary(rv$data_df, arm_var = arm_var)
    if (!length(sm)) return(div(class = "alert alert-warning", "No covariate tables are available for the current dataset."))
    ui <- tagList()
    if (!is.null(sm$continuous) && nrow(sm$continuous)) {
      ui <- tagAppendChildren(ui,
                              h5("Continuous covariates"),
                              kable_html_safe(sm$continuous, caption = NULL)
      )
    }
    if (!is.null(sm$categorical) && nrow(sm$categorical)) {
      ui <- tagAppendChildren(ui,
                              h5("Categorical covariates"),
                              kable_html_safe(sm$categorical, caption = NULL)
      )
    }
    ui
  })
  
  output$key_results_ui <- renderUI({
    rd <- run_output()$results$results_data
    if (is.null(rd) || !nrow(rd)) {
      return(div(class = "alert alert-danger", "Key results are unavailable. Run analysis to compute key outputs."))
    }
    if (identical(input$analysis_type, "Power")) {
      target_n <- suppressWarnings(as.numeric(strsplit(input$sample_sizes %||% "100,150,200", ",")[[1]][1]))
      nn <- rd[[grep("^N_", names(rd), value = TRUE)[1] %||% "N_per_arm"]]
      pow <- rd$Power
      idx <- if (is.finite(target_n)) which.min(abs(nn - target_n)) else nrow(rd)
      txt <- sprintf("Nearest power estimate at N=%.3f is %.3f.", as.numeric(nn[idx]), as.numeric(pow[idx]))
      if (identical(input$calc_method, "Repeated") && "SE" %in% names(rd) && is.finite(rd$SE[idx])) {
        txt <- paste0(txt, sprintf(" Monte Carlo SE: %.3f.", as.numeric(rd$SE[idx])))
      }
      div(class = "alert alert-info", txt)
    } else {
      req_row <- rd[1, , drop = FALSE]
      req_col <- grep("Required_N", names(req_row), value = TRUE)[1]
      if (!length(req_col)) return(div(class = "alert alert-warning", "Required sample size is not available in the current output table."))
      target_power <- suppressWarnings(as.numeric(input$target_power %||% 0.8))
      if (!is.finite(target_power)) target_power <- 0.8
      div(class = "alert alert-info", sprintf(
        "Required sample size is %.3f at target power %.3f.",
        as.numeric(req_row[[req_col]]),
        target_power
      ))
    }
  })
  
  output$run_log_summary_ui <- renderUI({
    warns <- unique(rv$last_warning_lines %||% character(0))
    errs <- unique(rv$last_error_lines %||% character(0))
    if (!length(warns) && !length(errs)) return(div(class = "alert alert-success", "No warnings or errors in latest run log."))
    tagList(
      if (length(errs)) div(class = "alert alert-danger", strong("Errors:"), tags$ul(lapply(utils::head(errs, 5), tags$li))),
      if (length(warns)) div(class = "alert alert-warning", strong("Warnings:"), tags$ul(lapply(utils::head(warns, 5), tags$li)))
    )
  })
  
  # Console log (text only)
  output$console_log_output <- renderText({
    clean_part <- rv$cleaning_logs %||% character(0)
    run_part <- console_log() %||% character(0)
    parts <- c(
      if (length(clean_part)) c("=== Data Cleaning Log ===", clean_part, ""),
      if (nzchar(run_part)) c("=== Analysis Log ===", run_part)
    )
    if (!length(parts)) "No log entries yet." else paste(parts, collapse = "\n")
  })
  
  # ------------------ Downloads (PDF & HTML) ------------------
  data_provenance <- reactive({
    prov <- rv$provenance
    if (is.null(prov) && !is.null(rv$data_df)) {
      prov <- list(
        Source = rv$data_source %||% "uploaded",
        Number_of_rows = nrow(rv$data_df),
        Variable_names = names(rv$data_df),
        Covariates_defined = if (rv$data_source == "simulated") lapply(rv$covariates, function(d) d) else list()
      )
    }
    prov
  })
  get_pilot_data <- reactive({ rv$data_df })
  
  output$download_report_pdf <- downloadHandler(
    filename = function() paste0("RMSTpowerBoost_report_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      id <- showNotification("Generating PDF report...", type="message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      tpl <- make_inline_template()
      out_file <- tempfile(fileext = ".pdf")
      on.exit(unlink(out_file), add = TRUE)
      write_pdf_fallback <- function(target_file, reason_message) {
        grDevices::pdf(target_file, width = 8.5, height = 11)
        plot.new()
        text(0.5, 0.70, "RMSTpowerBoost PDF export fallback", cex = 1.15)
        text(0.5, 0.58, paste("Reason:", reason_message %||% "Unknown reason"), cex = 0.92)
        text(0.5, 0.48, "Install TinyTeX/LaTeX for full PDF report rendering.", cex = 0.9)
        text(0.5, 0.40, "You can also use HTML export for full report content.", cex = 0.9)
        grDevices::dev.off()
      }
      if (is.null(run_output()$results)) {
        write_pdf_fallback(file, "No analysis results available for PDF export.")
        showNotification("Run analysis first, then export PDF.", type = "warning", duration = 8)
        return(invisible(NULL))
      }
      sim_seed_report <- rv$effective_sim_seed %||% if (is.na(input$sim_seed)) sample.int(.Machine$integer.max, 1L) else as.integer(input$sim_seed)
      reps_seed_report <- rv$effective_reps_seed %||% if (is.na(input$seed_reps)) sample.int(.Machine$integer.max, 1L) else as.integer(input$seed_reps)
      has_latex <- nzchar(Sys.which("pdflatex")) ||
        (requireNamespace("tinytex", quietly = TRUE) && isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
      if (!isTRUE(has_latex)) {
        write_pdf_fallback(file, "LaTeX engine was not found on this system.")
        showNotification("PDF requires TinyTeX/LaTeX. Exported diagnostic PDF fallback instead.", type = "error", duration = 10)
        return(invisible(NULL))
      }
      tryCatch({
        rendered <- rmarkdown::render(
          input         = tpl,
          output_format = rmarkdown::pdf_document(),
          output_file   = basename(out_file),
          output_dir    = dirname(out_file),
          params        = list(
            inputs          = report_inputs_builder(input),
            results         = run_output()$results,
            log             = paste(console_log(), run_output()$log, sep = "\n"),
            data_provenance = data_provenance(),
            data            = get_pilot_data(),
            data_cleaning   = rv$cleaning_report %||% NULL,
            reproducibility = list(
              timestamp = Sys.time(),
              sim_seed = sim_seed_report,
              seed_reps = reps_seed_report,
              r_version = R.version.string,
              package_versions = {
                pk <- c("shiny","bslib","plotly","survival","dplyr","tidyr","purrr","stringr","rmarkdown")
                ip <- as.data.frame(utils::installed.packages(), stringsAsFactors = FALSE)
                ip <- ip[ip$Package %in% pk, c("Package","Version"), drop = FALSE]
                ip[order(ip$Package), , drop = FALSE]
              },
              session_info = paste(capture.output(utils::sessionInfo()), collapse = "\n")
            )
          ),
          envir         = new.env(parent = globalenv()),
          clean         = TRUE
        )
        produced <- if (is.character(rendered) && length(rendered) == 1) rendered else out_file
        if (!file.exists(produced)) produced <- out_file
        ok <- file.copy(produced, file, overwrite = TRUE)
        if (!isTRUE(ok)) {
          write_pdf_fallback(file, "Failed to prepare PDF download file.")
        }
      }, error = function(e) {
        write_pdf_fallback(file, paste("PDF generation failed:", conditionMessage(e)))
        showNotification(paste("PDF generation failed; exported diagnostic PDF fallback instead:", conditionMessage(e)), type = "error", duration = 10)
      })
    }
  )
  output$download_report_html <- downloadHandler(
    filename = function() paste0("RMSTpowerBoost_report_", Sys.Date(), ".html"),
    contentType = "text/html",
    content = function(file) {
      id <- showNotification("Generating HTML report...", type="message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      tpl <- make_inline_template()
      out_file <- tempfile(fileext = ".html")
      on.exit(unlink(out_file), add = TRUE)
      if (is.null(run_output()$results)) {
        writeLines("<html><body><h2>No analysis results available for HTML export.</h2></body></html>", con = file, useBytes = TRUE)
        showNotification("Run analysis first, then export HTML.", type = "warning", duration = 8)
        return(invisible(NULL))
      }
      sim_seed_report <- rv$effective_sim_seed %||% if (is.na(input$sim_seed)) sample.int(.Machine$integer.max, 1L) else as.integer(input$sim_seed)
      reps_seed_report <- rv$effective_reps_seed %||% if (is.na(input$seed_reps)) sample.int(.Machine$integer.max, 1L) else as.integer(input$seed_reps)
      tryCatch({
        rendered <- rmarkdown::render(
          input         = tpl,
          output_format = rmarkdown::html_document(theme = "flatly", toc = TRUE, toc_depth = 3),
          output_file   = basename(out_file),
          output_dir    = dirname(out_file),
          params        = list(
            inputs          = report_inputs_builder(input),
            results         = run_output()$results,
            log             = paste(console_log(), run_output()$log, sep = "\n"),
            data_provenance = data_provenance(),
            data            = get_pilot_data(),
            data_cleaning   = rv$cleaning_report %||% NULL,
            reproducibility = list(
              timestamp = Sys.time(),
              sim_seed = sim_seed_report,
              seed_reps = reps_seed_report,
              r_version = R.version.string,
              package_versions = {
                pk <- c("shiny","bslib","plotly","survival","dplyr","tidyr","purrr","stringr","rmarkdown")
                ip <- as.data.frame(utils::installed.packages(), stringsAsFactors = FALSE)
                ip <- ip[ip$Package %in% pk, c("Package","Version"), drop = FALSE]
                ip[order(ip$Package), , drop = FALSE]
              },
              session_info = paste(capture.output(utils::sessionInfo()), collapse = "\n")
            )
          ),
          envir         = new.env(parent = globalenv()),
          clean         = TRUE
        )
        produced <- if (is.character(rendered) && length(rendered) == 1) rendered else out_file
        if (!file.exists(produced)) produced <- out_file
        ok <- file.copy(produced, file, overwrite = TRUE)
        if (!isTRUE(ok)) {
          writeLines("<html><body><h2>Failed to prepare HTML download file.</h2></body></html>", con = file, useBytes = TRUE)
        }
      }, error = function(e) {
        html_fallback <- paste0(
          "<html><body><h2>RMSTpowerBoost HTML generation failed</h2><p>",
          htmltools::htmlEscape(conditionMessage(e)),
          "</p></body></html>"
        )
        writeLines(html_fallback, con = out_file, useBytes = TRUE)
        ok_fallback <- file.copy(out_file, file, overwrite = TRUE)
        if (!isTRUE(ok_fallback)) {
          writeLines(html_fallback, con = file, useBytes = TRUE)
        }
        showNotification(paste("HTML generation failed; exported diagnostic HTML instead:", conditionMessage(e)), type = "error", duration = 10)
      })
    }
  )
  
  # Workflow visibility by confirmed steps
  analysis_ready <- reactive({
    has_data <- !is.null(rv$data_df) && nrow(rv$data_df) > 0
    if (!has_data) return(FALSE)
    if (is_upload_like_source(rv$data_source) && isTRUE(rv$cleaning_required)) return(FALSE)
    TRUE
  })
  
  observe({
    has_data <- !is.null(rv$data_df) && nrow(rv$data_df) > 0
    mode <- input$data_mode %||% rv$data_mode %||% "Upload"
    step1_done <- isTRUE(rv$step1_confirmed)
    step2_done <- isTRUE(rv$step2_data_confirmed)
    needs_cleaning <- is_upload_like_source(rv$data_source) && isTRUE(rv$cleaning_required)
    show_step1_panel <- !step1_done ||
      (step1_done && identical(mode, "Generate") && !step2_done) ||
      (step1_done && identical(mode, "Upload") && !has_data)
    show_step2_generate <- step1_done && identical(mode, "Generate") && !step2_done
    show_step2_upload <- step1_done && identical(mode, "Upload") && has_data && !step2_done
    show_step3_4 <- step2_done && isTRUE(analysis_ready())
    shinyjs::toggle(id = "step1_panel", condition = show_step1_panel)
    shinyjs::toggle(id = "step1_choice_block", condition = !step1_done)
    shinyjs::toggle(id = "simulate_panel", condition = show_step2_generate)
    shinyjs::toggle(id = "upload_panel", condition = step1_done && identical(mode, "Upload") && !has_data)
    shinyjs::toggle(id = "data_cleaning_panel", condition = show_step2_upload)
    shinyjs::toggle(id = "model_analysis_panel", condition = show_step3_4)
    shinyjs::toggle(id = "step3_controls", condition = show_step3_4 && !isTRUE(rv$step2_confirmed))
    shinyjs::toggle(id = "step4_controls", condition = show_step3_4 && isTRUE(rv$step2_confirmed))
  })

  observeEvent(input$apply_theme, {
    sel <- input$theme_preset %||% "sandstone"
    session$setCurrentTheme(build_app_theme(sel))
    showNotification(sprintf("Theme applied: %s", sel), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$theme_widget_open, {
    shinyjs::hide("theme_widget_open")
    shinyjs::show("theme_widget_panel")
  }, ignoreInit = TRUE)

  observeEvent(input$theme_widget_close, {
    shinyjs::hide("theme_widget_panel")
    shinyjs::show("theme_widget_open")
  }, ignoreInit = TRUE)
  
  # Reset workflow (always available in Step 2/3)
  observeEvent(input$reset_workflow, {
    rv$covariates <- list()
    rv$cat_rows <- tibble::tibble(cat = character(), prob = numeric(), coef = numeric())
    rv$data_df <- NULL; rv$data_source <- NULL; rv$console_buf <- character(0); rv$provenance <- NULL
    rv$data_df_raw_upload <- NULL
    rv$missing_summary <- NULL
    rv$cleaning_logs <- character(0)
    rv$cleaning_mode <- NULL
    rv$cleaning_report <- list(
      mode = "complete_data",
      used_mice = FALSE,
      message = "Complete data used.",
      mice_parameters = NULL
    )
    rv$cleaning_required <- FALSE
    rv$recommended_clean_mode <- "ignore"
    rv$step1_confirmed <- FALSE
    rv$step2_data_confirmed <- FALSE
    rv$step2_confirmed <- FALSE
    rv$auto_run_pending <- FALSE
    rv$effective_sim_seed <- NULL
    rv$effective_reps_seed <- NULL
    shinyjs::hide("download_reset_row")
    shinyjs::show("simulate_panel")
    updateTabsetPanel(session, "main_tabs", selected = "Pipeline")
    showNotification("Workflow reset to main page.", type = "message")
  })
  
}

shinyApp(ui, server)


