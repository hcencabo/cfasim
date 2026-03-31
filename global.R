
# Libraries, parallel backend, model builders, simulation worker

library(shiny)
library(simsem)
library(lavaan)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(future)
library(promises)
library(furrr)


# Parallel backend — cores - 1, minimum 1

n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
plan(multisession, workers = n_workers)

# Bias flag detection
# These three functions encode the conditions under which Wolf et al.-style
# bias checking is activated.  They operate on raw user inputs, not sim output.

# Flag 1 — factor correlation at or beyond boundary values
flag_cor_boundary <- function(factor_cor, n_factors) {
  n_factors > 1L && (factor_cor >= 0.50 || factor_cor <= 0.10)
}

# Flag 2 — weak loadings (below .50; Wolf et al. did not evaluate this range)
flag_weak_loading <- function(loading_value) {
  loading_value <= 0.40
}

# Flag 3 — any factor has exactly 3 indicators (identification boundary)
flag_id_boundary <- function(items_per_factor) {
  any(items_per_factor == 3L)
}

# Convenience: return a named logical vector of all three flags
get_bias_flags <- function(loading_value, factor_cor, n_factors,
                           items_per_factor) {
  c(
    cor_boundary  = flag_cor_boundary(factor_cor, n_factors),
    weak_loading  = flag_weak_loading(loading_value),
    id_boundary   = flag_id_boundary(items_per_factor)
  )
}


# Helper: Build population model string dynamically

build_population_model <- function(n_factors, items_per_factor,
                                   loading_value, residual_var, factor_cor) {
  lines    <- character(0)
  item_idx <- 1L

  for (f in seq_len(n_factors)) {
    items <- paste0(loading_value, "*x",
                    item_idx:(item_idx + items_per_factor[f] - 1L),
                    collapse = " + ")
    lines    <- c(lines, sprintf("  factor%d =~ %s", f, items))
    item_idx <- item_idx + items_per_factor[f]
  }
  lines <- c(lines, "")

  if (n_factors > 1L) {
    for (f1 in seq_len(n_factors - 1L)) {
      others <- paste0(factor_cor, "*factor", (f1 + 1L):n_factors,
                       collapse = " + ")
      lines <- c(lines, sprintf("  factor%d ~~ %s", f1, others))
    }
    lines <- c(lines, "")
  }

  total_items <- sum(items_per_factor)
  for (j in seq_len(total_items))
    lines <- c(lines, sprintf("  x%d ~~ %.4f*x%d", j, residual_var, j))

  paste(lines, collapse = "\n")
}


# Helper: Build analysis model string (freely estimated)

build_analysis_model <- function(n_factors, items_per_factor) {
  lines    <- character(0)
  item_idx <- 1L

  for (f in seq_len(n_factors)) {
    items <- paste0("x", item_idx:(item_idx + items_per_factor[f] - 1L),
                    collapse = " + ")
    lines    <- c(lines, sprintf("  factor%d =~ %s", f, items))
    item_idx <- item_idx + items_per_factor[f]
  }

  if (n_factors > 1L) {
    lines <- c(lines, "")
    for (f1 in seq_len(n_factors - 1L))
      lines <- c(lines, sprintf("  factor%d ~~ %s", f1,
                                paste0("factor", (f1 + 1L):n_factors,
                                       collapse = " + ")))
  }

  paste(lines, collapse = "\n")
}

# Helper: Compute relative bias from simsem coef slot
#
# sim_obj@coef is a data.frame with one row per replication and one column
# per free parameter.  The replications were averaged then compared to the
# known population value.
#
# bias = (mean_estimated - population) / |population|   (relative)
#
# Returns a list:
#   $max_loading_bias   — worst absolute relative bias across loading params
#   $max_cor_bias       — worst absolute relative bias across factor correlations
#   $bias_ok_loading    — TRUE if max loading bias < .05
#   $bias_ok_cor        — TRUE if max cor    bias < .05

compute_bias <- function(sim_obj, loading_value, factor_cor,
                         n_factors, flags) {

  # Default: no bias computed (flags not triggered)
  out <- list(
    max_loading_bias = NA_real_,
    max_cor_bias     = NA_real_,
    bias_ok_loading  = TRUE,
    bias_ok_cor      = TRUE
  )

  coef_mat <- tryCatch(sim_obj@coef, error = function(e) NULL)
  if (is.null(coef_mat) || nrow(coef_mat) == 0) return(out)

  # Column names encode parameter labels — e.g. "factor1=~x1", "factor1~~factor2"
  col_nms <- colnames(coef_mat)

  # Loading bias (Flag 2: weak loadings) 
  if (flags["weak_loading"]) {
    loading_cols <- grep("=~", col_nms, value = TRUE)
    if (length(loading_cols) > 0) {
      mean_loadings <- colMeans(coef_mat[, loading_cols, drop = FALSE],
                                na.rm = TRUE)
      rel_bias      <- abs((mean_loadings - loading_value) / loading_value)
      out$max_loading_bias <- round(max(rel_bias, na.rm = TRUE), 4)
      out$bias_ok_loading  <- out$max_loading_bias < 0.05
    }
  }

  # Factor correlation bias (Flag 1: boundary correlations) 
  if (flags["cor_boundary"] && n_factors > 1L) {
    # Factor correlation columns use "~~" but NOT "x" (residuals use "~~" too)
    # Pattern: "factorN~~factorM"
    cor_cols <- grep("factor[0-9]+~~factor[0-9]+", col_nms, value = TRUE)
    if (length(cor_cols) > 0) {
      mean_cors <- colMeans(coef_mat[, cor_cols, drop = FALSE], na.rm = TRUE)
      # Guard against near-zero true correlations (division instability)
      if (abs(factor_cor) > 0.01) {
        rel_bias         <- abs((mean_cors - factor_cor) / factor_cor)
        out$max_cor_bias <- round(max(rel_bias, na.rm = TRUE), 4)
        out$bias_ok_cor  <- out$max_cor_bias < 0.05
      }
    }
  }

  out
}


# Helper: Extract simulation summary for one N
#
# bias_flags — named logical vector from get_bias_flags()
# loading_value, factor_cor, n_factors — needed for bias reference values

extract_summary <- function(sim_obj, n,
                            bias_flags    = c(cor_boundary  = FALSE,
                                              weak_loading  = FALSE,
                                              id_boundary   = FALSE),
                            loading_value = 0.5,
                            factor_cor    = 0.3,
                            n_factors     = 1L) {

  # Convergence 
  conv_rate <- mean(sim_obj@converged == 0, na.rm = TRUE)

  # Power 
  pwr          <- getPower(sim_obj, alpha = .05)
  loading_rows <- grepl("=~", names(pwr))
  min_power    <- if (any(loading_rows)) min(as.numeric(pwr)[loading_rows],
                                             na.rm = TRUE) else NA_real_

  # Fit indices 
  fit_df <- as.data.frame(summaryFit(sim_obj))

  get_fit_index <- function(df, nm) {
    rm <- grep(nm, rownames(df), ignore.case = TRUE)
    if (length(rm)) {
      cm <- grep("mean", colnames(df), ignore.case = TRUE)
      if (length(cm)) return(as.numeric(df[rm[1], cm[1]]))
    }
    cm <- grep(nm, colnames(df), ignore.case = TRUE)
    if (length(cm)) {
      rm2 <- grep("mean", rownames(df), ignore.case = TRUE)
      if (length(rm2)) return(as.numeric(df[rm2[1], cm[1]]))
      v <- suppressWarnings(as.numeric(df[, cm[1]]))
      v <- v[!is.na(v)]
      if (length(v)) return(v[1])
    }
    NA_real_
  }

  mean_rmsea <- get_fit_index(fit_df, "rmsea")
  mean_cfi   <- get_fit_index(fit_df, "cfi")

  # Bias computation (only when flags triggered) 
  bias <- compute_bias(sim_obj, loading_value, factor_cor,
                       n_factors, bias_flags)

  # Adequacy judgment 
  # Core criteria (always applied)
  core_ok <- !is.na(min_power) && !is.na(mean_rmsea) && !is.na(mean_cfi) &&
    min_power  >= 0.80 &&
    mean_rmsea <  0.05 &&
    mean_cfi   >  0.95

  # Flag 3: identification boundary — require conv_rate >= .98
  conv_ok <- if (bias_flags["id_boundary"]) {
    !is.na(conv_rate) && conv_rate >= 0.98
  } else {
    TRUE
  }

  # Flag 2: weak loading bias gate
  loading_bias_ok <- if (bias_flags["weak_loading"]) bias$bias_ok_loading else TRUE

  # Flag 1: factor correlation bias gate
  cor_bias_ok <- if (bias_flags["cor_boundary"]) bias$bias_ok_cor else TRUE

  adequate <- core_ok && conv_ok && loading_bias_ok && cor_bias_ok

  # Build failure reason string (for display) 
  reasons <- character(0)
  if (!is.na(min_power)  && min_power  < 0.80) reasons <- c(reasons, "Power")
  if (!is.na(mean_rmsea) && mean_rmsea >= 0.05) reasons <- c(reasons, "RMSEA")
  if (!is.na(mean_cfi)   && mean_cfi   <= 0.95) reasons <- c(reasons, "CFI")
  if (bias_flags["id_boundary"] && !is.na(conv_rate) && conv_rate < 0.98)
    reasons <- c(reasons, "Conv<98%")
  if (bias_flags["weak_loading"] && !isTRUE(loading_bias_ok))
    reasons <- c(reasons, sprintf("LoadBias=%.1f%%",
                                  bias$max_loading_bias * 100))
  if (bias_flags["cor_boundary"] && !isTRUE(cor_bias_ok))
    reasons <- c(reasons, sprintf("CorBias=%.1f%%",
                                  bias$max_cor_bias * 100))

  status <- if (adequate) "Yes" else if (length(reasons) > 0)
    paste0("No (", paste(reasons, collapse = ", "), ")") else "No"

  data.frame(
    N              = n,
    Min_Power      = round(min_power,              3),
    Mean_RMSEA     = round(as.numeric(mean_rmsea), 3),
    Mean_CFI       = round(as.numeric(mean_cfi),   3),
    Conv_Rate      = round(conv_rate,              3),
    Loading_Bias   = if (!is.na(bias$max_loading_bias))
                       paste0(round(bias$max_loading_bias * 100, 1), "%")
                     else "—",
    Cor_Bias       = if (!is.na(bias$max_cor_bias))
                       paste0(round(bias$max_cor_bias * 100, 1), "%")
                     else "—",
    Adequate       = status,
    stringsAsFactors = FALSE
  )
}

# Worker: runs ONE candidate N in a parallel process

run_one_n <- function(n, pop_model, ana_model, nRep, seed_val,
                      bias_flags, loading_value, factor_cor, n_factors) {

  set.seed(seed_val + n)

  tryCatch({
    sim_obj <- simsem::sim(
      nRep      = nRep,
      model     = ana_model,
      n         = n,
      generate  = pop_model,
      lavaanfun = "cfa",
      std.lv    = TRUE,
      silent    = TRUE
    )
    extract_summary(sim_obj, n,
                    bias_flags    = bias_flags,
                    loading_value = loading_value,
                    factor_cor    = factor_cor,
                    n_factors     = n_factors)
  }, error = function(e) {
    data.frame(
      N            = n,
      Min_Power    = NA_real_,
      Mean_RMSEA   = NA_real_,
      Mean_CFI     = NA_real_,
      Conv_Rate    = NA_real_,
      Loading_Bias = "—",
      Cor_Bias     = "—",
      Adequate     = "Error",
      stringsAsFactors = FALSE
    )
  })
}
