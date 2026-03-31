# global.R — Loaded once at app startup (shared by ui.R and server.R)


library(shiny)
library(simsem)
library(lavaan)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(future)          # parallel backend
library(promises)        # async promise chaining
library(furrr)           # parallel purrr — future_map

# Parallel backend: use all available cores minus one (keep UI responsive).
# Falls back gracefully to sequential if only 1 core is detected.

n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
plan(multisession, workers = n_workers)

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


# Helper: Extract simulation summary for one N

extract_summary <- function(sim_obj, n) {

  conv_rate    <- mean(sim_obj@converged == 0, na.rm = TRUE)

  pwr          <- getPower(sim_obj, alpha = .05)
  loading_rows <- grepl("=~", names(pwr))
  min_power    <- if (any(loading_rows)) min(as.numeric(pwr)[loading_rows],
                                             na.rm = TRUE) else NA_real_

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

  adequate <- !is.na(min_power) && !is.na(mean_rmsea) && !is.na(mean_cfi) &&
    min_power >= .80 && mean_rmsea < .05 && mean_cfi > .95

  data.frame(
    N          = n,
    Min_Power  = round(min_power,              3),
    Mean_RMSEA = round(as.numeric(mean_rmsea), 3),
    Mean_CFI   = round(as.numeric(mean_cfi),   3),
    Conv_Rate  = round(conv_rate,              3),
    Adequate   = ifelse(adequate, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}


# Worker: runs ONE candidate N entirely — designed to be sent to a parallel
# worker via furrr::future_map.  All arguments are plain R objects (no
# Shiny reactives) so they serialise cleanly across processes.

run_one_n <- function(n, pop_model, ana_model, nRep, seed_val) {

  # Each worker gets a *different* but deterministic seed so results are
  # reproducible yet not correlated across N values.
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
    extract_summary(sim_obj, n)
  }, error = function(e) {
    data.frame(N = n, Min_Power = NA_real_, Mean_RMSEA = NA_real_,
               Mean_CFI = NA_real_, Conv_Rate = NA_real_,
               Adequate = "Error", stringsAsFactors = FALSE)
  })
}
