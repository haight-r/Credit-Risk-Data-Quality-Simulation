# ============================================================================
# 03_monte_carlo.R
# ============================================================================
# Monte Carlo simulation: N iterations of the full pipeline.
# Each iteration = fresh DGP → impair → prepare → fit → evaluate.
#
# Dependencies: base R + xgboost
# Expected runtime: ~2-4 hours for 200 iterations on a modern laptop
#
# Usage:
#   Rscript 06_monte_carlo.R
#   — or source() interactively and monitor checkpoints
# ============================================================================

source("01_functions.R")
library(xgboost)

# ============================================================================
# CONFIG
# ============================================================================

N_MC    <- 5000L       # number of Monte Carlo iterations
N_FIRMS <- 5000L      # firms per iteration

# Where to save results
CHECKPOINT_DIR <- "mc_checkpoints"
FINAL_OUTPUT   <- "mc_results_final.rds"

dir.create(CHECKPOINT_DIR, showWarnings = FALSE)


# ============================================================================
# FAST AUC (rank-based, replaces the O(n1*n0) version in evaluate_models)
# ============================================================================
# The evaluate_models() function in 01_functions.R uses a vapply-based
# Mann-Whitney U that is correct but O(n1 * n0) — roughly 49M comparisons
# per call with our data dimensions. This rank-based version is O(n log n)
# and produces identical results. We inject it into evaluate_models by
# defining a wrapper that patches the AUC computation.

fast_auc <- function(pred, actual) {
  n1 <- sum(actual == 1)
  n0 <- sum(actual == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(pred)
  U <- sum(r[actual == 1]) - n1 * (n1 + 1) / 2
  U / (n1 * n0)
}


# ── Patched evaluate function using fast AUC ─────────────────────────
# Identical logic to evaluate_models() but with rank-based AUC.

evaluate_models_fast <- function(models, eval_data, formula,
                                 model_type = c("glm", "xgboost")) {

  model_type <- match.arg(model_type)
  response   <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]
  actual     <- eval_data[[response]]

  compute_ks <- function(pred, actual) {
    scores_1 <- pred[actual == 1]
    scores_0 <- pred[actual == 0]
    if (length(scores_1) == 0 || length(scores_0) == 0) return(NA_real_)
    cdf_1 <- ecdf(scores_1)
    cdf_0 <- ecdf(scores_0)
    all_scores <- sort(unique(pred))
    max(abs(cdf_1(all_scores) - cdf_0(all_scores)))
  }

  results <- vector("list", length(models))
  names(results) <- names(models)

  for (key in names(models)) {

    mod <- models[[key]]

    if (model_type == "glm") {
      pd_pred <- predict(mod, newdata = eval_data, type = "response")
    } else {
      X <- as.matrix(eval_data[, predictors, drop = FALSE])
      pd_pred <- predict(mod, xgb.DMatrix(data = X))
    }

    auc_val  <- fast_auc(pd_pred, actual)
    gini_val <- 2 * auc_val - 1
    ks_val   <- compute_ks(pd_pred, actual)

    brier_val <- mean((pd_pred - actual)^2)
    obs_rate  <- mean(actual)
    exp_rate  <- mean(pd_pred)
    oe_ratio  <- obs_rate / exp_rate

    stage     <- ifelse(key == "clean", "clean",
                        ifelse(grepl("^imp_", key), "impaired", "prepared"))
    condition <- ifelse(key == "clean", "clean",
                        sub("^imp_|^prep_", "", key))

    results[[key]] <- data.frame(
      key        = key,
      stage      = stage,
      condition  = condition,
      model_type = model_type,
      auc        = auc_val,
      gini       = gini_val,
      ks         = ks_val,
      brier      = brier_val,
      obs_rate   = obs_rate,
      pred_rate  = exp_rate,
      oe_ratio   = oe_ratio,
      stringsAsFactors = FALSE
    )
  }

  metrics <- do.call(rbind, results)
  rownames(metrics) <- NULL

  metrics$type <- ifelse(
    metrics$condition == "clean", "clean",
    sub("_mild$|_severe$", "", metrics$condition)
  )
  metrics$severity <- ifelse(
    metrics$condition == "clean", "none",
    ifelse(grepl("severe", metrics$condition), "severe", "mild")
  )

  metrics
}


# ============================================================================
# SINGLE-ITERATION WRAPPER
# ============================================================================

run_one_iteration <- function(seed) {

  pd_formula <- default ~ log_assets_z + debt_to_equity_z +
                          interest_cov_z + sector_2 + sector_3 +
                          firm_age_z + crefo_z

  # Stage 1: Generate clean data
  result <- simulate_clean_data_panel(N = N_FIRMS, seed = seed)
  dat    <- result$data

  # Stage 2: Impair (all 10 conditions)
  impaired_list <- list(
    mcar_mild    = impair_mcar(dat, "mild"),
    mcar_severe  = impair_mcar(dat, "severe"),
    mar_mild     = impair_mar(dat, "mild"),
    mar_severe   = impair_mar(dat, "severe"),
    mnar_mild    = impair_mnar(dat, "mild"),
    mnar_severe  = impair_mnar(dat, "severe"),
    noise_mild   = impair_noise(dat, result$scaling, "mild"),
    noise_severe = impair_noise(dat, result$scaling, "severe"),
    impl_mild    = impair_implausible(dat, result$scaling, "mild"),
    impl_severe  = impair_implausible(dat, result$scaling, "severe")
  )

  # Stage 3: Prepare (ersatzwert only for main thesis)
  prep_list <- lapply(impaired_list, function(d) {
    prepare_for_modeling(d, result$scaling,
                         method = "ersatzwert",
                         model_formula = pd_formula)
  })
  names(prep_list) <- names(impaired_list)

  # Stage 4a: Fit logistic regression
  glm_res <- fit_logistic_models(
    clean_data    = dat,
    impaired_list = impaired_list,
    prepared_list = prep_list,
    formula       = pd_formula,
    true_betas    = result$true_betas
  )

  # Stage 4b: Fit XGBoost
  xgb_res <- fit_xgboost_models(
    clean_data    = dat,
    impaired_list = impaired_list,
    prepared_list = prep_list,
    formula       = pd_formula,
    seed          = seed
  )

  # Stage 5: Evaluate (all models score on THIS iteration's clean data)
  glm_metrics <- evaluate_models_fast(glm_res$models, dat, pd_formula, "glm")
  xgb_metrics <- evaluate_models_fast(xgb_res$models, dat, pd_formula, "xgboost")

  # Stage 5b: Univariate discriminatory power (Trennschärfe)
  # Three snapshots: clean baseline, post-corruption, post-imputation
  uni_rows <- vector("list", 1 + 2 * length(impaired_list))
  idx <- 1L

  # Clean baseline
  u <- compute_univariate_auc(dat)
  u$condition <- "clean"
  u$type      <- "clean"
  u$severity  <- "none"
  u$stage     <- "clean"
  uni_rows[[idx]] <- u; idx <- idx + 1L

  for (nm in names(impaired_list)) {
    meta <- attr(impaired_list[[nm]], "impairment")
    imp_type <- if (!is.null(meta$type)) meta$type else nm
    imp_sev  <- if (!is.null(meta$severity)) meta$severity else "unknown"

    # Post-corruption / pre-imputation (complete cases only — NAs excluded by rank_auc)
    u_imp <- compute_univariate_auc(impaired_list[[nm]])
    u_imp$condition <- nm
    u_imp$type      <- imp_type
    u_imp$severity  <- imp_sev
    u_imp$stage     <- "impaired"
    uni_rows[[idx]] <- u_imp; idx <- idx + 1L

    # Post-imputation
    u_prep <- compute_univariate_auc(prep_list[[nm]])
    u_prep$condition <- nm
    u_prep$type      <- imp_type
    u_prep$severity  <- imp_sev
    u_prep$stage     <- "prepared"
    uni_rows[[idx]] <- u_prep; idx <- idx + 1L
  }

  univariate_auc <- do.call(rbind, uni_rows)
  rownames(univariate_auc) <- NULL

  # Return numeric summaries only — no model objects (memory)
  list(
    seed           = seed,
    n_obs          = nrow(dat),
    default_rate   = mean(dat$default),
    metrics        = rbind(glm_metrics, xgb_metrics),
    coef_recovery  = glm_res$coef_recovery,
    importance     = xgb_res$importance,
    univariate_auc = univariate_auc
  )
}


# ============================================================================
# RUN THE LOOP
# ============================================================================

set.seed(21)
mc_seeds <- sample(1e6, N_MC)

mc_results <- vector("list", N_MC)

cat(sprintf("Starting Monte Carlo: %d iterations, %d firms each\n", N_MC, N_FIRMS))
cat(sprintf("Checkpoints every 25 iterations → %s/\n", CHECKPOINT_DIR))
cat(sprintf("Final output → %s\n\n", FINAL_OUTPUT))

total_t0 <- Sys.time()

# ── Parallel setup (Windows) ──
library(parallel)
n_cores <- 80L
cl <- makeCluster(n_cores)

# Export everything the workers need
clusterExport(cl, c("run_one_iteration", "mc_seeds", "N_FIRMS"))

# Source dependencies on each worker
clusterEvalQ(cl, {
  source("01_functions.R")
  library(xgboost)
})

cat(sprintf("Running %d iterations across %d cores...\n", N_MC, n_cores))
total_t0 <- Sys.time()

mc_results <- parLapply(cl, seq_len(N_MC), function(i) {
  tryCatch({
    invisible(capture.output(
      res <- run_one_iteration(mc_seeds[i])
    ))
    res
  }, error = function(e) {
    list(seed = mc_seeds[i], n_obs = NA, default_rate = NA,
         metrics = NULL, coef_recovery = NULL, importance = NULL,
         univariate_auc = NULL)
  })
})

stopCluster(cl)

total_elapsed <- as.numeric(difftime(Sys.time(), total_t0, units = "mins"))
cat(sprintf("\nAll %d iterations complete in %.1f minutes.\n", N_MC, total_elapsed))
  elapsed <- as.numeric(difftime(Sys.time(), iter_t0, units = "secs"))

  # One-line progress: iteration, seed, time, default rate
  dr <- mc_results[[i]]$default_rate
  cat(sprintf("[%3d/%d] seed=%d | %.1fs | dr=%.3f\n",
              i, N_MC, mc_seeds[i], elapsed,
              ifelse(is.na(dr), 0, dr)))

  # Checkpoint every 25 iterations
  if (i %% 25 == 0) {
    cp_path <- file.path(CHECKPOINT_DIR, sprintf("mc_checkpoint_%03d.rds", i))
    saveRDS(mc_results[1:i], cp_path)
    total_elapsed <- as.numeric(difftime(Sys.time(), total_t0, units = "mins"))
    avg_per_iter  <- total_elapsed / i
    est_remaining <- avg_per_iter * (N_MC - i)
    cat(sprintf("  ── Checkpoint saved: %s (%.1f min elapsed, ~%.0f min remaining) ──\n",
                cp_path, total_elapsed, est_remaining))
  }
}

total_elapsed <- as.numeric(difftime(Sys.time(), total_t0, units = "mins"))
cat(sprintf("\nAll %d iterations complete in %.1f minutes.\n", N_MC, total_elapsed))


# ============================================================================
# STACK RESULTS
# ============================================================================

cat("Stacking results...\n")

# ── Metrics (Gini, Brier, O/E, KS per condition × stage × model) ──
all_metrics <- do.call(rbind, lapply(seq_along(mc_results), function(i) {
  m <- mc_results[[i]]$metrics
  if (!is.null(m)) {
    m$iteration    <- i
    m$seed         <- mc_results[[i]]$seed
    m$default_rate <- mc_results[[i]]$default_rate
  }
  m
}))
rownames(all_metrics) <- NULL

# ── Coefficient recovery (GLM betas vs. true betas) ──
all_coefs <- do.call(rbind, lapply(seq_along(mc_results), function(i) {
  cr <- mc_results[[i]]$coef_recovery
  if (!is.null(cr)) {
    cr$iteration <- i
    cr$seed      <- mc_results[[i]]$seed
  }
  cr
}))
rownames(all_coefs) <- NULL

# ── XGBoost feature importance ──
all_importance <- do.call(rbind, lapply(seq_along(mc_results), function(i) {
  imp <- mc_results[[i]]$importance
  if (!is.null(imp)) {
    imp$iteration <- i
    imp$seed      <- mc_results[[i]]$seed
  }
  imp
}))
rownames(all_importance) <- NULL

# ── Univariate discriminatory power (Trennschärfe) ──
all_univariate <- do.call(rbind, lapply(seq_along(mc_results), function(i) {
  ua <- mc_results[[i]]$univariate_auc
  if (!is.null(ua)) {
    ua$iteration <- i
    ua$seed      <- mc_results[[i]]$seed
  }
  ua
}))
rownames(all_univariate) <- NULL


# ============================================================================
# SAVE
# ============================================================================

saveRDS(list(
  metrics    = all_metrics,
  coefs      = all_coefs,
  importance = all_importance,
  univariate = all_univariate,
  seeds      = mc_seeds,
  config     = list(N_MC = N_MC, N_FIRMS = N_FIRMS,
                    timestamp = Sys.time())
), FINAL_OUTPUT)

cat(sprintf("Results saved to %s\n", FINAL_OUTPUT))

# Quick sanity check
n_ok <- sum(sapply(mc_results, function(r) !is.null(r$metrics)))
cat(sprintf("Successful iterations: %d / %d\n", n_ok, N_MC))

if (n_ok > 0) {
  cat("\nSample means across iterations (prepared GLM):\n")
  prep_glm <- all_metrics[all_metrics$stage == "prepared" &
                           all_metrics$model_type == "glm", ]
  cat(sprintf("  Gini:  %.4f (sd %.4f)\n",
              mean(prep_glm$gini), sd(prep_glm$gini)))
  cat(sprintf("  Brier: %.6f (sd %.6f)\n",
              mean(prep_glm$brier), sd(prep_glm$brier)))
  cat(sprintf("  O/E:   %.4f (sd %.4f)\n",
              mean(prep_glm$oe_ratio), sd(prep_glm$oe_ratio)))
}

cat("\nDone.\n")
