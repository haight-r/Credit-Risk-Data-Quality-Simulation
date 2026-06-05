# ============================================================================
# 02 Helper Functions
# ============================================================================


# Function 1:
# ============================================================================
# Domain check + Winsorizing (RR practitioner approach)
# ============================================================================
# These run BEFORE imputation inside prepare_for_modeling().
#
# Step 1 — domain_to_na(): values that are fundamentally impossible
#   (wrong sign, outside hard bounds) are treated as missing. A negative
#   firm age or negative interest coverage ratio is not an outlier —
#   it is nonsense, and should be handled the same way as a missing value.
#
# Step 2 — winsorize_vars(): remaining extreme values are capped at
#   empirical percentile cutoffs computed from the data as received.
#   This is the practitioner approach: a real-world analyst computes
#   percentiles on whatever dataset lands on their desk, without access
#   to the "clean" distribution. Two-sided variables get clipped at
#   the 2.5th and 97.5th percentiles; one-sided variables bounded at
#   zero get clipped only at the 95th percentile on the upper end
#   (the lower bound is already naturally at zero). Crefo is bounded
#   by its index definition [100, 600] so domain_to_na() handles it.
#
# NOTE: clean-data winsor_bounds are still stored in result$winsor_bounds
# and used by the FRS diagnostic (compute_frs), where the purpose is to
# measure data quality against a known reference. The distinction is:
#   - FRS (diagnostic):   "how far is this data from the clean standard?"
#   - Winsorizing (prep): "what would a practitioner do with this data?"
#
# This reflects Risk Research's standard data preparation workflow:
# identify → nullify → cap → impute.
# ============================================================================


domain_to_na <- function(dat) {
  # Replace domain-impossible values with NA.
  # These are values where the sign or magnitude makes no physical/business
  
  # sense — they cannot be real observations, so treating them as missing
  # is more appropriate than capping them.
  
  n_flagged <- 0
  
  # log_assets: must be >= 0 (ln of $1 = 0; negative is impossible)
  bad <- !is.na(dat$log_assets) & dat$log_assets < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible log_assets (< 0) → NA\n", sum(bad)))
    dat$log_assets[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }
  
  # debt_to_equity: must be >= 0 (negative leverage is nonsensical)
  bad <- !is.na(dat$debt_to_equity) & dat$debt_to_equity < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible debt_to_equity (< 0) → NA\n", sum(bad)))
    dat$debt_to_equity[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }
  
  # interest_cov: must be >= 0 (negative coverage is nonsensical)
  bad <- !is.na(dat$interest_cov) & dat$interest_cov < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible interest_cov (< 0) → NA\n", sum(bad)))
    dat$interest_cov[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }
  
  # firm_age: must be > 0 (negative age is a date arithmetic bug)
  bad <- !is.na(dat$firm_age) & dat$firm_age <= 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible firm_age (<= 0) → NA\n", sum(bad)))
    dat$firm_age[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }
  
  # crefo: must be in [100, 600] (Creditreform Bonitätsindex range)
  bad <- !is.na(dat$crefo) & (dat$crefo < 100 | dat$crefo > 600)
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible crefo (outside 100–600) → NA\n", sum(bad)))
    dat$crefo[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }
  
  if (n_flagged == 0) cat("  Domain check: no impossible values found.\n")
  
  dat
}


winsorize_vars <- function(dat) {
  # Cap extreme-but-plausible values at empirical percentile cutoffs
  # computed from the data as received — i.e., the practitioner approach.
  # A real-world analyst does not have access to the clean distribution;
  # they compute percentiles on whatever dataset lands on their desk.
  # This mirrors the Risk Research standard workflow.
  #
  # Two-sided variables: clipped at the 2.5th and 97.5th percentiles.
  # One-sided variables (bounded at zero): clipped at the 95th percentile
  # on the upper end only (the lower bound is already naturally at zero).
  # Crefo is not winsorized here — it has hard domain bounds [100, 600]
  # enforced by domain_to_na().
  
  # --- log_assets (two-sided) ----------------------------------------------
  if (any(!is.na(dat$log_assets))) {
    b <- quantile(dat$log_assets, c(0.025, 0.975), na.rm = TRUE)
    n_lo <- sum(dat$log_assets < b[1], na.rm = TRUE)
    n_hi <- sum(dat$log_assets > b[2], na.rm = TRUE)
    dat$log_assets <- pmin(pmax(dat$log_assets, b[1]), b[2])
    if (n_lo + n_hi > 0)
      cat(sprintf("  Winsorize log_assets: %d low (< %.2f), %d high (> %.2f)\n",
                  n_lo, b[1], n_hi, b[2]))
  }
  
  # --- debt_to_equity (one-sided upper) ------------------------------------
  if (any(!is.na(dat$debt_to_equity))) {
    b_upper <- quantile(dat$debt_to_equity, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$debt_to_equity > b_upper, na.rm = TRUE)
    dat$debt_to_equity <- pmin(dat$debt_to_equity, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize debt_to_equity: %d high (> %.2f)\n", n_hi, b_upper))
  }
  
  # --- interest_cov (one-sided upper) --------------------------------------
  if (any(!is.na(dat$interest_cov))) {
    b_upper <- quantile(dat$interest_cov, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$interest_cov > b_upper, na.rm = TRUE)
    dat$interest_cov <- pmin(dat$interest_cov, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize interest_cov: %d high (> %.2f)\n", n_hi, b_upper))
  }
  
  # --- firm_age (one-sided upper) ------------------------------------------
  if (any(!is.na(dat$firm_age))) {
    b_upper <- quantile(dat$firm_age, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$firm_age > b_upper, na.rm = TRUE)
    dat$firm_age <- pmin(dat$firm_age, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize firm_age: %d high (> %.2f)\n", n_hi, b_upper))
  }
  
  
  dat
}


# Function 2:
# ============================================================================
# Feature Reliability Score (FRS) -- per-feature data quality diagnostic
# ============================================================================
#
# Reference: Igl & Gruber (2025), "Handbuch Datenqualitaet", Chapter 8.
#
# The FRS is computed on the impaired dataset AFTER apply_impairment() but
# BEFORE any rescaling or imputation. This is where a practitioner would
# use it: as a pre-modelling diagnostic that characterizes how damaged each
# feature is, without knowledge of the true DGP.
#
# For the clean baseline the FRS should return values at or very close to 1
# for all predictors. Any deviation from 1 in the clean dataset is a useful
# sanity check -- it tells you whether your DGP produces features that are
# already plausible and well-distributed.
#
# Components implemented (following Ch. 8, Sec. 8.2-8.8):
#
#   1. Fuellgrad     (Completeness)   -- proportion non-missing     [Sec. 8.2]
#   2. Plausibilitaet (Validity)      -- proportion within domain   [Sec. 8.6]
#   3. Outlier Score                  -- 1 - (prop. flagged)        [Sec. 8.8]
#   4. Cross-field Consistency        -- sign of known relationship [Sec. 8.7]
#
# Components deliberately omitted (see Sec. 8.3-8.5):
#   - Diversitaet     -- uninformative for continuous ratios
#   - Klumpenbildung  -- designed for real-world heaping artefacts
#   - Verteilung      -- entropy requires discretization; adds noise
#
# Variables included: log_assets, debt_to_equity, interest_cov, firm_age,
# crefo. Sector is excluded because it is categorical (outlier and
# IQR checks do not apply), always fully observed, and always valid by
# construction -- its FRS would be a constant 1.0 across all conditions,
# adding no diagnostic information.
#
# Aggregation method: geometric mean of the K included components.
# The geometric mean is recommended in Sec. 8.9 as "besonders sensitiv auf
# niedrige Einzelwerte" -- exactly the behavior we want, since a single
# badly damaged dimension should pull the overall FRS down hard.
# ============================================================================


compute_frs <- function(dat,
                        vars = c("log_assets", "debt_to_equity",
                                 "interest_cov", "firm_age",
                                 "crefo"),
                        plausibility_bounds = NULL,
                        outlier_method      = "percentile",
                        outlier_k           = 3,
                        winsor_bounds       = NULL,
                        consistency_rules   = NULL) {
  
  # ---- 0. Defaults --------------------------------------------------------
  
  # Plausibility bounds (Sec. 8.6): "can this value exist?"
  # Wider than outlier bounds — 1st/99th percentiles from clean data.
  # Falls back to basic domain rules if not provided.
  if (is.null(plausibility_bounds)) {
    plausibility_bounds <- list(
      log_assets     = c(0, 20),
      debt_to_equity = c(0, Inf),
      interest_cov   = c(0, Inf),
      firm_age       = c(0, Inf),
      crefo          = c(100, 600)
    )
  }
  
  # Cross-field consistency rules: known directional relationships.
  # Each rule is a function that takes the full data frame and returns
  # a logical vector (TRUE = consistent, FALSE = violated).
  if (is.null(consistency_rules)) {
    consistency_rules <- list(
      
      # Higher debt_to_equity should generally correspond to lower
      # interest_cov (more leveraged firms have less coverage).
      # Soft rule: both above 90th percentile is flagged as inconsistent.
      dte_vs_ic = function(d) {
        both_obs <- !is.na(d$debt_to_equity) & !is.na(d$interest_cov)
        result   <- rep(TRUE, nrow(d))
        if (sum(both_obs) < 10) return(result)
        
        dte_q90 <- quantile(d$debt_to_equity[both_obs], 0.90, na.rm = TRUE)
        ic_q90  <- quantile(d$interest_cov[both_obs],   0.90, na.rm = TRUE)
        
        result[both_obs] <- !(d$debt_to_equity[both_obs] > dte_q90 &
                                d$interest_cov[both_obs]  > ic_q90)
        result
      },
      
      # Larger firms (higher log_assets) should generally have lower
      # debt_to_equity. Same soft check: both in the extreme tail is
      # inconsistent.
      la_vs_dte = function(d) {
        both_obs <- !is.na(d$log_assets) & !is.na(d$debt_to_equity)
        result   <- rep(TRUE, nrow(d))
        if (sum(both_obs) < 10) return(result)
        
        la_q90  <- quantile(d$log_assets[both_obs],     0.90, na.rm = TRUE)
        dte_q90 <- quantile(d$debt_to_equity[both_obs], 0.90, na.rm = TRUE)
        
        result[both_obs] <- !(d$log_assets[both_obs]     > la_q90 &
                                d$debt_to_equity[both_obs] > dte_q90)
        result
      },
      
      # Crefo score should be broadly consistent with financial health.
      # The Crefo index runs 100 (best) to 600 (worst), so a firm with
      # strong financials (high log_assets, high interest_cov, low
      # debt_to_equity) but a very poor Crefo score is inconsistent,
      # and vice versa.
      #
      # "Strong financials" = log_assets above median AND interest_cov
      # above median AND debt_to_equity below median.
      # "Poor Crefo" = crefo above the 90th percentile (i.e.
      # worst 10%).
      # The reverse is also flagged: weak financials (all three
      # indicators in the bad tail) paired with an excellent Crefo
      # score (below 10th percentile).
      crefo_vs_fin = function(d) {
        obs_mask <- !is.na(d$crefo) & !is.na(d$log_assets) &
          !is.na(d$interest_cov) & !is.na(d$debt_to_equity)
        result   <- rep(TRUE, nrow(d))
        if (sum(obs_mask) < 10) return(result)
        
        la_med  <- median(d$log_assets[obs_mask])
        ic_med  <- median(d$interest_cov[obs_mask])
        dte_med <- median(d$debt_to_equity[obs_mask])
        cr_q10  <- quantile(d$crefo[obs_mask], 0.10)
        cr_q90  <- quantile(d$crefo[obs_mask], 0.90)
        
        # Strong financials + poor Crefo
        strong_fin <- obs_mask &
          d$log_assets     > la_med  &
          d$interest_cov   > ic_med  &
          d$debt_to_equity < dte_med
        poor_crefo <- obs_mask & d$crefo > cr_q90
        
        # Weak financials + excellent Crefo
        weak_fin <- obs_mask &
          d$log_assets     < la_med  &
          d$interest_cov   < ic_med  &
          d$debt_to_equity > dte_med
        good_crefo <- obs_mask & d$crefo < cr_q10
        
        result[strong_fin & poor_crefo] <- FALSE
        result[weak_fin   & good_crefo] <- FALSE
        result
      }
    )
  }
  
  # ---- 1. Component scores per variable -----------------------------------
  
  n_total <- nrow(dat)
  results <- list()
  
  for (v in vars) {
    x <- dat[[v]]
    
    # --- 1a. Completeness (Sec. 8.2.1) ---
    n_miss    <- sum(is.na(x))
    s_compl   <- 1 - n_miss / n_total
    
    # --- 1b. Plausibility (Sec. 8.6.3) ---
    # Evaluated on non-missing values only
    x_obs     <- x[!is.na(x)]
    n_obs     <- length(x_obs)
    
    if (n_obs > 0 && v %in% names(plausibility_bounds)) {
      bounds    <- plausibility_bounds[[v]]
      n_invalid <- sum(x_obs < bounds[1] | x_obs > bounds[2])
      s_valid   <- 1 - n_invalid / n_obs
    } else {
      s_valid   <- 1
    }
    
    # --- 1c. Outlier Score (Sec. 8.8.1) ---
    # Igl & Gruber (Ch. 9) present multiple methods; Ch. 9.2 notes that
    # IQR assumptions break down for skewed distributions — relevant here
    # since debt_to_equity, interest_cov, and firm_age are zero-bounded
    # and right-skewed. The percentile method uses portfolio-calibrated
    # bounds (same as winsorizing cutoffs), consistent with RR's
    # tail-based approach. Z-score retained as an alternative.
    if (n_obs > 0) {
      if (outlier_method == "percentile") {
        if (is.null(winsor_bounds) || !(v %in% names(winsor_bounds))) {
          # Variable not in winsor_bounds (e.g. crefo with hard domain bounds).
          # Fall back to plausibility bounds as outlier threshold.
          if (v %in% names(plausibility_bounds)) {
            b_lo <- plausibility_bounds[[v]][1]
            b_hi <- plausibility_bounds[[v]][2]
            n_outlier <- sum(x_obs < b_lo | x_obs > b_hi)
          } else {
            n_outlier <- 0
          }
        } else {
          b <- winsor_bounds[[v]]
          n_outlier <- sum(x_obs < b["lower"] | x_obs > b["upper"])
        }
      } else if (outlier_method == "zscore") {
        z_vals    <- (x_obs - mean(x_obs)) / sd(x_obs)
        n_outlier <- sum(abs(z_vals) > outlier_k)
        
      } else {
        stop("outlier_method must be 'percentile' or 'zscore'")
      }
      s_outlier <- 1 - n_outlier / n_obs
    } else {
      s_outlier <- 1
    }
    
    # --- 1d. Cross-field Consistency (Sec. 8.7.3) ---
    # Match rules to variables via abbreviation lookup in rule names.
    relevant_rules <- list()
    for (rule_name in names(consistency_rules)) {
      v_abbrev <- switch(v,
                         log_assets     = "la",
                         debt_to_equity = "dte",
                         interest_cov   = "ic",
                         firm_age       = "fa",
                         crefo    = "crefo",
                         v
      )
      if (grepl(v_abbrev, rule_name, fixed = TRUE)) {
        relevant_rules[[rule_name]] <- consistency_rules[[rule_name]]
      }
    }
    
    if (length(relevant_rules) > 0) {
      consistent <- rep(TRUE, n_total)
      for (rule_fn in relevant_rules) {
        consistent <- consistent & rule_fn(dat)
      }
      s_consis <- mean(consistent)
    } else {
      s_consis <- 1
    }
    
    # ---- 2. Aggregate into FRS (Sec. 8.9) --------------------------------
    # Geometric mean: FRS = (S1 * S2 * ... * Sk)^(1/k)
    components <- c(completeness  = s_compl,
                    plausibility  = s_valid,
                    outlier       = s_outlier,
                    consistency   = s_consis)
    
    components_floored <- pmax(components, 1e-6)
    frs <- prod(components_floored)^(1 / length(components_floored))
    
    results[[v]] <- c(components, frs = frs)
  }
  
  # ---- 3. Assemble output ------------------------------------------------
  
  frs_matrix <- do.call(rbind, results)
  frs_df     <- as.data.frame(frs_matrix)
  
  frs_df["MEAN", ] <- colMeans(frs_df)
  
  attr(frs_df, "frs_meta") <- list(
    n_obs           = n_total,
    vars            = vars,
    components      = c("completeness", "plausibility", "outlier", "consistency"),
    aggregation     = "geometric_mean",
    outlier_method  = outlier_method,
    outlier_k       = outlier_k,
    plausibility_bounds = plausibility_bounds
  )
  
  frs_df
}


# ============================================================================
# Convenience wrapper: compute FRS for clean + all impaired datasets at once
# ============================================================================

compute_frs_all <- function(dat_clean, impaired_list, winsor_bounds, 
                            plausibility_bounds) {
  
  frs_clean <- compute_frs(dat_clean, winsor_bounds = winsor_bounds,
                           plausibility_bounds = plausibility_bounds)
  frs_clean$var   <- rownames(frs_clean)
  frs_clean$condition <- "clean"
  frs_clean$type     <- "none"
  frs_clean$severity <- "none"
  
  all_frs <- list(frs_clean)
  
  for (nm in names(impaired_list)) {
    d   <- impaired_list[[nm]]
    
    frs <- compute_frs(d, winsor_bounds = winsor_bounds,
                       plausibility_bounds = plausibility_bounds)
    frs$var       <- rownames(frs)
    frs$condition <- nm
    
    meta <- attr(d, "impairment")
    frs$type     <- if (!is.null(meta$type))     meta$type     else nm
    frs$severity <- if (!is.null(meta$severity)) meta$severity else "unknown"
    
    all_frs <- c(all_frs, list(frs))
  }
  
  out <- do.call(rbind, all_frs)
  rownames(out) <- NULL
  out
}


# ============================================================================
# Summarize FRS across conditions
# ============================================================================
#
# Takes the stacked output from compute_frs_all() and produces two summary
# data frames:
#
#   1. by_condition: mean FRS components per impairment type x severity,
#      collapsed across variables. Answers: "How bad is MCAR-severe overall?"
#
#   2. by_variable: mean FRS components per variable x impairment type,
#      collapsed across severity. Answers: "Which variables get hit hardest
#      by each mechanism?"
#
# Both are returned in a list for easy ggplot use.
# ============================================================================

summarize_frs <- function(frs_all) {
  
  # Remove the MEAN rows added by compute_frs() -- these are per-dataset
  # averages that would double-count if we aggregate again here.
  frs <- frs_all[frs_all$var != "MEAN", ]
  
  # ---- 1. By condition: type x severity (collapsed across variables) ------
  
  by_condition <- aggregate(
    cbind(completeness, plausibility, outlier, consistency, frs) ~
      type + severity,
    data = frs,
    FUN  = mean
  )
  
  # Sort for readability: clean first, then alphabetical
  by_condition <- by_condition[order(
    by_condition$type != "none",   # clean (type="none") first
    by_condition$type,
    by_condition$severity
  ), ]
  rownames(by_condition) <- NULL
  
  # ---- 2. By variable: variable x type (collapsed across severity) --------
  
  by_variable <- aggregate(
    cbind(completeness, plausibility, outlier, consistency, frs) ~
      var + type,
    data = frs,
    FUN  = mean
  )
  
  by_variable <- by_variable[order(
    by_variable$type != "none",
    by_variable$type,
    by_variable$var
  ), ]
  rownames(by_variable) <- NULL
  
  # ---- 3. Return both ----------------------------------------------------
  
  list(
    by_condition = by_condition,
    by_variable  = by_variable
  )
}


# ============================================================================
# Post-calculation receipt
# ============================================================================

print_frs <- function(frs_df, digits = 3) {
  cat("\n=== Feature Reliability Score ===\n")
  cat("Aggregation: geometric mean of",
      paste(attr(frs_df, "frs_meta")$components, collapse = ", "), "\n")
  cat("Outlier detection:",
      attr(frs_df, "frs_meta")$outlier_method,
      "with k =", attr(frs_df, "frs_meta")$outlier_k, "\n")
  cat("N =", attr(frs_df, "frs_meta")$n_obs, "\n\n")
  print(round(frs_df[, c("completeness", "plausibility", "outlier",
                         "consistency", "frs")], digits))
  cat("\n")
}


# ============================================================================
# Function 3: Compute FRS - side quest 1
# ============================================================================
# ============================================================================
# Compare FRS across imputation methods for missingness mechanisms
# ============================================================================
#
# Builds a stacked FRS data frame with three stages per condition:
#   - impaired: after missingness is applied, before any imputation
#   - regression: after regression imputation
#   - median:   after median imputation
#   - mice:     after MICE imputation
#
# Restricted to MCAR / MAR / MNAR since these are the mechanisms where
# imputation is relevant (noise and implausible values aren't missing data).
# ============================================================================

compare_frs_imputation <- function(dat_clean, impaired_list,
                                   regr_list, median_list, mice_list,
                                   winsor_bounds, plausibility_bounds) {
  
  frs_clean <- compute_frs(dat_clean, winsor_bounds = winsor_bounds,
                           plausibility_bounds = plausibility_bounds)
  frs_clean$var       <- rownames(frs_clean)
  frs_clean$condition <- "clean"
  frs_clean$type      <- "none"
  frs_clean$severity  <- "none"
  frs_clean$stage     <- "clean"
  
  all_frs <- list(frs_clean)
  
  add_stage <- function(dat_list, stage_label) {
    for (nm in names(dat_list)) {
      d   <- dat_list[[nm]]
      frs <- compute_frs(d, winsor_bounds = winsor_bounds,
                         plausibility_bounds = plausibility_bounds)
      frs$var       <- rownames(frs)
      frs$condition <- nm
      frs$stage     <- stage_label
      
      meta <- attr(d, "impairment")
      frs$type     <- if (!is.null(meta$type))     meta$type     else nm
      frs$severity <- if (!is.null(meta$severity)) meta$severity else "unknown"
      
      all_frs[[length(all_frs) + 1]] <<- frs
    }
  }
  
  add_stage(impaired_list, "impaired")
  add_stage(regr_list, "regression")
  add_stage(median_list,   "median")
  add_stage(mice_list,     "mice")
  
  out <- do.call(rbind, all_frs)
  rownames(out) <- NULL
  
  out$stage <- factor(out$stage,
                      levels = c("clean", "impaired", "regression", "median", "mice"))
  out
}


# ============================================================================
# Function 4: Fit Logistic Models
# ============================================================================



# ── Fit logistic regression across all conditions ──────────────────────

fit_logistic_models <- function(clean_data,
                                impaired_list,
                                prepared_list,
                                formula,
                                true_betas) {
  
  # Build a master list: stage label + dataset, all in one sequence.
  # Clean appears once; impaired and prepared share the same condition
  # names but get different stage tags.
  master <- list()
  master[["clean"]] <- list(stage = "clean", data = clean_data)
  
  for (nm in names(impaired_list)) {
    master[[paste0("imp_", nm)]] <- list(stage = "impaired", data = impaired_list[[nm]])
  }
  for (nm in names(prepared_list)) {
    master[[paste0("prep_", nm)]] <- list(stage = "prepared", data = prepared_list[[nm]])
  }
  
  # Pre-allocate storage
  models       <- vector("list", length(master))
  coefficients <- vector("list", length(master))
  names(models) <- names(coefficients) <- names(master)
  
  for (key in names(master)) {
    
    d     <- master[[key]]$data
    stage <- master[[key]]$stage
    
    # Fit the model -- glm uses na.action = na.omit by default,
    # so raw impaired datasets with NAs get complete-case analysis
    fit <- glm(formula, data = d, family = binomial())
    
    models[[key]] <- fit
    
    # Coefficient recovery table
    est <- coef(fit)
    recovery <- data.frame(
      key       = key,
      stage     = stage,
      condition = ifelse(stage == "clean", "clean",
                         sub("^imp_|^prep_", "", key)),
      term      = names(true_betas),
      true      = as.numeric(true_betas),
      estimated = as.numeric(est),
      diff      = as.numeric(est) - as.numeric(true_betas),
      abs_diff  = abs(as.numeric(est) - as.numeric(true_betas)),
      n_obs     = nobs(fit),
      stringsAsFactors = FALSE
    )
    
    coefficients[[key]] <- recovery
    
    if (!fit$converged) {
      cat(sprintf("[WARNING] glm did not converge for: %s\n", key))
    }
  }
  
  # Stack into one long data.frame
  coef_recovery <- do.call(rbind, coefficients)
  rownames(coef_recovery) <- NULL
  
  # Parse condition into impairment type + severity
  coef_recovery$type <- ifelse(
    coef_recovery$condition == "clean", "clean",
    sub("_mild$|_severe$", "", coef_recovery$condition)
  )
  coef_recovery$severity <- ifelse(
    coef_recovery$condition == "clean", "none",
    ifelse(grepl("severe", coef_recovery$condition), "severe", "mild")
  )
  
  list(
    models        = models,          # named list of glm objects
    coef_recovery = coef_recovery    # long-format data.frame
  )
}


