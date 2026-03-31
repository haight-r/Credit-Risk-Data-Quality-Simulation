# ============================================================================
# Feature Reliability Score (FRS) -- per-feature data quality diagnostic
# ============================================================================
#
# Reference: Igl & Gruber (2025), "Handbuch Datenqualitaet", Chapter 8.
#
# The FRS is computed on the impaired dataset AFTER apply_impairment() but
# BEFORE any rescaling or imputation. This is where a practitioner would
# use it: as a pre-modelling diagnostic that characterises how damaged each
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
# crefo_score. Sector is excluded because it is categorical (outlier and
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
                                 "crefo_score"),
                        plausibility_bounds = NULL,
                        outlier_method      = "iqr",
                        outlier_k           = 3,
                        consistency_rules   = NULL) {
  
  # ---- 0. Defaults --------------------------------------------------------
  
  # Plausibility bounds: domain-valid ranges for each predictor.
  # These reflect credit-risk domain knowledge -- a practitioner would
  # define these from business rules, not from the data itself (Sec. 8.6.3).
  if (is.null(plausibility_bounds)) {
    plausibility_bounds <- list(
      log_assets     = c(0, 20),       # ln(assets): 0 ~ $1, 20 ~ $500M
      debt_to_equity = c(0, Inf),      # leverage ratio cannot be negative
      interest_cov   = c(0, Inf),      # coverage ratio cannot be negative
      firm_age       = c(0, Inf),      # age cannot be negative
      crefo_score    = c(100, 600)     # Creditreform Bonitaetsindex range
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
      # "Poor Crefo" = crefo_score above the 90th percentile (i.e.
      # worst 10%).
      # The reverse is also flagged: weak financials (all three
      # indicators in the bad tail) paired with an excellent Crefo
      # score (below 10th percentile).
      crefo_vs_fin = function(d) {
        obs_mask <- !is.na(d$crefo_score) & !is.na(d$log_assets) &
          !is.na(d$interest_cov) & !is.na(d$debt_to_equity)
        result   <- rep(TRUE, nrow(d))
        if (sum(obs_mask) < 10) return(result)
        
        la_med  <- median(d$log_assets[obs_mask])
        ic_med  <- median(d$interest_cov[obs_mask])
        dte_med <- median(d$debt_to_equity[obs_mask])
        cr_q10  <- quantile(d$crefo_score[obs_mask], 0.10)
        cr_q90  <- quantile(d$crefo_score[obs_mask], 0.90)
        
        # Strong financials + poor Crefo
        strong_fin <- obs_mask &
          d$log_assets     > la_med  &
          d$interest_cov   > ic_med  &
          d$debt_to_equity < dte_med
        poor_crefo <- obs_mask & d$crefo_score > cr_q90
        
        # Weak financials + excellent Crefo
        weak_fin <- obs_mask &
          d$log_assets     < la_med  &
          d$interest_cov   < ic_med  &
          d$debt_to_equity > dte_med
        good_crefo <- obs_mask & d$crefo_score < cr_q10
        
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
    if (n_obs > 0) {
      if (outlier_method == "iqr") {
        q1  <- quantile(x_obs, 0.25)
        q3  <- quantile(x_obs, 0.75)
        iqr <- q3 - q1
        n_outlier <- sum(x_obs < (q1 - outlier_k * iqr) |
                           x_obs > (q3 + outlier_k * iqr))
      } else if (outlier_method == "zscore") {
        z_vals    <- (x_obs - mean(x_obs)) / sd(x_obs)
        n_outlier <- sum(abs(z_vals) > outlier_k)
      } else {
        stop("outlier_method must be 'iqr' or 'zscore'")
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
                         crefo_score    = "crefo",
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

compute_frs_all <- function(dat_clean, impaired_list) {
  
  frs_clean       <- compute_frs(dat_clean)
  frs_clean$var   <- rownames(frs_clean)
  frs_clean$condition <- "clean"
  frs_clean$type     <- "none"
  frs_clean$severity <- "none"
  
  all_frs <- list(frs_clean)
  
  for (nm in names(impaired_list)) {
    d   <- impaired_list[[nm]]
    
    d_for_frs <- attr(d, "pre_imputation")
    if (is.null(d_for_frs)) {
      d_for_frs <- d
      message("Note: no pre_imputation snapshot for ", nm,
              " - computing FRS on post-imputation data.")
    }
    
    frs <- compute_frs(d_for_frs)
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
# Pretty-print helper
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