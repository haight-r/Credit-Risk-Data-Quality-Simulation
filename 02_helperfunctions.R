

# ── Data Prep ──

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
#   empirical percentile cutoffs. Two-sided variables get clipped at
#   the 2.5th and 97.5th percentiles; one-sided variables bounded at
#   zero get clipped only at the 95th percentile on the upper end
#   (the lower bound is already naturally at zero). Crefo is bounded
#   by its index definition [100, 600] so winsorizing applies to both
#   tails within that range.
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


winsorize_vars <- function(dat, winsor_bounds) {
  # Cap extreme-but-plausible values using clean-data cutoffs.
  # Bounds are precomputed from the unimpaired portfolio (same discipline
  # as scaling parameters). One-sided variables have lower = 0 in the
  # bounds; two-sided variables have empirical lower bounds.
  
  # --- log_assets (two-sided) ----------------------------------------------
  b <- winsor_bounds$log_assets
  if (any(!is.na(dat$log_assets))) {
    n_lo <- sum(dat$log_assets < b["lower"], na.rm = TRUE)
    n_hi <- sum(dat$log_assets > b["upper"], na.rm = TRUE)
    dat$log_assets <- pmin(pmax(dat$log_assets, b["lower"]), b["upper"])
    if (n_lo + n_hi > 0)
      cat(sprintf("  Winsorize log_assets: %d low (< %.2f), %d high (> %.2f)\n",
                  n_lo, b["lower"], n_hi, b["upper"]))
  }
  
  # --- debt_to_equity (one-sided upper) ------------------------------------
  b <- winsor_bounds$debt_to_equity
  if (any(!is.na(dat$debt_to_equity))) {
    n_hi <- sum(dat$debt_to_equity > b["upper"], na.rm = TRUE)
    dat$debt_to_equity <- pmin(dat$debt_to_equity, b["upper"])
    if (n_hi > 0)
      cat(sprintf("  Winsorize debt_to_equity: %d high (> %.2f)\n", n_hi, b["upper"]))
  }
  
  # --- interest_cov (one-sided upper) --------------------------------------
  b <- winsor_bounds$interest_cov
  if (any(!is.na(dat$interest_cov))) {
    n_hi <- sum(dat$interest_cov > b["upper"], na.rm = TRUE)
    dat$interest_cov <- pmin(dat$interest_cov, b["upper"])
    if (n_hi > 0)
      cat(sprintf("  Winsorize interest_cov: %d high (> %.2f)\n", n_hi, b["upper"]))
  }
  
  # --- firm_age (one-sided upper) ------------------------------------------
  b <- winsor_bounds$firm_age
  if (any(!is.na(dat$firm_age))) {
    n_hi <- sum(dat$firm_age > b["upper"], na.rm = TRUE)
    dat$firm_age <- pmin(dat$firm_age, b["upper"])
    if (n_hi > 0)
      cat(sprintf("  Winsorize firm_age: %d high (> %.2f)\n", n_hi, b["upper"]))
  }
  
  
  dat
}



# ============================================================================
# Rescales -- not currently used
# ============================================================================

rescale_with_clean_params <- function(x_impaired, scaling_params) {
  (x_impaired - scaling_params["mean"]) / scaling_params["sd"]
}

# for random noise only
rescale_mean_only <- function(x_impaired, scaling_params) {
  x_impaired - scaling_params["mean"]
}

# implausible values doesn't get rescaled


