# ============================================================================
# 02 Helper Functions
# ============================================================================



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


