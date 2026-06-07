# ============================================================================
# 01_functions.R
# ============================================================================
# All function definitions for the Credit Risk Data Quality simulation.
#
# This file consolidates every function from the development notebook
# (credit_risk_data_quality_walkthrough.Rmd) and the original helper
# functions file (02_helperfunctions.R) into a single source.
#
# Usage:
#   source("01_functions.R")   # at the top of the MC runner or the Rmd
#
# Contents:
#   Stage 1 — simulate_clean_data_panel()
#   Stage 2 — impair_mcar(), impair_mar(), impair_mnar(),
#             impair_noise(), impair_implausible()
#   Stage 3 — domain_to_na(), winsorize_vars(), prepare_for_modeling()
#   Stage 4 — fit_logistic_models(), fit_xgboost_models(),
#             assign_pd_grades()
#   Stage 5 — evaluate_models()
#   FRS     — compute_frs(), compute_frs_all(), summarize_frs(),
#             print_frs(), compare_frs_imputation()
# ============================================================================




# ============================================================================
# STAGE 1: DATA GENERATING PROCESS
# ============================================================================


# Creating panel data

##########################################

simulate_clean_data_panel <- function(N = 5000, seed = 21,
                                      min_term = 3, max_term = 7,
                                      drift_sd_log_assets     = 0.05,
                                      drift_sd_debt_to_equity = 0.3,
                                      drift_sd_interest_cov   = 0.3,
                                      drift_scale             = 4.0,
                                      seasoning_period        = 1,
                                      crefo_refresh_prob      = 0.5) {  
  set.seed(seed)
  
  # ============================================================
  # PHASE 1: FIRM ORIGINATION
  # ============================================================
  
  # Var 1: Firm Age
  firm_age_orig <- rgamma(N, shape = 3, rate = 0.15)
  
  
  # Var 2: Sector
  # Three sectors instead of binary
  # 1 = services (reference), 2 = hospitality, 3 = manufacturing
  # Proportions reflect a realistic B2B lending portfolio for a leasing company
  sector   <- sample(1:3, N, replace = TRUE, prob = c(0.57, 0.19, 0.24))
  sector_2 <- as.integer(sector == 2)   # Hospitality (gastro) dummy
  sector_3 <- as.integer(sector == 3)   # Manufacturing (Verarb. Gewerbe) dummy
  
  # Var 3: Total Assets (firm size. log transformed, as per Ohlson (1980))
  log_assets_orig <- 8 + 0.03 * firm_age_orig - 0.3 * sector_2 + 0.3 * sector_3 + rnorm(N, 0, 1)
  # Gastro - smaller firms, manufacturing - need for more assets
  
  # Var 4: Debt-to-Equity Ratio (leverage)
  debt_to_equity_orig <- pmax(3 - 0.1 * scale(log_assets_orig) - 0.02 * scale(firm_age_orig) +   rgamma(N, shape = 1, rate = 1), 0)
  
  # Var 5: Interest Coverage Ratio (cash flow)
  interest_cov_orig   <- pmax(5 - 0.4 * scale(debt_to_equity_orig) + 0.3 * scale(log_assets_orig) + rnorm(N, 0, 1), 0)
  
  log_assets_mean     <- mean(log_assets_orig);     log_assets_sd     <- sd(log_assets_orig)
  debt_to_equity_mean <- mean(debt_to_equity_orig); debt_to_equity_sd <- sd(debt_to_equity_orig)
  interest_cov_mean   <- mean(interest_cov_orig);   interest_cov_sd   <- sd(interest_cov_orig)
  firm_age_mean       <- mean(firm_age_orig);       firm_age_sd       <- sd(firm_age_orig)
  
  
  # Var 6: CreditReform Bonitätsindex (Crefo score)
  crefo_orig <- round(
    300
    + 60  * scale(debt_to_equity_orig)
    - 50  * scale(interest_cov_orig)
    - 30  * scale(log_assets_orig)
    + 40  * sector_2   # Gastro: worse baseline creditworthiness
    + 20  * sector_3   # Manufactuing: slight penalty vs services
    + rnorm(N, 0, 25)
  )
  
  crefo_orig <- pmin(pmax(crefo_orig, 100), 600)     # clamp to valid Crefo range
  
  crefo_mean <- mean(crefo_orig);   crefo_sd   <- sd(crefo_orig)
  
  # Additional variables -- keeping record of firms and their loans over time
  firm_id      <- sprintf("FIRM_%05d", seq_len(N))
  vintage_year <- sample(2005:2025, N, replace = TRUE)
  loan_term    <- sample(min_term:max_term, N, replace = TRUE)
  
  # Firm trajectory type: assigned once at origination, fixed for the firm's lifetime.
  # Determines the direction of the drift mean in the annual random walk.
  # Deteriorating firms face state-dependent worsening (as before).
  # Stable firms follow a pure random walk with zero drift mean.
  # Improving firms experience a gentle drift in the opposite direction --
  # leverage falls slowly, coverage and assets improve.
  # Proportions (0.2 / 0.6 / 0.2) reflect a realistic credit portfolio
  # where most firms are broadly stable and distressed/improving firms are minorities.
  trajectory <- sample(c("deteriorating", "stable", "improving"),
                       N, replace = TRUE, prob = c(0.2, 0.6, 0.2))
  
  # ============================================================
  # PHASE 2: TRUE COEFFICIENTS
  # All betas are on the standardized scale (change in log-odds per 1 SD).
  # ============================================================
  
  beta0               <- -3.9   # the log-odds of the default rate we want to recover. currently around 2%
  beta_log_assets     <- -0.2   # larger companies - less risk
  beta_debt_to_equity <-  0.4   # companies with more leverage (debt) = more risk
  beta_interest_cov   <- -0.3   # more interest coverage / higher cash flows = less risk
  beta_sector2        <-  0.4   # gastro = more risk, higher default rate
  beta_sector3        <-  0.18  # manufacturing needs slight uplift to get to 2.3% default rate
  beta_firm_age       <- -0.2   # older companies - less risk
  beta_crefo          <-  0.3   # higher Crefo score → higher default risk
  
  
  # Accounting for Gastro default risk increases in later vintages
  # Centered on midpoint of 2005-2025 range so effect is ~zero at portfolio average
  gastro_trend        <- 0.04  # log-odds increase per year for Gastro
  vintage_center      <- 2015
  # (by 2025, gastro firms get an additional +.4 on log-odds scale relative to 2025, so a meaningful increase in PD)
  
  # ============================================================
  # PHASE 3: PANEL EXPANSION
  # ============================================================
  
  rows <- vector("list", N)
  
  # Creating the origination values for firm [i]
  for (i in seq_len(N)) {
    
    la    <- log_assets_orig[i]
    dte   <- debt_to_equity_orig[i]
    ic    <- interest_cov_orig[i]
    fa    <- firm_age_orig[i]
    crefo <- crefo_orig[i]           
    
    firm_rows <- vector("list", loan_term[i])
    
    for (t in seq_len(loan_term[i])) {
      
      obs_year <- vintage_year[i] + t - 1
      
      # --- 3a. State-dependent drift from year 2 onward ---
      if (t > 1) {
        
        # Standardize current values to compute pd_curr as deterioration weight
        la_z_curr  <- (la  - log_assets_mean)     / log_assets_sd
        dte_z_curr <- (dte - debt_to_equity_mean) / debt_to_equity_sd
        ic_z_curr  <- (ic  - interest_cov_mean)   / interest_cov_sd
        fa_z_curr  <- (fa  - firm_age_mean)        / firm_age_sd
        
        # pd_curr probability creation: how intensely are you deteriorating (if at all)?
        pd_curr <- plogis(beta0 +
                            beta_log_assets     * la_z_curr  +
                            beta_debt_to_equity * dte_z_curr +
                            beta_interest_cov   * ic_z_curr  +
                            beta_sector2        * sector_2[i] +
                            beta_sector3        * sector_3[i] +
                            beta_firm_age       * fa_z_curr)
        # Note: Crefo intentionally excluded from pd_curr used for drift weighting.
        # The drift mechanism operates on the underlying financial state; Crefo
        # is a derived / external signal and including it here would create circular feedback.
        
        # Below: the drift mechanism depending on state (deteriorating, stable, improving).
        # For deteriorating: the worse your current PD is, the stronger it pulls you down.
        if (trajectory[i] == "deteriorating") {
          drift_mean_dte <- +drift_scale * pd_curr
          drift_mean_ic  <- -drift_scale * pd_curr
          drift_mean_la  <- -drift_scale * 0.5 * pd_curr
        } else if (trajectory[i] == "stable") {
          drift_mean_dte <- 0
          drift_mean_ic  <- 0
          drift_mean_la  <- 0
        } else {  # improving
          drift_mean_dte <- -drift_scale * 0.5
          drift_mean_ic  <-  drift_scale * 0.5
          drift_mean_la  <-  drift_scale * 0.25
        }
        
        # Here are the yearly updates for the financials: drifting plus random noise
        dte <- pmax(dte + rnorm(1, mean = drift_mean_dte, sd = drift_sd_debt_to_equity), 0)
        ic  <- pmax(ic  + rnorm(1, mean = drift_mean_ic,  sd = drift_sd_interest_cov),   0)
        la  <-      la  + rnorm(1, mean = drift_mean_la,  sd = drift_sd_log_assets)
        fa  <- fa + 1
        
        # Added: Crefo staleness mechanism
        # Each year the score is refreshed with probability crefo_refresh_prob.
        # A refreshed score reflects the firm's *current* financial state (post-drift).
        # Otherwise the prior year's value carries forward unchanged.
        # This is a baseline data generation feature, not an experimental impairment —
        # it reflects realistic bureau update cycles (Creditreform typically updates
        # scores annually or on event triggers, but not all firms are updated each year).
        if (runif(1) < crefo_refresh_prob) {
          crefo <- round(
            340
            + 30  * ((dte - debt_to_equity_mean) / debt_to_equity_sd)
            - 20  * ((ic  - interest_cov_mean)   / interest_cov_sd)
            - 20  * ((la  - log_assets_mean)      / log_assets_sd)
            + 40  * sector_2[i]
            + 20  * sector_3[i]
            + rnorm(1, 0, 20)
          )
          crefo <- min(max(crefo, 100), 600)
        }
        # else: crefo carries forward from previous year — staleness preserved
      }
      
      # --- 3b. Standardize using origination-year scaling parameters ---
      la_z    <- (la    - log_assets_mean)     / log_assets_sd
      dte_z   <- (dte   - debt_to_equity_mean) / debt_to_equity_sd
      ic_z    <- (ic    - interest_cov_mean)   / interest_cov_sd
      fa_z    <- (fa    - firm_age_mean)        / firm_age_sd
      crefo_z <- (crefo - crefo_mean)           / crefo_sd   # NEW
      
      # --- 3c. Compute default probability and draw outcome ---
      
      # The linear predictor: computing log-odds of default for each firm-year
      linpred <- beta0 +
        beta_log_assets     * la_z +
        beta_debt_to_equity * dte_z +
        beta_interest_cov   * ic_z +
        beta_sector2        * sector_2[i] +
        beta_sector3        * sector_3[i] +
        beta_firm_age       * fa_z +
        beta_crefo          * crefo_z +
        gastro_trend * sector_2[i] * (obs_year - vintage_center)
      
      #remove gastro_trend to see if recovery improves (or include in the model)
      
      
      pd_true <- plogis(linpred)
      
      # --- 3d. Seasoning period: no defaults allowed in year 1 on the books ---
      if (t <= seasoning_period) {
        default <- 0L
      } else {
        default <- rbinom(1, size = 1, prob = pd_true)
      }
      
      # Some firms will pay back their loans when due.
      repaid <- as.integer(!default & t == loan_term[i])
      
      # --- 3e. Store this firm-year row ---
      firm_rows[[t]] <- data.frame(
        firm_id          = firm_id[i],
        vintage_year     = vintage_year[i],
        obs_year         = vintage_year[i] + t - 1,
        years_on_book    = t,
        sector           = sector[i],      # raw label (1/2/3) — for diagnostics
        sector_2         = sector_2[i],
        sector_3         = sector_3[i],
        loan_term        = loan_term[i],
        trajectory       = trajectory[i],
        log_assets       = la,
        debt_to_equity   = dte,
        interest_cov     = ic,
        firm_age         = fa,
        crefo            = crefo,          
        log_assets_z     = la_z,
        debt_to_equity_z = dte_z,
        interest_cov_z   = ic_z,
        firm_age_z       = fa_z,
        crefo_z          = crefo_z,        
        pd_true          = pd_true,
        default          = default,
        repaid           = repaid
      )
      
      # Stop following the firm after default.
      if (default == 1L) {
        firm_rows <- firm_rows[seq_len(t)]
        break
      }
    }
    
    # Finish the firm, then store it in its "sub-list".
    rows[[i]] <- do.call(rbind, firm_rows)
  }
  
  # Then combine all firms to create the final panel
  panel <- do.call(rbind, rows)
  
  # ============================================================
  # PHASE 4: REPORTING
  # ============================================================
  
  n_rows      <- nrow(panel)
  n_defaulted <- sum(panel$default)
  n_repaid    <- length(unique(panel$firm_id[panel$repaid == 1]))
  
  cat("Firms simulated:       ", N, "\n")
  cat("Total firm-year rows:  ", n_rows, "\n")
  cat("Default events:        ", n_defaulted,
      sprintf("(%.1f%% of firm-years)\n", 100 * n_defaulted / n_rows))
  cat("Firms repaid:          ", n_repaid, "\n")
  cat("Obs year range:        ", min(panel$obs_year), "to", max(panel$obs_year), "\n")
  
  # --- Deterioration diagnostic ---
  defaulted_firms <- panel[panel$default == 1, "firm_id"]
  defaulter_panel <- panel[panel$firm_id %in% defaulted_firms, ]
  
  pd_trajectory <- do.call(rbind, lapply(
    split(defaulter_panel, defaulter_panel$firm_id),
    function(df) {
      df_sorted <- df[order(df$years_on_book), ]
      data.frame(pd_year1 = df_sorted$pd_true[1],
                 pd_final = df_sorted$pd_true[nrow(df_sorted)])
    }
  ))
  
  cat("\n--- Deterioration diagnostic (defaulting firms only) ---\n")
  cat("Mean pd_true year 1:  ", round(mean(pd_trajectory$pd_year1), 4), "\n")
  cat("Mean pd_true final:   ", round(mean(pd_trajectory$pd_final), 4), "\n")
  cat("Mean years observed:  ",
      round(mean(tapply(defaulter_panel$firm_id, defaulter_panel$firm_id, length)), 2), "\n")
  
  # ============================================================
  # PHASE 5: RETURN
  # ============================================================
  
  # Winsorizing bounds from clean data — used by FRS diagnostics (compute_frs)
  # to measure data quality against the clean-world reference distribution.
  # NOT used by prepare_for_modeling(), which computes its own percentile
  # cutoffs from the data it receives (practitioner approach).
  winsor_bounds <- list(
    log_assets     = c(lower = as.numeric(quantile(panel$log_assets, 0.025)),
                       upper = as.numeric(quantile(panel$log_assets, 0.975))),
    debt_to_equity = c(lower = 0,
                       upper = as.numeric(quantile(panel$debt_to_equity, 0.95))),
    interest_cov   = c(lower = 0,
                       upper = as.numeric(quantile(panel$interest_cov, 0.95))),
    firm_age       = c(lower = 0,
                       upper = as.numeric(quantile(panel$firm_age, 0.95)))
    # Note: not winsorizing Crefo as it already has hard domain bounds 
  )
  
  
  # Plausibility bounds from clean data — wider than winsorizing cutoffs.
  # These represent "can this value exist in this domain?" rather than
  # "is this value typical?" (which is what winsor_bounds/outlier captures).
  # Using 1st/99th percentiles for two-sided variables, 99th for one-sided.
  # Igl (Sec. 8.6): plausibility requires domain knowledge; these empirical
  # bounds operationalize a practitioner's sense of "what's possible."
  plausibility_bounds_empirical <- list(
    log_assets     = c(as.numeric(quantile(panel$log_assets, 0.01)),
                       as.numeric(quantile(panel$log_assets, 0.99))),
    debt_to_equity = c(0,
                       as.numeric(quantile(panel$debt_to_equity, 0.99))),
    interest_cov   = c(0,
                       as.numeric(quantile(panel$interest_cov, 0.99))),
    firm_age       = c(0,
                       as.numeric(quantile(panel$firm_age, 0.99))),
    crefo          = c(100, 600)   # hard domain bounds from Creditreform index
  )
  
  list(
    data = panel,
    scaling = list(
      log_assets     = c(mean = log_assets_mean,     sd = log_assets_sd),
      debt_to_equity = c(mean = debt_to_equity_mean, sd = debt_to_equity_sd),
      interest_cov   = c(mean = interest_cov_mean,   sd = interest_cov_sd),
      firm_age       = c(mean = firm_age_mean,        sd = firm_age_sd),
      crefo          = c(mean = crefo_mean,           sd = crefo_sd)    # NEW
    ),
    winsor_bounds = winsor_bounds,
    plausibility_bounds = plausibility_bounds_empirical,  
    true_betas = c(
      intercept       = beta0,
      log_assets      = beta_log_assets,
      debt_to_equity  = beta_debt_to_equity,
      interest_cov    = beta_interest_cov,
      sector_2        = beta_sector2,        
      sector_3        = beta_sector3,        
      firm_age        = beta_firm_age,
      crefo           = beta_crefo           
    )
  )
}

# For later: record what our PD formula is
pd_formula <- default ~ log_assets_z + debt_to_equity_z + interest_cov_z +
                             firm_age_z + crefo_z + sector_2 + sector_3

# ============================================================================
# STAGE 2: IMPAIRMENT FUNCTIONS
# ============================================================================


# ── MCAR: Missing Completely At Random ─────────────────────────────────
# Missingness applied independently to each cell at random.
# Variables: financial ratios and firm_age (not sector, not crefo).
# Mild: ~8% missing per cell, Severe: ~28% missing per cell

impair_mcar <- function(dat, severity = "mild") {

  miss_prob <- switch(severity,
    mild   = 0.08,
    severe = 0.28
  )

  dat_impaired <- dat
  n <- nrow(dat)

  financial_vars <- c("log_assets", "debt_to_equity", "interest_cov", "firm_age")

  for (var in financial_vars) {
    missing_idx <- rbinom(n, 1, prob = miss_prob) == 1
    dat_impaired[[var]][missing_idx] <- NA
    dat_impaired[[paste0(var, "_z")]][missing_idx] <- NA
    cat(sprintf("MCAR missing rate for %s: %.3f\n", var, mean(missing_idx)))
  }

  attr(dat_impaired, "impairment") <- list(
    type      = "MCAR",
    severity  = severity,
    miss_prob = miss_prob,
    vars      = financial_vars
  )

  dat_impaired
}


# ── MAR: Missing At Random ────────────────────────────────────────────
# Missingness in crefo depends on firm_age (fully observed).
# Younger firms less likely to have a Creditreform score.
# Mild: ~8% average missing, Severe: ~28% average missing

# Intercepts tuned to match MCAR severity levels for fair comparison:
# Mild: ~8% average missing, Severe: ~28% average missing
# Negative coefficient on firm_age: younger firms → higher missingness prob

impair_mar <- function(dat, severity = "mild") {

  mar_intercept <- switch(severity,
    mild   = -1.5,
    severe = 0.1
  )

  dat_impaired <- dat
  n <- nrow(dat)

  prob_missing <- plogis(mar_intercept - 0.05 * dat$firm_age)
  cat("Average MAR missing rate:", round(mean(prob_missing), 3), "\n")

  missing_idx <- rbinom(n, 1, prob = prob_missing) == 1
  dat_impaired$crefo[missing_idx] <- NA
  dat_impaired$crefo_z[missing_idx] <- NA

  attr(dat_impaired, "impairment") <- list(
    type          = "MAR",
    severity      = severity,
    mar_intercept = mar_intercept,
    mar_coef      = -0.05,
    avg_miss_rate = mean(prob_missing),
    driven_by     = "firm_age",
    vars          = "crefo"
  )

  dat_impaired
}


# ── MNAR: Missing Not At Random ───────────────────────────────────────
# Missingness in interest_cov depends on interest_cov itself.
# Firms with poor coverage strategically omit reporting.
# Persistence: once triggered, remains missing for all subsequent years.
# Mild: ~8% of firm-years, Severe: ~28% of firm-years

impair_mnar <- function(dat, severity = "mild") {

  mnar_intercept <- switch(severity,
    mild   = -3.9,
    severe = -2.5
  )

  dat_impaired <- dat
  n <- nrow(dat)

  # Step 1: Compute per-observation trigger probability
  # Negative coefficient: lower interest coverage = higher missingness prob
  prob_missing <- plogis(mnar_intercept - 1.0 * dat$interest_cov_z)
  cat("Average MNAR trigger probability:", round(mean(prob_missing), 3), "\n")

  # Step 2: Draw initial missingness triggers (per firm-year)
  trigger <- rbinom(n, 1, prob = prob_missing) == 1

  # Step 3: Carry forward within each firm (now vectorized)
  # Once triggered, all subsequent years for that firm are also missing
  # Data is already sorted by firm_id + years_on_book from the DGP,
  # so within each split group the row order is chronological.

  firm_factor <- factor(dat_impaired$firm_id)
  trigger <- unsplit(
    lapply(split(trigger, firm_factor), function(tv) {
      first_hit <- which(tv)
      if (length(first_hit) > 0) tv[first_hit[1]:length(tv)] <- TRUE
      tv
    }),
    firm_factor
  )
  
  # Step 4: Apply missingness
  dat_impaired$interest_cov[trigger] <- NA
  dat_impaired$interest_cov_z[trigger] <- NA

  realized_rate <- mean(trigger)
  cat("Realized MNAR missing rate (after persistence):", round(realized_rate, 3), "\n")

  attr(dat_impaired, "impairment") <- list(
    type           = "MNAR",
    severity       = severity,
    mnar_intercept = mnar_intercept,
    mnar_coef      = -1.0,
    trigger_rate   = mean(prob_missing),
    realized_rate  = realized_rate,
    persistent     = TRUE,
    driven_by      = "interest_cov_z",
    vars           = "interest_cov"
  )

  dat_impaired
}


# ── Measurement Noise ─────────────────────────────────────────────────
# Gaussian noise added to continuous financial predictors.
# Severity defined as % of each variable's clean SD.
# Mild: 30% of clean SD, Severe: 100% of clean SD

impair_noise <- function(dat, scaling, severity = "mild") {

  noise_sd_multiplier <- switch(severity,
    mild   = 0.30,
    severe = 1.00
  )

  dat_impaired <- dat
  n <- nrow(dat)

  # Variables to corrupt — continuous financial predictors only
  # (firm_age excluded: it drives MNAR so corrupting it would confound comparisons)
  # (sector excluded: categorical)
  noise_vars <- c("log_assets", "debt_to_equity", "interest_cov")

  for (v in noise_vars) {
    clean_sd   <- scaling[[v]]["sd"]
    clean_mean <- scaling[[v]]["mean"]
    noise      <- rnorm(n, mean = 0, sd = noise_sd_multiplier * clean_sd)
    dat_impaired[[v]] <- dat_impaired[[v]] + noise
    dat_impaired[[paste0(v, "_z")]] <- (dat_impaired[[v]] - clean_mean) / clean_sd
  }

  for (v in noise_vars) {
    cat(sprintf("Noise SD for %s: %.4f (%.0f%% of clean SD = %.4f) | realized SD: %.4f\n",
      v,
      noise_sd_multiplier * scaling[[v]]["sd"],
      noise_sd_multiplier * 100,
      scaling[[v]]["sd"],
      sd(dat_impaired[[v]], na.rm = TRUE)
    ))
  }

  attr(dat_impaired, "impairment") <- list(
    type               = "noise",
    severity           = severity,
    noise_sd_multiplier = noise_sd_multiplier,
    vars               = noise_vars
  )

  dat_impaired
}


impair_implausible <- function(dat, scaling, severity = "mild") {
  
  corrupt_rate <- switch(severity,
                         mild   = 0.05,
                         severe = 0.15
  )
  
  dat_impaired <- dat
  n <- nrow(dat)
  
  # ---- log_assets --------------------------------------------------------
  # Real-world errors: decimal shifts, unit confusion (thousands vs millions),
  # or placeholder values from system migrations
  n_corrupt <- floor(corrupt_rate * n)
  idx <- sample(n, n_corrupt, replace = FALSE)
  error_type <- sample(1:3, n_corrupt, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  
  new_vals <- dat_impaired$log_assets[idx]  # start from real values
  new_vals[error_type == 1] <- -1           # domain-invalid: negative log assets
  new_vals[error_type == 2] <- new_vals[error_type == 2] + 
    sample(c(-1, 1), sum(error_type == 2), replace = TRUE) * 
    runif(sum(error_type == 2), 4, 6) * sd(dat$log_assets)   # extreme outlier
  new_vals[error_type == 3] <- new_vals[error_type == 3] * 10  # decimal shift
  
  dat_impaired$log_assets[idx] <- new_vals
  dat_impaired$log_assets_z <- (dat_impaired$log_assets - scaling$log_assets["mean"]) / scaling$log_assets["sd"]
  cat(sprintf("Implausible in log_assets: %d rows (%.0f%%) — %d invalid, %d extreme, %d subtle\n",
              n_corrupt, corrupt_rate * 100,
              sum(error_type == 1), sum(error_type == 2), sum(error_type == 3)))
  
  # ---- debt_to_equity ----------------------------------------------------
  # Real-world errors: sign errors, ratio computed upside down,
  # or values carried from a different reporting standard
  idx <- sample(n, n_corrupt, replace = FALSE)
  error_type <- sample(1:3, n_corrupt, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  
  new_vals <- dat_impaired$debt_to_equity[idx]
  new_vals[error_type == 1] <- -abs(new_vals[error_type == 1]) - 
    runif(sum(error_type == 1), 0.5, 5)                        # negative values
  new_vals[error_type == 2] <- new_vals[error_type == 2] * 
    runif(sum(error_type == 2), 5, 20)                          # ratio blown up
  new_vals[error_type == 3] <- 1 / pmax(new_vals[error_type == 3], 0.01)  # inverted ratio
  
  dat_impaired$debt_to_equity[idx] <- new_vals
  dat_impaired$debt_to_equity_z <- (dat_impaired$debt_to_equity - scaling$debt_to_equity["mean"]) / scaling$debt_to_equity["sd"]
  cat(sprintf("Implausible in debt_to_equity: %d rows (%.0f%%) — %d invalid, %d extreme, %d subtle\n",
              n_corrupt, corrupt_rate * 100,
              sum(error_type == 1), sum(error_type == 2), sum(error_type == 3)))
  
  # ---- interest_cov ------------------------------------------------------
  # Real-world errors: sign flip, decimal shift, or stale placeholder
  idx <- sample(n, n_corrupt, replace = FALSE)
  error_type <- sample(1:3, n_corrupt, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  
  new_vals <- dat_impaired$interest_cov[idx]
  new_vals[error_type == 1] <- -abs(new_vals[error_type == 1])  # sign flip: negative coverage
  new_vals[error_type == 2] <- new_vals[error_type == 2] * 
    runif(sum(error_type == 2), 10, 100)                         # decimal shift
  new_vals[error_type == 3] <- 0                                 # placeholder zero
  
  dat_impaired$interest_cov[idx] <- new_vals
  dat_impaired$interest_cov_z <- (dat_impaired$interest_cov - scaling$interest_cov["mean"]) / scaling$interest_cov["sd"]
  cat(sprintf("Implausible in interest_cov: %d rows (%.0f%%) — %d invalid, %d extreme, %d subtle\n",
              n_corrupt, corrupt_rate * 100,
              sum(error_type == 1), sum(error_type == 2), sum(error_type == 3)))
  
  # ---- firm_age ----------------------------------------------------------
  # Real-world errors: negative from date arithmetic bugs, age in months
  # instead of years, or placeholder values
  idx <- sample(n, n_corrupt, replace = FALSE)
  error_type <- sample(1:3, n_corrupt, replace = TRUE, prob = c(0.3, 0.3, 0.4))
  
  new_vals <- dat_impaired$firm_age[idx]
  new_vals[error_type == 1] <- -runif(sum(error_type == 1), 1, 20)   # negative age
  new_vals[error_type == 2] <- 999                                    # placeholder
  new_vals[error_type == 3] <- new_vals[error_type == 3] * 12         # months not years
  
  dat_impaired$firm_age[idx] <- new_vals
  cat(sprintf("Implausible in firm_age: %d rows (%.0f%%) — %d invalid, %d placeholder, %d subtle\n",
              n_corrupt, corrupt_rate * 100,
              sum(error_type == 1), sum(error_type == 2), sum(error_type == 3)))
  
  # No rescaling — implausible values are intended to distort the variable's
  # distribution. Rescaling would compress the damage.
  
  attr(dat_impaired, "impairment") <- list(
    type         = "implausible",
    severity     = severity,
    corrupt_rate = corrupt_rate,
    error_types  = c("domain_invalid", "extreme_outlier", "subtle"),
    vars         = c("log_assets", "debt_to_equity", "interest_cov", "firm_age")
  )
  
  dat_impaired
}



# ============================================================================
# STAGE 3: DATA PREPARATION
# ============================================================================

# ── Domain check: replace impossible values with NA ────────────────────

domain_to_na <- function(dat) {
  n_flagged <- 0

  # log_assets: must be >= 0
  bad <- !is.na(dat$log_assets) & dat$log_assets < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible log_assets (< 0) → NA\n", sum(bad)))
    dat$log_assets[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }

  # debt_to_equity: must be >= 0
  bad <- !is.na(dat$debt_to_equity) & dat$debt_to_equity < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible debt_to_equity (< 0) → NA\n", sum(bad)))
    dat$debt_to_equity[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }

  # interest_cov: must be >= 0
  bad <- !is.na(dat$interest_cov) & dat$interest_cov < 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible interest_cov (< 0) → NA\n", sum(bad)))
    dat$interest_cov[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }

  # firm_age: must be > 0
  bad <- !is.na(dat$firm_age) & dat$firm_age <= 0
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible firm_age (<= 0) → NA\n", sum(bad)))
    dat$firm_age[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }

  # crefo: must be in [100, 600]
  bad <- !is.na(dat$crefo) & (dat$crefo < 100 | dat$crefo > 600)
  if (any(bad)) {
    cat(sprintf("  Domain check: %d impossible crefo (outside 100-600) → NA\n", sum(bad)))
    dat$crefo[bad] <- NA
    n_flagged <- n_flagged + sum(bad)
  }

  if (n_flagged == 0) cat("  Domain check: no impossible values found.\n")

  dat
}


# ── Winsorize extreme-but-plausible values ─────────────────────────────
# Percentile cutoffs computed from the data as received (practitioner
# approach). Crefo excluded — hard domain bounds handled by domain_to_na.

winsorize_vars <- function(dat) {

  # --- log_assets (two-sided) ---
  if (any(!is.na(dat$log_assets))) {
    b <- quantile(dat$log_assets, c(0.025, 0.975), na.rm = TRUE)
    n_lo <- sum(dat$log_assets < b[1], na.rm = TRUE)
    n_hi <- sum(dat$log_assets > b[2], na.rm = TRUE)
    dat$log_assets <- pmin(pmax(dat$log_assets, b[1]), b[2])
    if (n_lo + n_hi > 0)
      cat(sprintf("  Winsorize log_assets: %d low (< %.2f), %d high (> %.2f)\n",
                  n_lo, b[1], n_hi, b[2]))
  }

  # --- debt_to_equity (one-sided upper) ---
  if (any(!is.na(dat$debt_to_equity))) {
    b_upper <- quantile(dat$debt_to_equity, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$debt_to_equity > b_upper, na.rm = TRUE)
    dat$debt_to_equity <- pmin(dat$debt_to_equity, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize debt_to_equity: %d high (> %.2f)\n", n_hi, b_upper))
  }

  # --- interest_cov (one-sided upper) ---
  if (any(!is.na(dat$interest_cov))) {
    b_upper <- quantile(dat$interest_cov, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$interest_cov > b_upper, na.rm = TRUE)
    dat$interest_cov <- pmin(dat$interest_cov, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize interest_cov: %d high (> %.2f)\n", n_hi, b_upper))
  }

  # --- firm_age (one-sided upper) ---
  if (any(!is.na(dat$firm_age))) {
    b_upper <- quantile(dat$firm_age, 0.95, na.rm = TRUE)
    n_hi <- sum(dat$firm_age > b_upper, na.rm = TRUE)
    dat$firm_age <- pmin(dat$firm_age, b_upper)
    if (n_hi > 0)
      cat(sprintf("  Winsorize firm_age: %d high (> %.2f)\n", n_hi, b_upper))
  }

  dat
}


# ── Prepare data for modeling ──────────────────────────────────────────
# Pipeline: domain-to-NA → winsorize → impute → rescale.
# Methods: "ersatzwert" (RR default), "median", "mice"

prepare_for_modeling <- function(dat, scaling, method = "ersatzwert",
                                 model_formula = NULL, label = NULL) {
  
  # Auto-generate label from impairment metadata if not provided
  if (is.null(label)) {
    imp_meta <- attr(dat, "impairment")
    if (!is.null(imp_meta)) {
      label <- paste(imp_meta$type, imp_meta$severity)
    } else {
      label <- "unlabeled"
    }
  }
  
  # Preparation function: sits between impairment and model fitting.
  # Handles the following tasks:
  #   1. Identify extreme outliers and converts them to missings
  #   2. Winsorize data (two or one tailed, depending on variables)
  #   3. Impute NA values introduced by missingness impairment (MCAR/MAR/MNAR)
  #   4. Recompute _z columns using clean scaling parameters
  #
  # The method argument controls the imputation strategy:
  #   "ersatzwert" — RR univariate Ersatzwert imputation (default).
  #              For each variable independently: fits a univariate logistic
  #              regression (default ~ x) on non-missing cases, computes the
  #              observed default rate among missing cases (AR_MV), and solves
  #              for the x-value that would produce that default rate:
  #              Ersatzwert = (logit(AR_MV) - β₀) / β₁
  #              All missing rows receive the same replacement value per variable.
  #              Implements the RR development-phase imputation methodology.
  #   "median" — univariate median imputation (PD-neutral baseline;
  #              maps to ~z=0 after standardisation, adding approximately
  #              zero to the linear predictor). 
  #   "mice"  — multivariate imputation by chained equations (van Buuren
  #              & Groothuis-Oudshoorn, 2011). Uses predictive mean matching
  #              (pmm) as the default method, which preserves the distribution
  #              shape and avoids impossible values. Single imputation (m=1)
  #              is used because Monte Carlo replications serve the role of
  #              repeated draws. Requires the mice package.
  #
  # This function is applied identically regardless of the missingness
  # mechanism (MCAR/MAR/MNAR), reflecting practitioner norms where the
  # true mechanism is unknown and all missingness is treated the same.
  
  dat_prepared <- dat
  
  # Variables that may have been impaired with missingness
  imputable_vars <- c("log_assets", "debt_to_equity", "interest_cov", "firm_age", "crefo")
  
  # ---- Prep Part 1 & 2: Domain check + Winsorize (RR practitioner workflow) --------
  # Applied to ALL impairment types, not just implausible values.
  # A practitioner doesn't know the true impairment mechanism, so the
  # same preparation pipeline runs uniformly on every dataset.
  cat(sprintf("[%s] Data preparation:\n", label))
  
  # This function can be found in "helper functions" -- creating NAs where implausible
  dat_prepared <- domain_to_na(dat_prepared)
  
  # Make sure NAs also live in _z columns
  for (v in imputable_vars) {
    z_col <- paste0(v, "_z")
    if (z_col %in% names(dat_prepared)) {
      dat_prepared[[z_col]][is.na(dat_prepared[[v]])] <- NA
    }
  }
  
  # Winsorizing the data — percentile cutoffs computed from the data as
  # received, matching what a practitioner would do without oracle access.
  dat_prepared <- winsorize_vars(dat_prepared)
  
  # Count NAs here for diagnostics BEFORE imputation (in _z and raw cols)
  predictor_vars_z <- paste0(imputable_vars, "_z")
  predictor_vars_z <- predictor_vars_z[predictor_vars_z %in% names(dat_prepared)]
  n_missing_cells_pre <- sum(is.na(dat_prepared[, predictor_vars_z]))
  
  # Check if there are any NAs to impute
  has_missing <- any(sapply(imputable_vars, function(v) any(is.na(dat_prepared[[v]]))))
  
  
  # ---- Prep Part 3: Imputation 3 ways --------
  # ---- Ersatzwert (default), median, MICE ----
  
  if (has_missing) {
    
    if (method == "ersatzwert") {
      
      outcome_var <- all.vars(model_formula)[1]
      
      for (v in imputable_vars) {
        z_col <- paste0(v, "_z")
        if (!(z_col %in% names(dat_prepared))) next
        
        miss_idx <- is.na(dat_prepared[[z_col]])
        if (!sum(miss_idx)) next
        
        # Default rate among firms with missing values for this variable
        ar_mv <- mean(dat_prepared[[outcome_var]][miss_idx], na.rm = TRUE)
        
        # Guard against 0 or 1 default rates (would produce -Inf/Inf)
        ar_mv <- pmax(pmin(ar_mv, 1 - 1e-6), 1e-6)
        
        # Univariate logit on non-missing cases
        uni_dat <- dat_prepared[!miss_idx, c(outcome_var, z_col)]
        uni_fit <- glm(as.formula(paste(outcome_var, "~", z_col)),
                       data = uni_dat, family = binomial())
        
        beta0 <- coef(uni_fit)[1]
        beta1 <- coef(uni_fit)[2]
        
        if (is.na(beta1) || abs(beta1) < 1e-8) {
          # Degenerate case: feature has no univariate signal → impute 0
          replacement <- 0
        } else {
          replacement <- (log(ar_mv / (1 - ar_mv)) - beta0) / beta1
        }
        
        # All missing rows get the same replacement value
        dat_prepared[[z_col]][miss_idx] <- replacement
        
        # Back-transform to raw scale for consistency
        if (v %in% names(scaling) && v %in% names(dat_prepared)) {
          dat_prepared[[v]][miss_idx] <- scaling[[v]]["mean"] +
            replacement * scaling[[v]]["sd"]
        }
        
        cat(sprintf("  %s: %d NAs → Ersatzwert = %.4f (AR_MV = %.4f)\n",
                    z_col, sum(miss_idx), replacement, ar_mv))
      }
      
      cat(sprintf("[%s] RR univariate imputation complete | %d cells imputed\n",
                  label, n_missing_cells_pre))
      
    } else if (method == "median") {
      
      for (var in imputable_vars) {
        n_missing <- sum(is.na(dat_prepared[[var]]))
        if (n_missing > 0) {
          fill_value <- median(dat_prepared[[var]], na.rm = TRUE)
          dat_prepared[[var]][is.na(dat_prepared[[var]])] <- fill_value
          cat(sprintf("Median imputed %d NAs in %-20s method: %s, value: %.4f\n",
                      n_missing, var, method, fill_value))
        }
      }
      
    } else if (method == "mice") {
      
      require(mice)
      
      mice_vars <- imputable_vars[imputable_vars %in% names(dat_prepared)]
      mice_cols <- c(mice_vars, "sector_2", "sector_3", "default")
      mice_cols <- mice_cols[mice_cols %in% names(dat_prepared)]
      
      mice_input <- dat_prepared[, mice_cols]
      
      for (var in imputable_vars) {
        n_missing <- sum(is.na(dat_prepared[[var]]))
        if (n_missing > 0) {
          cat(sprintf("MICE imputing %d NAs in %s\n", n_missing, var))
        }
      }
      
      imp <- mice(mice_input, m = 1, method = "pmm", maxit = 5,
                  printFlag = FALSE)
      completed <- complete(imp, 1)
      
      for (var in mice_vars) {
        dat_prepared[[var]] <- completed[[var]]
      }
      
      cat("MICE imputation complete.\n")
      
    } else {
      stop(sprintf("Unknown imputation method: '%s'", method))
    }
  }
  
  
  # ---- Prep Part 4: Rescaling --------
  # Recompute all _z columns using clean scaling parameters.
  # Applied identically regardless of impairment type — the practitioner
  # does not know the true mechanism, so the same standardization runs
  # on every dataset.
  for (var in names(scaling)) {
    z_col <- paste0(var, "_z")
    if (z_col %in% names(dat_prepared)) {
      dat_prepared[[z_col]] <- (dat_prepared[[var]] - scaling[[var]]["mean"]) / scaling[[var]]["sd"]
    }
  }
  
  # Tag with preparation metadata
  attr(dat_prepared, "preparation") <- list(
    method = method,
    vars_checked = imputable_vars
  )
  
  dat_prepared
}



# ============================================================================
# STAGE 4: MODEL FITTING
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





# ── Fit XGBoost across all conditions ──────────────────────────────────

fit_xgboost_models <- function(clean_data,
                               impaired_list,
                               prepared_list,
                               formula,
                               true_betas = NULL,
                               # ── Hyperparameters (fixed across all conditions) ──
                               params = list(
                                 objective        = "binary:logistic",
                                 eval_metric      = "logloss",
                                 max_depth        = 3,
                                 eta              = 0.1,
                                 subsample        = 0.8, # Adding randomness by not always using full sample / cols
                                 colsample_bytree = 0.8,
                                 min_child_weight = 5
                               ),
                               nrounds          = 500,
                               early_stopping   = 30,
                               seed             = 21) {
  
  # Extract predictor names and response from the formula
  response   <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]
  
  # Build the master list: clean + impaired + prepared
  master <- list()
  master[["clean"]] <- list(stage = "clean", data = clean_data)
  
  for (nm in names(impaired_list)) {
    master[[paste0("imp_", nm)]] <- list(stage = "impaired", data = impaired_list[[nm]])
  }
  for (nm in names(prepared_list)) {
    master[[paste0("prep_", nm)]] <- list(stage = "prepared", data = prepared_list[[nm]])
  }
  
  # Pre-allocate storage
  models     <- vector("list", length(master))
  importance <- vector("list", length(master))
  names(models) <- names(importance) <- names(master)
  
  for (key in names(master)) {
    
    d     <- master[[key]]$data
    stage <- master[[key]]$stage
    
    # Build xgb.DMatrix -- XGBoost accepts NA natively in the matrix
    X <- as.matrix(d[, predictors, drop = FALSE])
    y <- d[[response]]
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    # Train with early stopping using the training set as evals (watchlist).
    # In a single-run simulation (not CV), this prevents gross overfitting
    # while keeping the pipeline simple. The evaluation story comes later
    # with out-of-sample prediction on clean holdout data.
    set.seed(seed)
    fit <- xgb.train(
      params    = params,
      data      = dtrain,
      nrounds   = nrounds,
      evals =  list(train = dtrain),
      early_stopping_rounds = early_stopping,
      verbose   = 0
    )
    
    models[[key]] <- fit
    
    # Feature importance (gain-based)
    imp <- xgb.importance(model = fit)
    
    # Ensure all predictors appear even if importance is zero
    imp_full <- data.frame(
      key       = key,
      stage     = stage,
      condition = ifelse(stage == "clean", "clean",
                         sub("^imp_|^prep_", "", key)),
      Feature   = predictors,
      stringsAsFactors = FALSE
    )
    imp_full <- merge(imp_full, imp[, c("Feature", "Gain", "Cover", "Frequency")],
                      by = "Feature", all.x = TRUE)
    imp_full[is.na(imp_full)] <- 0
    
    importance[[key]] <- imp_full
  }
  
  # Stack importance tables
  importance_df <- do.call(rbind, importance)
  rownames(importance_df) <- NULL
  
  # Parse condition into type + severity
  importance_df$type <- ifelse(
    importance_df$condition == "clean", "clean",
    sub("_mild$|_severe$", "", importance_df$condition)
  )
  importance_df$severity <- ifelse(
    importance_df$condition == "clean", "none",
    ifelse(grepl("severe", importance_df$condition), "severe", "mild")
  )
  
  list(
    models     = models,         # named list of xgb.Booster objects
    importance = importance_df   # long-format data.frame
  )
}


# ── PD Grading: assign firms to Moody's-aligned risk grades ───────────



assign_pd_grades <- function(models,
                             datasets,
                             formula,
                             model_type = c("glm", "xgboost")) {
  
  model_type <- match.arg(model_type)
  response   <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]
  
  # Moody's-aligned PD grade boundaries (upper bounds, inclusive)
  grade_breaks <- c(0, 0.00001, 0.0006, 0.0011, 0.003,
                    0.007, 0.015, 0.03, 0.08, 0.15, 1.0)
  grade_labels <- as.character(1:10)
  
  results <- vector("list", length(models))
  names(results) <- names(models)
  
  for (key in names(models)) {
    
    mod <- models[[key]]
    d   <- datasets[[key]]
    
    # Predict PDs
    if (model_type == "glm") {
      pd_pred <- predict(mod, newdata = d, type = "response")
    } else {
      X <- as.matrix(d[, predictors, drop = FALSE])
      pd_pred <- predict(mod, xgb.DMatrix(data = X))
    }
    
    # Assign grades using fixed Moody's-aligned boundaries
    grade <- cut(pd_pred,
                 breaks  = grade_breaks,
                 labels  = grade_labels,
                 include.lowest = TRUE,
                 right   = TRUE)
    
    results[[key]] <- data.frame(
      key     = key,
      firm_id = if ("firm_id" %in% names(d)) d$firm_id else seq_len(nrow(d)),
      year    = if ("year" %in% names(d)) d$year else NA,
      pd_pred = pd_pred,
      grade   = as.integer(as.character(grade)),
      default = d[[response]],
      stringsAsFactors = FALSE
    )
  }
  
  # Stack everything
  graded <- do.call(rbind, results)
  rownames(graded) <- NULL
  
  # Parse key into stage + condition
  graded$stage <- ifelse(graded$key == "clean", "clean",
                         ifelse(grepl("^imp_", graded$key), "impaired", "prepared"))
  graded$condition <- ifelse(graded$key == "clean", "clean",
                             sub("^imp_|^prep_", "", graded$key))
  graded$type <- ifelse(graded$condition == "clean", "clean",
                        sub("_mild$|_severe$", "", graded$condition))
  graded$severity <- ifelse(graded$condition == "clean", "none",
                            ifelse(grepl("severe", graded$condition), "severe", "mild"))
  
  graded
}

# ============================================================================
# STAGE 5: EVALUATION METRICS
# ============================================================================


# ── Evaluate models: discrimination + calibration metrics ──────────────

evaluate_models <- function(models,
                            eval_data,
                            formula,
                            model_type = c("glm", "xgboost")) {
  
  model_type <- match.arg(model_type)
  response   <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]
  
  actual <- eval_data[[response]]
  
  # ── Helper: AUC via Mann-Whitney U ──────────────────────────────────
  # P(score_defaulter > score_non_defaulter), with 0.5 for ties.
  # Equivalent to pROC::auc() but avoids the dependency.
  compute_auc <- function(pred, actual) {
    scores_1 <- pred[actual == 1]
    scores_0 <- pred[actual == 0]
    n1 <- length(scores_1)
    n0 <- length(scores_0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    U <- sum(vapply(scores_1, function(s) {
      sum(s > scores_0) + 0.5 * sum(s == scores_0)
    }, numeric(1)))
    U / (n1 * n0)
  }
  
  # ── Helper: KS statistic ────────────────────────────────────────────
  # Max |F1(t) - F0(t)| where F1 = CDF of defaulter scores,
  # F0 = CDF of non-defaulter scores.
  compute_ks <- function(pred, actual) {
    scores_1 <- pred[actual == 1]
    scores_0 <- pred[actual == 0]
    if (length(scores_1) == 0 || length(scores_0) == 0) return(NA_real_)
    cdf_1 <- ecdf(scores_1)
    cdf_0 <- ecdf(scores_0)
    all_scores <- sort(unique(pred))
    max(abs(cdf_1(all_scores) - cdf_0(all_scores)))
  }
  
  # ── Loop over all fitted models ─────────────────────────────────────
  results <- vector("list", length(models))
  names(results) <- names(models)
  
  for (key in names(models)) {
    
    mod <- models[[key]]
    
    # Predict on the SHARED clean evaluation data
    if (model_type == "glm") {
      pd_pred <- predict(mod, newdata = eval_data, type = "response")
    } else {
      X <- as.matrix(eval_data[, predictors, drop = FALSE])
      pd_pred <- predict(mod, xgb.DMatrix(data = X))
    }
    
    # Discrimination
    auc_val  <- compute_auc(pd_pred, actual)
    gini_val <- 2 * auc_val - 1
    ks_val   <- compute_ks(pd_pred, actual)
    
    # Calibration
    brier_val <- mean((pd_pred - actual)^2)
    obs_rate  <- mean(actual)
    exp_rate  <- mean(pd_pred)
    oe_ratio  <- obs_rate / exp_rate
    
    # Parse key into stage + condition
    stage <- ifelse(key == "clean", "clean",
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
  
  # Stack and parse type/severity
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
#   - ersatzwert: after Ersatzwert imputation
#   - median:   after median imputation
#   - mice:     after MICE imputation
#
# Restricted to MCAR / MAR / MNAR since these are the mechanisms where
# imputation is relevant (noise and implausible values aren't missing data).
# ============================================================================

compare_frs_imputation <- function(dat_clean, impaired_list,
                                   ersatz_list, median_list, mice_list,
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
  add_stage(ersatz_list, "ersatzwert")
  add_stage(median_list,   "median")
  add_stage(mice_list,     "mice")
  
  out <- do.call(rbind, all_frs)
  rownames(out) <- NULL
  
  out$stage <- factor(out$stage,
                      levels = c("clean", "impaired", "ersatzwert", "median", "mice"))
  out
}

