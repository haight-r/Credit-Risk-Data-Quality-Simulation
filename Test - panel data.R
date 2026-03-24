## DO NOT EDIT - this feature is already implemented in the main RMD 

# Creating panel data

# have an inner loop that simulates years / data for one business
# and an outer loop that simulates for multiple businesses

##########################################

simulate_clean_data_panel <- function(N = 5000, seed = 1,
                                      min_term = 3, max_term = 7,
                                      drift_sd_log_assets     = 0.05,
                                      drift_sd_debt_to_equity = 0.03,
                                      drift_sd_interest_cov   = 0.03,
                                      drift_scale             = 0.3,
                                      seasoning_period        = 1) {
  set.seed(seed)
  
  # ============================================================
  # PHASE 1: FIRM ORIGINATION
  # ============================================================
  
  firm_age_orig <- rgamma(N, shape = 3, rate = 0.15)
  sector        <- sample(0:1, N, replace = TRUE)
  
  log_assets_orig     <- 8 + 0.03 * firm_age_orig + 0.3 * sector + rnorm(N, 0, 1) # log bc very right-skewed
  debt_to_equity_orig <- pmax(3 - 0.1 * scale(log_assets_orig) - 0.02 * scale(firm_age_orig) +
                                rgamma(N, shape = 1, rate = 1), 0)
  interest_cov_orig   <- pmax(5 - 0.4 * scale(debt_to_equity_orig) + 0.3 * scale(log_assets_orig) +
                                rnorm(N, 0, 1), 0)
  
  log_assets_mean     <- mean(log_assets_orig);     log_assets_sd     <- sd(log_assets_orig)
  debt_to_equity_mean <- mean(debt_to_equity_orig); debt_to_equity_sd <- sd(debt_to_equity_orig)
  interest_cov_mean   <- mean(interest_cov_orig);   interest_cov_sd   <- sd(interest_cov_orig)
  firm_age_mean       <- mean(firm_age_orig);       firm_age_sd       <- sd(firm_age_orig)
  
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
  # beta0 lowered from -3.5 to -4.2 to reduce the annual default
  # probability, giving firms runway to deteriorate before defaulting
  # rather than exiting immediately at origination.
  # ============================================================
  
  beta0               <- -4.2
  beta_log_assets     <- -0.2
  beta_debt_to_equity <-  0.4
  beta_interest_cov   <- -0.3
  beta_sector         <-  0.2
  beta_firm_age       <- -0.1
  
  # ============================================================
  # PHASE 3: PANEL EXPANSION
  # ============================================================
  
  rows <- vector("list", N)
  
  for (i in seq_len(N)) {
    
    la  <- log_assets_orig[i]
    dte <- debt_to_equity_orig[i]
    ic  <- interest_cov_orig[i]
    fa  <- firm_age_orig[i]
    
    firm_rows <- vector("list", loan_term[i])
    
    for (t in seq_len(loan_term[i])) {
      
      # --- 3a. State-dependent drift from year 2 onward ---
      if (t > 1) {
        
        # Standardize current values to compute pd_curr as deterioration weight
        la_z_curr  <- (la  - log_assets_mean)     / log_assets_sd
        dte_z_curr <- (dte - debt_to_equity_mean) / debt_to_equity_sd
        ic_z_curr  <- (ic  - interest_cov_mean)   / interest_cov_sd
        fa_z_curr  <- (fa  - firm_age_mean)        / firm_age_sd
        
        pd_curr <- plogis(beta0 +
                            beta_log_assets     * la_z_curr +
                            beta_debt_to_equity * dte_z_curr +
                            beta_interest_cov   * ic_z_curr +
                            beta_sector         * sector[i] +
                            beta_firm_age       * fa_z_curr)
        
        # Drift means are determined by trajectory type.
        # Deteriorating: state-dependent push toward worse financials, scaled by pd_curr.
        # Stable: pure random walk, drift mean = 0 for all variables.
        # Improving: gentle fixed drift in the positive direction, independent of pd_curr.
        #   A fixed (not state-dependent) improvement rate reflects firms that are
        #   genuinely deleveraging or growing, rather than just getting lucky.
        if (trajectory[i] == "deteriorating") {
          drift_mean_dte <- +drift_scale * pd_curr
          drift_mean_ic  <- -drift_scale * pd_curr
          drift_mean_la  <- -drift_scale * 0.5 * pd_curr
        } else if (trajectory[i] == "stable") {
          drift_mean_dte <- 0
          drift_mean_ic  <- 0
          drift_mean_la  <- 0
        } else {  # improving
          drift_mean_dte <- -drift_scale * 0.5   # leverage slowly falls
          drift_mean_ic  <-  drift_scale * 0.5   # coverage slowly rises
          drift_mean_la  <-  drift_scale * 0.25  # assets grow modestly
        }
        
        dte <- pmax(dte + rnorm(1, mean = drift_mean_dte, sd = drift_sd_debt_to_equity), 0)
        ic  <- pmax(ic  + rnorm(1, mean = drift_mean_ic,  sd = drift_sd_interest_cov),   0)
        la  <-      la  + rnorm(1, mean = drift_mean_la,  sd = drift_sd_log_assets)
        fa  <- fa + 1
      }
      
      # --- 3b. Standardize using origination-year scaling parameters ---
      la_z  <- (la  - log_assets_mean)     / log_assets_sd
      dte_z <- (dte - debt_to_equity_mean) / debt_to_equity_sd
      ic_z  <- (ic  - interest_cov_mean)   / interest_cov_sd
      fa_z  <- (fa  - firm_age_mean)        / firm_age_sd
      
      # --- 3c. Compute default probability and draw outcome ---
      linpred <- beta0 +
        beta_log_assets     * la_z  +
        beta_debt_to_equity * dte_z +
        beta_interest_cov   * ic_z  +
        beta_sector         * sector[i] +
        beta_firm_age       * fa_z
      pd_true <- plogis(linpred)
      
      # --- 3d. Seasoning period: no defaults allowed in early years ---
      # Firms must survive at least `seasoning_period` years before they
      # are eligible to default. This reflects real credit portfolio
      # behaviour and ensures the deterioration mechanism has time to act.
      if (t <= seasoning_period) {
        default <- 0L
      } else {
        default <- rbinom(1, size = 1, prob = pd_true)
      }
      
      repaid <- as.integer(!default & t == loan_term[i])
      
      # --- 3e. Store this firm-year row ---
      firm_rows[[t]] <- data.frame(
        firm_id          = firm_id[i],
        vintage_year     = vintage_year[i],
        obs_year         = vintage_year[i] + t - 1,
        years_on_book    = t,
        sector           = sector[i],
        loan_term        = loan_term[i],
        trajectory       = trajectory[i],        # added
        log_assets       = la,
        debt_to_equity   = dte,
        interest_cov     = ic,
        firm_age         = fa,
        log_assets_z     = la_z,
        debt_to_equity_z = dte_z,
        interest_cov_z   = ic_z,
        firm_age_z       = fa_z,
        pd_true          = pd_true,
        default          = default,
        repaid           = repaid
      )
      
      if (default == 1L) {
        firm_rows <- firm_rows[seq_len(t)]
        break
      }
    }
    
    rows[[i]] <- do.call(rbind, firm_rows)
  }
  
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
  # For firms that defaulted, compare mean pd_true in their first observed
  # year vs their final (default) year. A healthy gap confirms the drift
  # mechanism is doing its job before exit.
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
  
  list(
    data = panel,
    scaling = list(
      log_assets     = c(mean = log_assets_mean,     sd = log_assets_sd),
      debt_to_equity = c(mean = debt_to_equity_mean, sd = debt_to_equity_sd),
      interest_cov   = c(mean = interest_cov_mean,   sd = interest_cov_sd),
      firm_age       = c(mean = firm_age_mean,        sd = firm_age_sd)
    ),
    true_betas = c(
      intercept       = beta0,
      log_assets      = beta_log_assets,
      debt_to_equity  = beta_debt_to_equity,
      interest_cov    = beta_interest_cov,
      sector          = beta_sector,
      firm_age        = beta_firm_age
    )
  )
}

######################################################################

result <- simulate_clean_data_panel()

###################################################################

head(result$data)
nrow(result$data)
table(result$data$default)
table(result$data$repaid)

panel <- result$data

# Correct check: count ALL rows per firm, but only for firms that eventually defaulted
defaulting_ids <- panel %>% filter(default == 1) %>% pull(firm_id)



library(dplyr)
panel %>%
  filter(firm_id %in% defaulting_ids) %>%
  group_by(firm_id) %>%
  summarise(years_observed = n()) %>%
  summary()



# trajectory check
panel %>%
  group_by(firm_id, trajectory) %>%
  summarise(pd_year1 = first(pd_true),
            pd_final = last(pd_true),
            .groups = "drop") %>%
  group_by(trajectory) %>%
  summarise(mean_pd_year1 = mean(pd_year1),
            mean_pd_final = mean(pd_final))

###############
defaulting_ids <- panel %>% filter(default == 1) %>% pull(firm_id)
repaid_ids     <- panel %>% filter(repaid == 1)  %>% pull(firm_id)

panel %>%
  filter(trajectory == "deteriorating") %>%
  group_by(firm_id) %>%
  summarise(pd_year1 = first(pd_true),
            pd_final = last(pd_true),
            exit     = case_when(
              firm_id %in% defaulting_ids ~ "defaulted",
              firm_id %in% repaid_ids     ~ "repaid",
              TRUE                        ~ "censored")) %>%
  group_by(exit) %>%
  summarise(mean_pd_year1 = mean(pd_year1),
            mean_pd_final = mean(pd_final))
