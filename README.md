# Credit-Risk-Data-Quality-Simulation

  # This project simulates a firm-level panel dataset for credit risk modeling, 
  # including both a clean "perfect world" dataset and systematically impaired 
  # datasets to study the impact of data quality on model performance 
  # (recovery & robustness).
  

  # ============================================================
  # ============================================================
  # DATA GENERATING PROCESS
  # ============================================================
  # ============================================================
  # Part 1: Creating the perfect world 
  # ============================================================
  
  # What this does:
  ### - Creates 5,000 firms with loans from 1-7 years w/ vintage 2005 to 2025
  ### - Over time firms will have financials that stay the same (random walk),
  ###   get worse (20%, negative drift), or get better (20%, positive drift)
  ### - Some firms will default, some will pay off their loans
  
  ###### (inner loop for the firm level + outer loop for multiple firms + rbind)
  
  
  #--------------------------------------------------------------
  
  # Indicator variables:
  
  ## pure values
  ### - Firm age (distribution: more younger firms)
  ### - Sector (currently two sectors, random coin flip)
  
  ## chained regression dependencies
  ### - (log) Assets (dependent on age and sector)
  ###        --> firm size proxy
  ### - Debt to equity (leverage: higher is worse - dep. on age, sector, assets)
  ###       --> capital structure + how much debt exists
  ### - Interest coverage ratio: (income / interest exp) - below 1 = can't pay
        interest. dependent on everything above. 
  ###         --> cash flow proxy (ebit / revenue / margins)
     
  ### - potential to add Crefo score as an indicator?   
        
  ### Most features are standardized / can be used to "anchor" impaired data later to avoid confounding effects
  
  
  # ============================================================
  # ============================================================
  # DATA DESTRUCTION
  # ============================================================
  # ============================================================
  # Part 2a: MCAR - data missing completely at random
  # ============================================================
  
  # underlying idea: financials are missing with no real explanation
  
  # What this does:
  ### - We take our probability of being missing (8% for mild, 28% for severe) and 
  ###   flip a weighted coin for if the financial data will be missing
  ### - Add mean imputation (can be replaced with something more accurate)
  ### - rescale the data back to what the original mean / sd were
  ### - report the missing rates for the financials
  
  
  
  # ============================================================
  # Part 2b: MNAR - data not missing at random
  # ============================================================
  
  # underlying idea: 
  ### MNAR: missingness in interest_cov depends on interest_cov itself
  ### Mechanism: firms with poor interest coverage strategically omit reporting
  ### This is MNAR because missingness depends on the unobserved value of the
  ### variable itself (Little & Rubin, 2002; Zhang, 2023, p. 117).
  ### Logistic regression method used for realistic gradual probability change.
  
  # What this does:
  ### - Logistic regresion step: probability of being missing depends on how bad
  ###   the "interest_cov" values are
  ### - based on the above: flip a weighted coin if values will be missing
  ### - Add mean imputation (can be replaced with something more accurate)
  ### - report the missing rates for the financials
  
  
  
  # ============================================================
  # Part 2c: MAR - data missing at random (could delete or edit)
  # ============================================================
  
  # Underlying idea:
  ### Missingness is due to older firms not reporting
  ### (could also tie this into the crefo score)
  
  ### same procedure as MNAR with a logistic regression
  
  
  
  
  # ============================================================
  # Part 2d: Measurement Noise
  # ============================================================
  
  # Underlying idea / execution
  ### the financials can have extra measurement noise from....
  ### assuming just gaussian noise (w/ no particular direction)
  
  
  # What this does
  ### - Mild data: adds an extra 10% of the clean SD to the financials
  ### - Severe: adds an extra 50%
  ### - in other words, pushing further away from the true value / expanding SEs
  ### - rescale just with the mean (or else we undo everything)
  
  
  
  
  # ============================================================
  # Part 2e: Implausible values
  # ============================================================  
  
  # Underlying idea / execution
  ### sometimes values are in the dataset that make no sense (negatives that 
  ### can't exist, super far-out outliers)
  
  ### create one dataset with untouched implausibles / one with them corrected
  ### (floored?)
  
  # What this does:
  ### - replace a random subset of rows with domain-invalid or extreme outlier
  ###   values, depending on the variable
  ###     --> log_assets and interest_coverage is being 6 SDs away
  ###     --> debt_equity and firm_age get implausible (negative) values
  
  
  
  # ============================================================
  # ============================================================
  # MODEL FITTING
  # ============================================================
  # ============================================================
  # Part 3a: Standard Logistic model
  # ============================================================
  