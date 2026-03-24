# Credit-Risk-Data-Quality-Simulation


  # ============================================================
  # Part 1: Creating the perfect world
  # ============================================================
  
  # What this does:
  ### - simulates a panel dataset. 
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
  # Part 2a: 
  # ============================================================