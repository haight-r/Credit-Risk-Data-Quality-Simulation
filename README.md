# Credit Risk Data Quality Simulation

This project simulates a firm-level panel dataset for credit risk modeling, including both a clean **"perfect world" dataset** and systematically impaired datasets to study the impact of data quality on model performance (recovery & robustness).

---

## How to Use
- Download the project zip file. "Credit risk data quality walkthrough" should run everything including the helper functions

### Stage 1: Data Generating Process

#### Part 1: Creating the Perfect World

**What this does:**
- Creates 5,000 firms with loans from 1–7 years (vintages 2005–2025)
- Firms evolve over time:
  - Stable (random walk)
  - Worse (20%, negative drift)
  - Better (20%, positive drift)
- Some firms default, others repay

---

### Indicator Variables

**Pure values**
- Firm age (skewed toward younger firms)
- Sector (currently three sectors)

**Chained regression dependencies**
- **Log assets**
  - Depends on age and sector  
  - → proxy for firm size  

- **Debt-to-equity**
  - Depends on age, sector, and assets  
  - → proxy for capital structure / leverage  

- **Interest coverage ratio**
  - Income / interest expense  
  - < 1 = cannot cover interest  
  - Depends on all prior variables  
  - → proxy for cash flow  

- **Crefo score**
  - External credit bureau score (payment behavior)
  - Depends on prior variables
  - Sector differences included (sectors and specs to be defined) 


---

## Stage 2: Data Destruction

### Impairment 1: MCAR (Missing Completely at Random)

**Idea:**  
Financials are missing with no systematic reason.

**What this does:**
- Apply missingness probabilities:
  - Mild: 8%
  - Severe: 28%
- Randomly remove values (weighted coin flip), no true pattern
- Report missingness rates per column

---

### Impairment 2: MAR (Missing at Random)

**Idea:**  
Missingness depends on observed variables.

- Crefo scores missing for newer firms
- Availability increases over time

**Method:**  
- Logistic regression: probability of Crefo being missing decreases as `firm_age` decreases
- Crefo scores removed based on probabilities calculated in prior step
- Report missingness rates

---

### Impairment 3: MNAR (Missing Not at Random)

**Idea:**  
Missingness depends on the variable itself.

- Firms with poor interest coverage are less likely to report it. And if they don't report it one year, they will just stop reporting it
- Logistic function controls missingness probability

**What this does:**
- Similar to MAR setup: missing probability increases as `interest_cov` worsens
- Apply probabilistic removal
- Report missingness rates


---

### Impairment 4: Measurement Noise

**Idea:**  
Financials contain random measurement error.

**What this does:**
- Mild: +10% of original standard deviation
- Severe: +50%
- Adds Gaussian noise and returns data


---

### Impairment 5: Implausible Values

**Idea:**  
Data may contain impossible or extreme values.

Different kinds of implausibility:
- 1) Domain-invalid (clearly impossible values -- like negative age or debt-equity)
- 2) Extreme outliers (technically possible but far outside the normal range. Or even placeholder "999"s)
- 3) Subtle mistakes (decimal place shifted, switching numerator and denominator in ratios)

**What this does:**
- Randomly injects the three types of implausibility at different ratios
- Mild impairment: 5% of rows damaged / severe: 15%

---

## Stage 3: Fixing the Data

**Step 1: Domain check: Identifying outliers and winsorizing**
- Values that violate domain rules (e.g. negative ratios) are treated as missing

**Step 2: Winsorizing**
- Remaining extremes capped at clean-data percentile cutoffs (5% tail, one or two-sided depending on the variable)

**Step 3: Imputation** (side quest 2: comparison across methods)
- Single regression (approach at RR)
- Median imputation
- MICE
- Listwise deletion (implicit part of logistic regression)

**Step 4: Rescaling**
- Z-scores recomputed using clean-data mean and SD, ensuring all coefficients remain on the original scale and any observed differences are attributable to the impairment itself, not to scale drift

---

## Feature Robustness Score (FRS)

Custom function based on:
- Completeness
- Validity
- Outlier detection
- Cross-field consistency

**Final score:**  
- Geometric mean of all four components

**FRS is tested on clean, impaired, and prepared data.**

---

## Model Fitting

### Part 3a: Standard Logistic Model