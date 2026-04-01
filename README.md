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
  - Sector differences included  

**Note:**  
Most features are standardized to allow anchoring after data impairments.

---

## Stage 2: Data Destruction

### Part 2a: MCAR (Missing Completely at Random)

**Idea:**  
Financials are missing with no systematic reason.

**What this does:**
- Apply missingness probabilities:
  - Mild: 8%
  - Severe: 28%
- Randomly remove values (weighted coin flip)
- Rescale to original mean and standard deviation
- Report missingness rates

---

### Part 2b: MNAR (Missing Not at Random)

**Idea:**  
Missingness depends on the variable itself.

- Firms with poor interest coverage are less likely to report it. And if they don't report it one year, they will just stop reporting it
- Logistic function controls missingness probability

**What this does:**
- Missing probability increases as `interest_cov` worsens
- Apply probabilistic removal
- Report missingness rates

---

### Part 2c: MAR (Missing at Random)

**Idea:**  
Missingness depends on observed variables.

- Crefo scores missing for newer firms
- Availability increases over time

**Method:**  
- Logistic regression (similar to MNAR setup)

---

### Part 2d: Measurement Noise

**Idea:**  
Financials contain random measurement error.

**What this does:**
- Mild: +10% of original standard deviation
- Severe: +50%
- Adds Gaussian noise
- Rescales mean only (to preserve distortion)

---

### Part 2e: Implausible Values

**Idea:**  
Data may contain impossible or extreme values.

**What this does:**
- Inject invalid or extreme values:
  - `log_assets`, `interest_cov`: extreme outliers (~6 SD)
  - `debt_to_equity`, `firm_age`: invalid negatives
- Create:
  - Dataset with raw implausibles
  - Dataset with corrected values (e.g., flooring)

---

## Stage 3: Fixing the Data

**Approaches tested:**
- Median imputation
- MICE
- Listwise deletion (implicit part of logistic regression)

**Additional step:**
- Rescaling to maintain original distributions (except implausibles)

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