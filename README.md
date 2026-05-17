# Health Insurance Premium Analysis & Indian Actuarial Pricing
**Language:** R  
**Data:** Kaggle Medical Cost Dataset (1,338 records) + IALM 2006-08 Mortality Table  
**Author:** Phebe Christofia D

---

## Project Overview
A three-part actuarial pricing project built in R.

**Part 1** analyses a US health insurance dataset using log-linear 
regression to identify risk factors and predict premiums.

**Part 2** uses the official Indian IALM 2006-08 mortality table 
to calculate term life insurance premiums using simple (qx × SA) 
and EPV (Axn) methods.

**Part 3** implements four core actuarial insurance products — 
Term Assurance, Pure Endowment, Endowment Assurance and Whole Life 
— with smoker loading applied to all.

---

## Key Findings

- Smoking multiplies insurance charges by **4.73x**
- BMI correlates **0.81** with charges for smokers 
  but only **0.08** for non-smokers — interaction effect
- Model explains **76.7%** of charge variation (R² = 0.767)
- A 55-year-old smoker pays **50x more** than a 25-year-old 
  non-smoker for identical ₹50 lakh coverage over 20 years

---

## Part 1 — Regression Analysis

**Dataset:** Medical Cost Personal Dataset (Kaggle)  
**Records:** 1,338 policyholders  
**Method:** Log-linear regression

**Results:**
| Metric | Value |
|---|---|
| Adjusted R² | 0.767 |
| RMSE (test data) | $8,692 |
| Smoker loading | 4.73x |
| Age effect | +3.5% per year |

**Steps:**
1. Data loading and verification
2. Exploratory Data Analysis — smoking, age, BMI
3. BMI-smoking interaction discovery
4. Train/test split 70/30
5. Log-linear model — log(charges) ~ all variables
6. Coefficient interpretation using exp()
7. RMSE validation on unseen test data
8. Interactive premium calculator function

---

## Part 2 — Indian Actuarial Pricing (Simple & EPV)

**Dataset:** IALM 2006-08 Mortality Table  
**Approved by:** IRDAI, effective 1st April 2013  
**Coverage:** ₹50 lakh, 20-year term, 6% interest

**Method 1 — Simple pricing:**
Pure Premium = qx × Sum Assured

**Method 2 — Expected Present Value (Axn):**
Axn = Σ v^(k+1) × kpx × qx+k  for k = 0 to n-1

**EPV Premium Table (20-year term, ₹50 lakh):**
| Age | Axn | Non-Smoker | Smoker |
|---|---|---|---|
| 25 | 0.0148 | ₹73,956 | ₹3,50,551 |
| 30 | 0.0203 | ₹1,01,456 | ₹4,80,901 |
| 35 | 0.0310 | ₹1,55,107 | ₹7,35,209 |
| 40 | 0.0486 | ₹2,42,759 | ₹11,50,679 |
| 45 | 0.0744 | ₹3,71,848 | ₹17,62,562 |
| 50 | 0.1103 | ₹5,51,552 | ₹26,14,355 |
| 55 | 0.1585 | ₹7,92,588 | ₹37,56,869 |

---

## Part 3 — Four Actuarial Products

All products use IALM 2006-08 mortality table with 
smoker loading (4.73x) derived from Part 1 regression.

| Product | Formula | Pays when |
|---|---|---|
| Term Assurance | Axn = Σ v^(k+1) × kpx × qx | Death within term |
| Pure Endowment | nEx = v^n × nPx | Survival to end of term |
| Endowment Assurance | Axn + nEx | Death OR survival |
| Whole Life | Ax = Σ v^(k+1) × kpx × qx (to age 115) | Death at any age |

**Product Comparison — Age 30, ₹50 lakh, 20 years, 6%:**
| Product | Non-Smoker | Smoker |
|---|---|---|
| Term Assurance | ₹1,01,456 | ₹4,80,901 |
| Pure Endowment | ₹14,95,038 | ₹70,86,481 |
| Endowment Assurance | ₹15,96,494 | ₹75,67,381 |
| Whole Life | ₹4,36,504 | ₹20,69,030 |

---

## Files

| File | Description |
|---|---|
| `insurance_analysis.R` | Complete R script — all 3 parts, 21 sections |
| `README.md` | This file |

**Note:** Data files not included.  
Download insurance.csv from Kaggle and IALM xlsx 
from actuariesindia.org

---

## How to Run

1. Install R and RStudio
2. Run: `install.packages("readxl")`
3. Open `insurance_analysis.R` in RStudio
4. Run top to bottom
5. Select `insurance.csv` when file browser opens first
6. Select IALM xlsx file when prompted second

---

## Data Sources

1. Kaggle — Medical Cost Personal Dataset  
   kaggle.com/datasets/mirichoi0218/insurance

2. Institute of Actuaries of India — IALM 2006-08  
   actuariesindia.org

3. IRDAI — Approval of IALM table, April 2013
