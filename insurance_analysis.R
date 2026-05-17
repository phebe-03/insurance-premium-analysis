# ============================================================
# HEALTH INSURANCE PREMIUM ANALYSIS & INDIAN ACTUARIAL PRICING
# Author   : Phebe Christofia D
# Data 1   : Medical Cost Personal Dataset (Kaggle, 1338 records)
# Data 2   : IALM 2006-08 Mortality Table (Actuaries India)
# ============================================================

# ── PACKAGES ─────────────────────────────────────────────────
library(readxl)

# ════════════════════════════════════════════════════════════
# PART 1 — REGRESSION ANALYSIS (US Health Insurance Data)
# ════════════════════════════════════════════════════════════

# ── 1. LOAD DATA ─────────────────────────────────────────────
data <- read.csv(file.choose())
head(data)
colnames(data)
nrow(data)

# ── 2. EXPLORE DATA ──────────────────────────────────────────
summary(data)

# ── 3. FIX DATA TYPES ────────────────────────────────────────
# Converting text columns to factors so R treats them as categories
data$smoker <- as.factor(data$smoker)
data$sex    <- as.factor(data$sex)
data$region <- as.factor(data$region)

summary(data$smoker)

# ── 4. EXPLORATORY DATA ANALYSIS (EDA) ───────────────────────

# Finding 1: Smoking effect on charges
mean(data$charges[data$smoker == "yes"])
mean(data$charges[data$smoker == "no"])

# Finding 2: Age correlation with charges
cor(data$age, data$charges)

# Finding 3: BMI correlation with charges
cor(data$bmi, data$charges)

# Finding 4: BMI-Smoking interaction effect
cor(data$bmi[data$smoker == "yes"], data$charges[data$smoker == "yes"])
cor(data$bmi[data$smoker == "no"],  data$charges[data$smoker == "no"])

# Visualise interaction effect
plot(data$bmi, data$charges,
     col  = ifelse(data$smoker == "yes", "red", "blue"),
     xlab = "BMI",
     ylab = "Charges ($)",
     main = "BMI vs Charges (Red = Smoker, Blue = Non-Smoker)")

# ── 5. TRAIN / TEST SPLIT ────────────────────────────────────
# set.seed ensures same random split every time we run
set.seed(42)
train_idx  <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data  <- data[-train_idx, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows :", nrow(test_data),  "\n")

# ── 6. BUILD MODEL ───────────────────────────────────────────
# log(charges) used because insurance charges are right-skewed
model <- lm(log(charges) ~ age + bmi + children + sex + smoker + region,
            data = train_data)

summary(model)

# ── 7. INTERPRET KEY COEFFICIENTS ────────────────────────────
cat("Smoking multiplier :", round(exp(coef(model)["smokeryes"]), 3), "x\n")
cat("Age effect per year:", round((exp(coef(model)["age"]) - 1) * 100, 2), "%\n")

# ── 8. VALIDATE MODEL ────────────────────────────────────────
test_data$pred_log     <- predict(model, test_data)
test_data$pred_charges <- exp(test_data$pred_log)

rmse <- sqrt(mean((test_data$charges - test_data$pred_charges)^2))
cat("─────────────────────────────\n")
cat("Model Validation\n")
cat("RMSE         : $", round(rmse, 2), "\n")
cat("Mean Charges : $", round(mean(data$charges), 2), "\n")
cat("─────────────────────────────\n")

# ── 9. PREMIUM CALCULATOR ────────────────────────────────────
predict_premium <- function(age, bmi, children, sex, smoker, region) {
  new_person <- data.frame(
    age      = age,
    bmi      = bmi,
    children = children,
    sex      = factor(sex,    levels = levels(train_data$sex)),
    smoker   = factor(smoker, levels = levels(train_data$smoker)),
    region   = factor(region, levels = levels(train_data$region))
  )
  pred_log    <- predict(model, new_person)
  pred_charge <- round(exp(pred_log), 2)
  cat("─────────────────────────────────────────\n")
  cat("Age:", age, "| BMI:", bmi, "| Smoker:", smoker, "\n")
  cat("Estimated Insurance Charge: $", pred_charge, "\n")
  cat("─────────────────────────────────────────\n")
}

# Example predictions
predict_premium(30, 26.5, 1, "female", "no",  "northwest")
predict_premium(30, 26.5, 1, "female", "yes", "northwest")
predict_premium(25, 26.5, 1, "female", "no",  "northwest")
predict_premium(55, 26.5, 1, "female", "no",  "northwest")

# ════════════════════════════════════════════════════════════
# PART 2 — INDIAN ACTUARIAL PRICING (IALM 2006-08)
# ════════════════════════════════════════════════════════════

# ── 10. LOAD IALM MORTALITY TABLE ────────────────────────────
ialm <- read_excel(file.choose(), skip = 2)

left  <- ialm[, 1:2]
right <- ialm[, 3:4]
colnames(left)  <- c("age", "qx")
colnames(right) <- c("age", "qx")
mortality <- rbind(left, right)

cat("Mortality table rows:", nrow(mortality), "\n")

# ── 11. PLOT MORTALITY CURVE ─────────────────────────────────
plot(mortality$age, mortality$qx,
     type = "l",
     col  = "darkred",
     lwd  = 2,
     xlab = "Age",
     ylab = "Probability of Death (qx)",
     main = "IALM 2006-08 Mortality Curve — India")

# ── 12. SIMPLE PRICING — qx x SA ─────────────────────────────
# Single year pure premium = qx x Sum Assured
pure_premium_smoker <- function(age, sum_assured, smoker) {
  qx      <- mortality$qx[mortality$age == age]
  premium <- qx * sum_assured
  loading       <- ifelse(smoker == "yes",
                          round(exp(coef(model)["smokeryes"]), 3), 1)
  final_premium <- premium * loading
  cat("─────────────────────────────────────\n")
  cat("Age:", age, "| Sum Assured: Rs.", sum_assured,
      "| Smoker:", smoker, "\n")
  cat("qx (mortality rate) :", qx, "\n")
  cat("Base Premium        : Rs.", round(premium, 2), "\n")
  cat("Final Premium       : Rs.", round(final_premium, 2), "\n")
  cat("─────────────────────────────────────\n")
}

pure_premium_smoker(30, 5000000, "no")
pure_premium_smoker(30, 5000000, "yes")
pure_premium_smoker(40, 5000000, "no")
pure_premium_smoker(40, 5000000, "yes")

# ── 13. SIMPLE PRICING COMPARISON TABLE ──────────────────────
ages    <- c(25, 30, 35, 40, 45, 50, 55)
loading <- round(exp(coef(model)["smokeryes"]), 3)

comparison <- data.frame(
  Age                = ages,
  qx                 = mortality$qx[mortality$age %in% ages],
  Non_Smoker_Premium = round(
    mortality$qx[mortality$age %in% ages] * 5000000, 0),
  Smoker_Premium     = round(
    mortality$qx[mortality$age %in% ages] * 5000000 * loading, 0)
)

print(comparison)

# ── 14. SIMPLE PRICING PLOT ───────────────────────────────────
plot(comparison$Age, comparison$Non_Smoker_Premium,
     type = "b", col = "blue", lwd = 2,
     ylim = c(0, 200000),
     xlab = "Age",
     ylab = "Annual Premium (Rs.)",
     main = "Term Insurance Premium by Age\nRs. 50 Lakh Cover — IALM 2006-08")

lines(comparison$Age, comparison$Smoker_Premium,
      type = "b", col = "red", lwd = 2)

legend("topleft",
       legend = c("Non-Smoker", "Smoker"),
       col    = c("blue", "red"),
       lwd    = 2)

# ════════════════════════════════════════════════════════════
# PART 3 — ADVANCED ACTUARIAL PRICING (EPV METHOD)
# ════════════════════════════════════════════════════════════

# ── 15. TERM ASSURANCE ───────────────────────────────────────
# Pays on death within term
# Formula: Axn = sum of v^(k+1) x kpx x qx for k = 0 to n-1
term_assurance <- function(age, term, interest_rate, sum_assured, smoker) {
  
  v   <- 1 / (1 + interest_rate)
  apv <- 0
  kpx <- 1
  
  for (k in 0:(term - 1)) {
    current_age <- age + k
    qx  <- mortality$qx[mortality$age == current_age]
    apv <- apv + (v^(k+1)) * kpx * qx
    kpx <- kpx * (1 - qx)
  }
  
  base_premium <- apv * sum_assured
  
  if (smoker == "yes") {
    loading       <- round(exp(coef(model)["smokeryes"]), 3)
    final_premium <- base_premium * loading
  } else {
    final_premium <- base_premium
  }
  
  cat("─────────────────────────────────────\n")
  cat("TERM ASSURANCE\n")
  cat("Age:", age, "| Term:", term, "years | Smoker:", smoker, "\n")
  cat("Interest Rate:", interest_rate * 100, "%\n")
  cat("Sum Assured: Rs.", format(sum_assured, big.mark = ","), "\n")
  cat("Axn value         :", round(apv, 6), "\n")
  cat("Base Premium (EPV): Rs.", round(base_premium, 2), "\n")
  cat("Final Premium     : Rs.", round(final_premium, 2), "\n")
  cat("─────────────────────────────────────\n")
}

term_assurance(30, 20, 0.06, 5000000, "no")
term_assurance(30, 20, 0.06, 5000000, "yes")
term_assurance(25, 20, 0.06, 5000000, "no")
term_assurance(40, 20, 0.06, 5000000, "no")

# ── 16. PURE ENDOWMENT ───────────────────────────────────────
# Pays only if person SURVIVES to end of term
# Formula: nEx = v^n x nPx
pure_endowment <- function(age, term, interest_rate, sum_assured, smoker) {
  
  v   <- 1 / (1 + interest_rate)
  kpx <- 1
  
  for (k in 0:(term - 1)) {
    current_age <- age + k
    qx  <- mortality$qx[mortality$age == current_age]
    kpx <- kpx * (1 - qx)
  }
  
  nEx          <- (v^term) * kpx
  base_premium <- nEx * sum_assured
  
  if (smoker == "yes") {
    loading       <- round(exp(coef(model)["smokeryes"]), 3)
    final_premium <- base_premium * loading
  } else {
    final_premium <- base_premium
  }
  
  cat("─────────────────────────────────────\n")
  cat("PURE ENDOWMENT\n")
  cat("Age:", age, "| Term:", term, "years | Smoker:", smoker, "\n")
  cat("Interest Rate:", interest_rate * 100, "%\n")
  cat("Sum Assured: Rs.", format(sum_assured, big.mark = ","), "\n")
  cat("nEx value         :", round(nEx, 6), "\n")
  cat("Base Premium (EPV): Rs.", round(base_premium, 2), "\n")
  cat("Final Premium     : Rs.", round(final_premium, 2), "\n")
  cat("─────────────────────────────────────\n")
}

pure_endowment(30, 20, 0.06, 5000000, "no")
pure_endowment(30, 20, 0.06, 5000000, "yes")
pure_endowment(25, 20, 0.06, 5000000, "no")
pure_endowment(40, 20, 0.06, 5000000, "no")

# ── 17. ENDOWMENT ASSURANCE ──────────────────────────────────
# Pays on death OR survival — whichever comes first
# Formula: Endowment = Axn + nEx
endowment_assurance <- function(age, term, interest_rate, sum_assured, smoker) {
  
  v   <- 1 / (1 + interest_rate)
  apv <- 0
  kpx <- 1
  
  for (k in 0:(term - 1)) {
    current_age <- age + k
    qx  <- mortality$qx[mortality$age == current_age]
    apv <- apv + (v^(k+1)) * kpx * qx
    kpx <- kpx * (1 - qx)
  }
  
  axn          <- apv
  nEx          <- (v^term) * kpx
  endowment    <- axn + nEx
  base_premium <- endowment * sum_assured
  
  if (smoker == "yes") {
    loading       <- round(exp(coef(model)["smokeryes"]), 3)
    final_premium <- base_premium * loading
  } else {
    final_premium <- base_premium
  }
  
  cat("─────────────────────────────────────\n")
  cat("ENDOWMENT ASSURANCE\n")
  cat("Age:", age, "| Term:", term, "years | Smoker:", smoker, "\n")
  cat("Interest Rate:", interest_rate * 100, "%\n")
  cat("Sum Assured: Rs.", format(sum_assured, big.mark = ","), "\n")
  cat("Axn (Term Assurance) :", round(axn, 6), "\n")
  cat("nEx (Pure Endowment) :", round(nEx, 6), "\n")
  cat("Endowment Value      :", round(endowment, 6), "\n")
  cat("Base Premium (EPV)   : Rs.", round(base_premium, 2), "\n")
  cat("Final Premium        : Rs.", round(final_premium, 2), "\n")
  cat("─────────────────────────────────────\n")
}

endowment_assurance(30, 20, 0.06, 5000000, "no")
endowment_assurance(30, 20, 0.06, 5000000, "yes")
endowment_assurance(25, 20, 0.06, 5000000, "no")
endowment_assurance(40, 20, 0.06, 5000000, "no")

# ── 18. WHOLE LIFE ───────────────────────────────────────────
# Pays whenever the person dies — no fixed term
# Formula: Ax = sum of v^(k+1) x kpx x qx from age x to age 115
whole_life <- function(age, interest_rate, sum_assured, smoker) {
  
  v   <- 1 / (1 + interest_rate)
  apv <- 0
  kpx <- 1
  
  for (k in 0:(115 - age)) {
    current_age <- age + k
    qx  <- mortality$qx[mortality$age == current_age]
    apv <- apv + (v^(k+1)) * kpx * qx
    kpx <- kpx * (1 - qx)
  }
  
  base_premium <- apv * sum_assured
  
  if (smoker == "yes") {
    loading       <- round(exp(coef(model)["smokeryes"]), 3)
    final_premium <- base_premium * loading
  } else {
    final_premium <- base_premium
  }
  
  cat("─────────────────────────────────────\n")
  cat("WHOLE LIFE ASSURANCE\n")
  cat("Age:", age, "| Term: Whole Life | Smoker:", smoker, "\n")
  cat("Interest Rate:", interest_rate * 100, "%\n")
  cat("Sum Assured: Rs.", format(sum_assured, big.mark = ","), "\n")
  cat("Ax value          :", round(apv, 6), "\n")
  cat("Base Premium (EPV): Rs.", round(base_premium, 2), "\n")
  cat("Final Premium     : Rs.", round(final_premium, 2), "\n")
  cat("─────────────────────────────────────\n")
}

whole_life(30, 0.06, 5000000, "no")
whole_life(30, 0.06, 5000000, "yes")
whole_life(25, 0.06, 5000000, "no")
whole_life(40, 0.06, 5000000, "no")

# ── 19. COMBINED PRODUCT COMPARISON TABLE ────────────────────
# Calculates EPV premium for all four products across ages 25-55
# sapply applies the calculation to each age in the list
ages    <- c(25, 30, 35, 40, 45, 50, 55)
loading <- round(exp(coef(model)["smokeryes"]), 3)
term    <- 20
i       <- 0.06
v       <- 1 / (1 + i)

product_comparison <- data.frame(
  
  Age = ages,
  
  # ── Term Assurance (Non-Smoker) ──
  Term_NS = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    round(apv * 5000000, 0)
  }),
  
  # ── Term Assurance (Smoker) ──
  Term_S = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    round(apv * 5000000 * loading, 0)
  }),
  
  # ── Pure Endowment (Non-Smoker) ──
  PureEndo_NS = sapply(ages, function(a) {
    kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      kpx <- kpx * (1 - qx)
    }
    nEx <- (v^term) * kpx
    round(nEx * 5000000, 0)
  }),
  
  # ── Pure Endowment (Smoker) ──
  PureEndo_S = sapply(ages, function(a) {
    kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      kpx <- kpx * (1 - qx)
    }
    nEx <- (v^term) * kpx
    round(nEx * 5000000 * loading, 0)
  }),
  
  # ── Endowment Assurance (Non-Smoker) ──
  Endo_NS = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    nEx       <- (v^term) * kpx
    endowment <- apv + nEx
    round(endowment * 5000000, 0)
  }),
  
  # ── Endowment Assurance (Smoker) ──
  Endo_S = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(term - 1)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    nEx       <- (v^term) * kpx
    endowment <- apv + nEx
    round(endowment * 5000000 * loading, 0)
  }),
  
  # ── Whole Life (Non-Smoker) ──
  WholeLife_NS = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(115 - a)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    round(apv * 5000000, 0)
  }),
  
  # ── Whole Life (Smoker) ──
  WholeLife_S = sapply(ages, function(a) {
    apv <- 0; kpx <- 1
    for (k in 0:(115 - a)) {
      qx  <- mortality$qx[mortality$age == (a + k)]
      apv <- apv + (v^(k+1)) * kpx * qx
      kpx <- kpx * (1 - qx)
    }
    round(apv * 5000000 * loading, 0)
  })
)

print(product_comparison)

# ── 20. COMBINED PRODUCT PLOT — NON-SMOKER ───────────────────
plot(product_comparison$Age, product_comparison$Term_NS,
     type = "b", col = "blue", lwd = 2,
     ylim = c(0, 1800000),
     xlab = "Age at Entry",
     ylab = "EPV Premium (Rs.)",
     main = "Insurance Product Comparison — Non-Smoker\nRs. 50 Lakh Cover, 20-Year Term, 6% Interest")

lines(product_comparison$Age, product_comparison$PureEndo_NS,
      type = "b", col = "darkgreen", lwd = 2)

lines(product_comparison$Age, product_comparison$Endo_NS,
      type = "b", col = "purple", lwd = 2)

lines(product_comparison$Age, product_comparison$WholeLife_NS,
      type = "b", col = "red", lwd = 2)

legend("topleft",
       legend = c("Term Assurance",
                  "Pure Endowment",
                  "Endowment Assurance",
                  "Whole Life"),
       col    = c("blue", "darkgreen", "purple", "red"),
       lwd    = 2)

# ── 21. COMBINED PRODUCT PLOT — SMOKER ───────────────────────
plot(product_comparison$Age, product_comparison$Term_S,
     type = "b", col = "blue", lwd = 2,
     ylim = c(0, 9500000),
     xlab = "Age at Entry",
     ylab = "EPV Premium (Rs.)",
     main = "Insurance Product Comparison — Smoker\nRs. 50 Lakh Cover, 20-Year Term, 6% Interest")

lines(product_comparison$Age, product_comparison$PureEndo_S,
      type = "b", col = "darkgreen", lwd = 2)

lines(product_comparison$Age, product_comparison$Endo_S,
      type = "b", col = "purple", lwd = 2)

lines(product_comparison$Age, product_comparison$WholeLife_S,
      type = "b", col = "red", lwd = 2)

legend("topleft",
       legend = c("Term Assurance",
                  "Pure Endowment",
                  "Endowment Assurance",
                  "Whole Life"),
       col    = c("blue", "darkgreen", "purple", "red"),
       lwd    = 2)
