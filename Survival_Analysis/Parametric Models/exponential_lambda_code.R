# Coefficients from the model
intercept <- 2.517830
coef_sex <- -0.114451
coef_hgb <- 0.045388
coef_creat <- -0.075849
coef_ptime <- 0.020399
coef_age_binary <- -0.232582

# Values for a specific individual
sex <- 1  # Male
hgb <- 14  # Hemoglobin level
creat <- 1.2  # Creatinine level
ptime <- 100  # Plasma time
age_binary <- 0  # Age ≤ 70

# Calculate the linear predictor
linear_predictor <- intercept + coef_sex * sex + coef_hgb * hgb +
  coef_creat * creat + coef_ptime * ptime + coef_age_binary * age_binary

# Calculate the hazard rate (lambda)
lambda <- exp(linear_predictor)

# Print the result
cat("The hazard rate (lambda) is:", lambda, "\n")
