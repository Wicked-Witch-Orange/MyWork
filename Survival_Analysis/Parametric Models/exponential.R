# Load necessary libraries
library(survival)    # For fitting survival models
library(ggplot2)     # For plotting
library(tidyr)       # For reshaping data

# Read dataset
mgus_data <- read.csv("C:/Users/sowon/Desktop/thesis/mgus2.csv")

# Select relevant columns
mgus_data_clean <- mgus_data[, c("sex", "hgb", "creat", "ptime", "age", "futime", "death")]

# Convert 'sex' to numeric (F = 0, M = 1)
mgus_data_clean$sex <- ifelse(mgus_data_clean$sex == "F", 0, 1)

# Convert 'age' to a binary variable: 0 for age <= 70, 1 for age > 70
mgus_data_clean$age_binary <- ifelse(mgus_data_clean$age <= 70, 0, 1)

# Remove rows with missing values
mgus_data_clean <- na.omit(mgus_data_clean)

# Fit the Exponential model (Weibull with shape parameter = 1)
exp_fit <- survreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, data = mgus_data_clean, dist = "exponential")
summary(exp_fit)


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
