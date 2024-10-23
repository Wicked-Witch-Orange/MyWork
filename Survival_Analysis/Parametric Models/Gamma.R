# Load necessary libraries
library(flexsurv)
library(ggplot2)
library(dplyr)

# Read the dataset
mgus_data <- read.csv("C:/Users/sowon/Desktop/thesis/mgus2.csv")

# Select relevant columns
mgus_data_clean <- mgus_data[, c("sex", "hgb", "creat", "ptime", "age", "futime", "death")]

# Convert 'sex' to numeric (F = 0, M = 1)
mgus_data_clean$sex <- ifelse(mgus_data_clean$sex == "F", 0, 1)

# Convert 'age' to a binary variable: 0 for age <= 70, 1 for age > 70
mgus_data_clean$age_binary <- ifelse(mgus_data_clean$age <= 70, 0, 1)

# Remove rows with missing values
mgus_data_clean <- na.omit(mgus_data_clean)


# Fit the Gamma survival model using flexsurv
gamma_model_flexsurv <- flexsurvreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, 
                                    data = mgus_data_clean, dist = "gamma")



# Fitting the Gamma survival model (as done earlier)
gamma_model_flexsurv <- flexsurvreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, 
                                    data = mgus_data_clean, dist = "gamma")

# Check the model summary to view available information
#summary(gamma_model_flexsurv)
# View the structure of the 'res' attribute
str(gamma_model_flexsurv$res)
# Check what names are available in the res list
names(gamma_model_flexsurv$res)


# Extract the shape and rate from the model
shape_gamma <- gamma_model_flexsurv$res["shape", "est"]  # Shape parameter (alpha)
rate_gamma <- gamma_model_flexsurv$res["rate", "est"]    # Rate parameter (lambda)

# The scale parameter is the inverse of the rate
scale_gamma <- 1 / rate_gamma

# Print the extracted parameters
cat("Shape parameter (alpha):", shape_gamma, "\n")
cat("Rate parameter (lambda):", rate_gamma, "\n")
cat("Scale parameter (theta):", scale_gamma, "\n")

# Print the coefficients (including for covariates)
print(gamma_model_flexsurv$coefficients)

# To derive the shape parameter, you would need more detailed distribution-specific information, 
# as flexsurv may not provide the Gamma shape directly for survival modeling. The shape is typically not a direct output of the survival model in `flexsurv`.


# Print the parameters
cat("Shape parameter (alpha):", shape_gamma, "\n")
cat("Scale parameter (theta):", scale_gamma, "\n")

# Rate parameter (lambda) is the inverse of the scale
rate_gamma <- 1 / scale_gamma
cat("Rate parameter (lambda):", rate_gamma, "\n")

# Print the summary of the Gamma model
summary(gamma_model_flexsurv)

# Extract AIC from the model
aic_gamma_flexsurv <- gamma_model_flexsurv$AIC
cat("AIC value:", aic_gamma_flexsurv, "\n")

# Define the survival and hazard functions based on the Gamma distribution
gamma_surv_flexsurv <- function(t, shape, rate) {
  pgamma(t, shape = shape, rate = rate, lower.tail = FALSE)
}

gamma_hazard_flexsurv <- function(t, shape, rate) {
  (dgamma(t, shape = shape, rate = rate)) / (pgamma(t, shape = shape, rate = rate, lower.tail = FALSE))
}

# Extract the shape and rate parameters from the fitted model
shape_gamma <- gamma_model_flexsurv$res[1, "est"]
rate_gamma <- 1 / gamma_model_flexsurv$res[2, "est"]

# Create a time vector for plotting
t <- seq(1, 400, length.out = 500)

# Calculate survival and hazard values for the Gamma model
survival_values_gamma_flexsurv <- gamma_surv_flexsurv(t, shape_gamma, rate_gamma)
hazard_values_gamma_flexsurv <- gamma_hazard_flexsurv(t, shape_gamma, rate_gamma)

