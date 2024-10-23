# Load necessary libraries
library(survival)
library(ggplot2)
library(dplyr)

mgus_data <- read.csv("C:/Users/sowon/Desktop/thesis/mgus2.csv")

# Select relevant columns
mgus_data_clean <- mgus_data[, c("sex", "hgb", "creat", "ptime", "age", "futime", "death")]

# Convert 'sex' to numeric (F = 0, M = 1)
mgus_data_clean$sex <- ifelse(mgus_data_clean$sex == "F", 0, 1)

# Convert 'age' to a binary variable: 0 for age <= 70, 1 for age > 70
mgus_data_clean$age_binary <- ifelse(mgus_data_clean$age <= 70, 0, 1)

# Ensure all relevant columns are numeric and drop NA values
mgus_data_clean <- na.omit(mgus_data_clean)
# Fit the Weibull survival model
weibull_model <- survreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, 
                         data = mgus_data_clean, dist = "weibull")

# Extract scale and shape parameters
lambda <- exp(weibull_model$icoef["Log(scale)"])  # Scale parameter (lambda)
beta <- 1 / weibull_model$scale  # Shape parameter (beta)

# Extract AIC from the model
aic_value <- -2 * weibull_model$loglik[2] + 2 * length(weibull_model$coefficients)

# Print model results, parameters, and AIC
summary(weibull_model)

# Define Weibull survival and hazard functions
weibull_surv <- function(t, lambda, beta) {
  exp(-(t / lambda)^beta)
}

weibull_hazard <- function(t, lambda, beta) {
  (beta / lambda) * (t / lambda)^(beta - 1)
}

# Create a time vector for plotting
t <- seq(0, 5, length.out = 500)

# Calculate survival and hazard values
survival_values <- weibull_surv(t, lambda, beta)
hazard_values <- weibull_hazard(t, lambda, beta)

# Create a dataframe for plotting with ggplot2
plot_data <- data.frame(
  time = t,
  survival = survival_values,
  hazard = hazard_values
)

# Plot Survival Function with ggplot2, including parameter values and AIC in the legend
ggplot(plot_data, aes(x = time, y = survival)) +
  geom_line(aes(color = paste0("Survival: λ = ", round(lambda, 3), 
                               ", β = ", round(beta, 3), ", AIC = ", round(aic_value, 2))), size = 1) +
  labs(title = "Weibull Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "blue", name = "Parameters")

# Plot Hazard Function with ggplot2, including parameter values and AIC in the legend
ggplot(plot_data, aes(x = time, y = hazard)) +
  geom_line(aes(color = paste0("Hazard: λ = ", round(lambda, 3), 
                               ", β = ", round(beta, 3), ", AIC = ", round(aic_value, 2))), size = 1) +
  labs(title = "Weibull Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "red", name = "Parameters")
