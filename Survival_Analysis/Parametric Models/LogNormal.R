# Load necessary libraries
library(survival)
library(ggplot2)
library(dplyr)

mgus_data <- read.csv(...)

# Select relevant columns
mgus_data_clean <- mgus_data[, c("sex", "hgb", "creat", "ptime", "age", "futime", "death")]

# Convert 'sex' to numeric (F = 0, M = 1)
mgus_data_clean$sex <- ifelse(mgus_data_clean$sex == "F", 0, 1)

# Convert 'age' to a binary variable: 0 for age <= 70, 1 for age > 70
mgus_data_clean$age_binary <- ifelse(mgus_data_clean$age <= 70, 0, 1)


# Fit the Log-Normal survival model
lognormal_model <- survreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, 
                           data = mgus_data_clean, dist = "lognormal")

# Print model summary to verify the results
print(summary(lognormal_model))

# Extract the log-location (mean) parameter
meanlog <- lognormal_model$coefficients[1]  # Intercept

# Log-scale parameter (standard deviation in log-space)
sdlog <- lognormal_model$scale  # This should give you 0.862


# Print the results
cat("Log-location (mean):", meanlog, "\n")
cat("Log-scale (standard deviation):", sdlog, "\n")

# Extract AIC from the model
aic_lognormal <- -2 * lognormal_model$loglik[2] + 2 * length(lognormal_model$coefficients)
cat("AIC value:", aic_lognormal, "\n")

# Define Log-Normal survival and hazard functions
lognormal_surv <- function(t, meanlog, sdlog) {
  # Ensure log(t) works properly (i.e., for t > 0)
  ifelse(t > 0, 1 - pnorm(log(t), meanlog, sdlog), NA)
}

lognormal_hazard <- function(t, meanlog, sdlog) {
  # Ensure log(t) works properly and handle invalid time values
  ifelse(t > 0, dnorm(log(t), meanlog, sdlog) / (t * (1 - pnorm(log(t), meanlog, sdlog))), NA)
}

# Create a time vector for plotting (Ensure time starts from a positive value, avoid near zero values)
t <- seq(1, max(mgus_data_clean$futime), length.out = 500)

# Calculate survival and hazard values
survival_values_lognormal <- lognormal_surv(t, meanlog, sdlog)
hazard_values_lognormal <- lognormal_hazard(t, meanlog, sdlog)

# Print first few survival and hazard values to check if they are calculated properly
cat("First few survival values:", head(survival_values_lognormal), "\n")
cat("First few hazard values:", head(hazard_values_lognormal), "\n")

# Create a dataframe for plotting with ggplot2
plot_data_lognormal <- data.frame(
  time = t,
  survival = survival_values_lognormal,
  hazard = hazard_values_lognormal
)

# Plot Survival Function with ggplot2
ggplot(plot_data_lognormal, aes(x = time, y = survival)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Log-Normal Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Hazard Function with ggplot2
ggplot(plot_data_lognormal, aes(x = time, y = hazard)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Log-Normal Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# Define the log-location (mean) and log-scale (standard deviation)
log_location_mean <- 1.819423
log_scale <- 0.862

# Create custom labels for the survival and hazard functions
survival_label <- paste("Survival (Mean =", log_location_mean, ", Scale =", log_scale, ")")
hazard_label <- paste("Hazard (Mean =", log_location_mean, ", Scale =", log_scale, ")")

# Survival Function with custom legend
ggplot(plot_data_lognormal, aes(x = time, y = survival)) +
  geom_line(aes(color = survival_label), size = 1) +
  labs(title = "Log-Normal Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "blue") +
  labs(color = "Parameters")

# Hazard Function with custom legend
ggplot(plot_data_lognormal, aes(x = time, y = hazard)) +
  geom_line(aes(color = hazard_label), size = 1) +
  labs(title = "Log-Normal Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "red") +
  labs(color = "Parameters")

