# Load necessary libraries
library(survival)
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

# Fit the Log-Logistic survival model
loglogistic_model <- survreg(Surv(futime, death) ~ sex + hgb + creat + ptime + age_binary, 
                             data = mgus_data_clean, dist = "loglogistic")

# Print model summary to verify the results
print(summary(loglogistic_model))

# Extract the log-location (mean) parameter
meanlog <- loglogistic_model$coefficients[1]  # Intercept

# Log-scale parameter (standard deviation in log-space)
sdlog <- loglogistic_model$scale

# Print the results
cat("Log-location (mean):", meanlog, "\n")
cat("Log-scale (standard deviation):", sdlog, "\n")

# Extract AIC from the model
aic_loglogistic <- -2 * loglogistic_model$loglik[2] + 2 * length(loglogistic_model$coefficients)
cat("AIC value:", aic_loglogistic, "\n")

# Define Log-Logistic survival and hazard functions
loglogistic_surv <- function(t, meanlog, sdlog) {
  # Survival function for log-logistic
  1 / (1 + (t / exp(meanlog))^exp(-sdlog))
}

loglogistic_hazard <- function(t, meanlog, sdlog) {
  alpha <- exp(meanlog)  # Scale parameter
  lambda <- exp(-sdlog)  # Shape parameter
  (lambda * (t / alpha)^(lambda - 1)) / (1 + (t / alpha)^lambda)
}

# Create a time vector for plotting (Ensure time starts from a positive value, avoid near-zero values)
t <- seq(1, max(mgus_data_clean$futime), length.out = 500)

# Calculate survival and hazard values
survival_values_loglogistic <- loglogistic_surv(t, meanlog, sdlog)
hazard_values_loglogistic <- loglogistic_hazard(t, meanlog, sdlog)

# Print first few survival and hazard values to check if they are calculated properly
cat("First few survival values:", head(survival_values_loglogistic), "\n")
cat("First few hazard values:", head(hazard_values_loglogistic), "\n")

# Create a dataframe for plotting with ggplot2
plot_data_loglogistic <- data.frame(
  time = t,
  survival = survival_values_loglogistic,
  hazard = hazard_values_loglogistic
)


# Plot Survival Function with ggplot2
ggplot(plot_data_loglogistic, aes(x = time, y = survival)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Log-Logistic Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Hazard Function with ggplot2
ggplot(plot_data_loglogistic, aes(x = time, y = hazard)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Log-Logistic Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Define the log-location (mean) and log-scale (standard deviation)
log_location_mean <- meanlog
log_scale <- sdlog

# Define the log-location (mean) and log-scale (standard deviation) with 4 decimal places
log_location_mean <- sprintf("%.4f", meanlog)
log_scale <- sprintf("%.4f", sdlog)

# Create custom labels for the survival and hazard functions
survival_label <- paste("Survival (Mean =", log_location_mean, ", Scale =", log_scale, ")")
hazard_label <- paste("Hazard (Mean =", log_location_mean, ", Scale =", log_scale, ")")

# Survival Function with custom legend
ggplot(plot_data_loglogistic, aes(x = time, y = survival)) +
  geom_line(aes(color = survival_label), size = 1) +
  labs(title = "Log-Logistic Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "blue") +
  labs(color = "Parameters")

# Hazard Function with custom legend
ggplot(plot_data_loglogistic, aes(x = time, y = hazard)) +
  geom_line(aes(color = hazard_label), size = 1) +
  labs(title = "Log-Logistic Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "red") +
  labs(color = "Parameters")


