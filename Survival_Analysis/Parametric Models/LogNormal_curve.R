# Load necessary libraries
library(ggplot2)

# Define the log-location (mean) and log-scale (standard deviation)
log_location <- 1.819423
log_scale <- 0.8616286 


# Create time points to calculate survival and hazard values
time_points <- seq(0.1, 200, by = 1)  # Avoid starting at 0 for log calculations

# Survival function for the log-normal distribution
survival_function <- function(t, mu, sigma) {
  return(1 - pnorm((log(t) - mu) / sigma))
}

# Hazard function for the log-normal distribution
hazard_function <- function(t, mu, sigma) {
  pdf_val <- dlnorm(t, meanlog = mu, sdlog = sigma)
  surv_val <- 1 - plnorm(t, meanlog = mu, sdlog = sigma)
  return(pdf_val / surv_val)
}

# Calculate survival probabilities and hazard rates
survival_values <- survival_function(time_points, log_location, log_scale)
hazard_values <- hazard_function(time_points, log_location, log_scale)

# Plot the survival curve
survival_df <- data.frame(Time = time_points, Survival = survival_values)
ggplot(survival_df, aes(x = Time, y = Survival)) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Log-Normal Survival Curve") +
  xlab("Time (Months)") +
  ylab("Survival Probability") +
  theme_minimal()

# Plot the hazard curve
hazard_df <- data.frame(Time = time_points, Hazard = hazard_values)
ggplot(hazard_df, aes(x = Time, y = Hazard)) +
  geom_line(color = "red", size = 1) +
  ggtitle("Log-Normal Hazard Curve") +
  xlab("Time (Months)") +
  ylab("Hazard Rate") +
  theme_minimal()
