# Load necessary libraries
library(ggplot2)

# Define the scale (alpha) and shape (lambda) parameters
alpha <- 9.648  # Scale parameter
lambda <- 2.390  # Shape parameter

# Define the log-logistic density function
loglogistic_density <- function(t, alpha, lambda) {
  (lambda * (t / alpha)^(lambda - 1)) / (alpha * (1 + (t / alpha)^lambda)^2)
}

# Define the log-logistic survival function
loglogistic_survival <- function(t, alpha, lambda) {
  1 / (1 + (t / alpha)^lambda)
}

# Define the log-logistic hazard function
loglogistic_hazard <- function(t, alpha, lambda) {
  (lambda * (t / alpha)^(lambda - 1)) / (1 + (t / alpha)^lambda)
}

# Create a time vector for plotting
t <- seq(0.1, 50, length.out = 500)

# Calculate values for density, survival, and hazard functions
density_values <- loglogistic_density(t, alpha, lambda)
survival_values <- loglogistic_survival(t, alpha, lambda)
hazard_values <- loglogistic_hazard(t, alpha, lambda)

# Create a dataframe for plotting
plot_data <- data.frame(
  time = t,
  density = density_values,
  survival = survival_values,
  hazard = hazard_values
)

# Plot the Log-Logistic Density Function with a legend
ggplot(plot_data, aes(x = time, y = density)) +
  geom_line(aes(color = paste0("Density: α = ", round(alpha, 3), ", λ = ", round(lambda, 3))), size = 1) +
  labs(title = "Log-Logistic Density Function (PDF)", x = "Time (t)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "green", name = "Parameters")

# Plot the Log-Logistic Survival Function with a legend
ggplot(plot_data, aes(x = time, y = survival)) +
  geom_line(aes(color = paste0("Survival: α = ", round(alpha, 3), ", λ = ", round(lambda, 3))), size = 1) +
  labs(title = "Log-Logistic Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "blue", name = "Parameters")

# Plot the Log-Logistic Hazard Function with a legend
ggplot(plot_data, aes(x = time, y = hazard)) +
  geom_line(aes(color = paste0("Hazard: α = ", round(alpha, 3), ", λ = ", round(lambda, 3))), size = 1) +
  labs(title = "Log-Logistic Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "red", name = "Parameters")
