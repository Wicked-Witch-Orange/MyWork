# Load necessary libraries
library(ggplot2)

# Define the shape (alpha) and scale (theta) parameters for the Gamma distribution
alpha <- 2.292221  # Shape parameter
theta <- 5.284005  # Scale parameter

# Define the Gamma density function (PDF)
gamma_density <- function(t, alpha, theta) {
  (t^(alpha - 1) * exp(-t / theta)) / (theta^alpha * gamma(alpha))
}

# Define the Gamma survival function
gamma_survival <- function(t, alpha, theta) {
  1 - pgamma(t, shape = alpha, scale = theta)
}

# Define the Gamma hazard function
gamma_hazard <- function(t, alpha, theta) {
  gamma_density(t, alpha, theta) / gamma_survival(t, alpha, theta)
}

# Create a time vector for plotting
t <- seq(0.1, 400, length.out = 500)

# Calculate values for density, survival, and hazard functions
density_values <- gamma_density(t, alpha, theta)
survival_values <- gamma_survival(t, alpha, theta)
hazard_values <- gamma_hazard(t, alpha, theta)

# Create a dataframe for plotting
plot_data <- data.frame(
  time = t,
  density = density_values,
  survival = survival_values,
  hazard = hazard_values
)

# Plot the Gamma Density Function with a legend
ggplot(plot_data, aes(x = time, y = density)) +
  geom_line(aes(color = paste0("Density: α = ", round(alpha, 3), ", θ = ", round(theta, 3))), size = 1) +
  labs(title = "Gamma Density Function (PDF)", x = "Time (t)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "green", name = "Parameters")

# Plot the Gamma Survival Function with a legend
ggplot(plot_data, aes(x = time, y = survival)) +
  geom_line(aes(color = paste0("Survival: α = ", round(alpha, 3), ", θ = ", round(theta, 3))), size = 1) +
  labs(title = "Gamma Survival Function", x = "Time (t)", y = "Survival Probability (S(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "blue", name = "Parameters")

# Plot the Gamma Hazard Function with a legend
ggplot(plot_data, aes(x = time, y = hazard)) +
  geom_line(aes(color = paste0("Hazard: α = ", round(alpha, 3), ", θ = ", round(theta, 3))), size = 1) +
  labs(title = "Gamma Hazard Function", x = "Time (t)", y = "Hazard Rate (h(t))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = "red", name = "Parameters")
