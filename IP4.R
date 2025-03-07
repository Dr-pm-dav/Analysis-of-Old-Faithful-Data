# Load necessary libraries
library(ggplot2)
library(stats)
library(MASS)

# Load the Old Faithful dataset
data("faithful")

# Preview the data
head(faithful)

# Water-like appearance for eruption times
eruption_hist <- ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "blue") +
  geom_density(color = "navy", linewidth = 1.2) +
  labs(title = "Eruption Times: Water-Themed Histogram",
       x = "Eruption Time (mins)", y = "Density") +
  theme_minimal()

print(eruption_hist)

# Explosion-like appearance for waiting times
waiting_hist <- ggplot(faithful, aes(x = waiting)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "orange", color = "red") +
  geom_density(color = "darkred", linewidth = 1.2) +
  labs(title = "Waiting Times: Explosion-Themed Histogram",
       x = "Waiting Time (mins)", y = "Density") +
  theme_minimal()

print(waiting_hist)

# Combined density histogram for eruption and waiting times
combined_density <- ggplot() +
  geom_density(data = faithful, aes(x = eruptions, y = ..density.., color = "Eruption Times"), size = 1.2) +
  geom_density(data = faithful, aes(x = waiting, y = ..density.., color = "Waiting Times"), size = 1.2) +
  scale_color_manual(values = c("Eruption Times" = "navy", "Waiting Times" = "darkred")) +
  labs(title = "Overlap of Density Curves: Eruption vs. Waiting Times",
       x = "Time (mins)", y = "Density", color = "Legend") +
  theme_minimal()

print(combined_density)

# Smoothed density for eruption times
eruption_fit <- density(faithful$eruptions, bw = "nrd0", kernel = "gaussian")
plot(eruption_fit, main = "Smoothed Density of Eruption Times (Local Polynomial)",
     xlab = "Eruption Time (mins)", ylab = "Density", col = "lightblue", lwd = 3)

# Smoothed density for waiting times
waiting_fit <- density(faithful$waiting, bw = "nrd0", kernel = "gaussian")
plot(waiting_fit, main = "Smoothed Density of Waiting Times (Local Polynomial)",
     xlab = "Waiting Time (mins)", ylab = "Density", col = "orange", lwd = 3)

# Smoothed density with local polynomial regression
eruption_fit <- density(faithful$eruptions, bw = "nrd0", kernel = "gaussian")
waiting_fit <- density(faithful$waiting, bw = "nrd0", kernel = "gaussian")

# Plot the combined smoothed densities
plot(eruption_fit, main = "Smoothed Density Overlap: Eruption vs. Waiting Times",
     xlab = "Time (mins)", ylab = "Density", col = "lightblue", lwd = 3, xlim = range(c(eruption_fit$x, waiting_fit$x)))
lines(waiting_fit, col = "orange", lwd = 3)
legend("topright", legend = c("Eruption Times", "Waiting Times"),
       col = c("lightblue", "orange"), lwd = 2)

