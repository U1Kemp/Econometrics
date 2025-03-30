# ------------------------------------------------------------------
# Code for plotting the two beta distributions in Question (3)
# ------------------------------------------------------------------

# Function to plot Beta distribution
plot_beta <- function(alpha, beta, title = "Beta Distribution", color = "blue", add = FALSE) {
  # Create a sequence of x values for plotting
  x <- seq(0, 1, length.out = 10000)
  
  # Calculate the density of the Beta distribution
  y <- dbeta(x, alpha, beta)
  
  # Plot the distribution
  if (add) {
    lines(x, y, col = color, lwd = 2) # Add to existing plot
  } else {
    plot(x, y,
         type = "l",  # Line plot
         xlab = "theta_1",
         ylab = "Density",
         main = title,
         col = color,
         lwd = 2) # Line width
  }
}

# plot for n = 1000.
plot_beta(alpha = 192, beta = 820, title = "Posterior Distributions", color = "red")

# Plot for n = 100, overlaying the previous plot.
plot_beta(alpha = 21, beta = 91, color = "blue", add = TRUE)

#add legend
legend("topright", legend = c("n=100", "n=1000"), col = c("blue", "red"), lty = 1)