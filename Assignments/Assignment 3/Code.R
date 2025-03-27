# Function to plot Beta distribution
plot_beta <- function(alpha, beta, title = "Beta Distribution", color = "blue") {
  # Create a sequence of x values for plotting
  x <- seq(0, 1, length.out = 200)
  
  # Calculate the density of the Beta distribution
  y <- dbeta(x, alpha, beta)
  
  # Check if this is the first plot.  If so, create a new plot.  Otherwise, add to the existing plot.
  if (par("new")) {
    plot(x, y,
         type = "l",
         xlab = "x",
         ylab = "Density",
         main = title,
         col = color,
         lwd = 2)
  } else {
    lines(x, y, col = color, lwd = 2)
  }
}

# Create a new plot and plot the first Beta distribution
plot_beta(alpha = 21, beta = 89, title = "Posterior Distributions for n = 100 and n = 1000", color = "blue")

# Add the second Beta distribution to the existing plot
plot_beta(alpha = 192, beta = 818, color = "red")

# Add a legend
legend("topright", 
       legend = c("n = 100", "n = 1000"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)
